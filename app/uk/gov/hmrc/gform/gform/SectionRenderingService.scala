/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.gform

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import cats.data.NonEmptyList
import cats.data.Validated.{ Invalid, Valid }
import cats.implicits._
import org.intellij.markdown.flavours.gfm.GFMFlavourDescriptor
import org.intellij.markdown.html.HtmlGenerator
import org.intellij.markdown.parser.MarkdownParser
import org.jsoup.Jsoup
import play.api.i18n.Messages
import play.api.mvc.Request
import play.twirl.api.Html
import uk.gov.hmrc.auth.core.AffinityGroup.Individual
import uk.gov.hmrc.auth.core.Enrolments
import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
import uk.gov.hmrc.gform.auth.models.{ MaterialisedRetrievals, UserDetails }
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.Origin
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.models.helpers.{ Fields, RepeatFormComponentIds }
import uk.gov.hmrc.gform.models.helpers.Javascript._
import uk.gov.hmrc.gform.models.{ DateExpr, Dependecies, FormComponentIdDeps, SectionRenderingInformation }
import uk.gov.hmrc.gform.ops.FormTemplateIdSyntax
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.DependencyGraph
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, _ }
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier

sealed trait HasErrors {

  def hasErrors: Boolean = this match {
    case Errors(_) => true
    case NoErrors  => false
  }

  def render: Html = this match {
    case Errors(html) => html
    case NoErrors     => Html("")
  }
}

case object NoErrors extends HasErrors
case class Errors(html: Html) extends HasErrors

case class FormRender(id: String, name: String, value: String)

class SectionRenderingService(
  prepopService: PrepopService,
  frontendAppConfig: FrontendAppConfig
) {

  case class ExtraInfo(
    formId: FormId,
    sectionNumber: SectionNumber,
    fieldData: Map[FormComponentId, Seq[String]],
    formTemplate: FormTemplate,
    envelope: Envelope,
    dynamicSections: List[BaseSection],
    formMaxAttachmentSizeMB: Int,
    section: BaseSection,
    retrievals: MaterialisedRetrievals
  )

  def renderSection(
    form: Form,
    sectionNumber: SectionNumber,
    fieldData: Map[FormComponentId, Seq[String]],
    formTemplate: FormTemplate,
    errors: List[(FormComponent, FormFieldValidationResult)],
    envelope: Envelope,
    envelopeId: EnvelopeId,
    validatedType: ValidatedType,
    dynamicSections: List[Section],
    formMaxAttachmentSizeMB: Int,
    contentTypes: List[ContentType],
    retrievals: MaterialisedRetrievals,
    lang: Option[String]
  )(implicit hc: HeaderCarrier, request: Request[_], messages: Messages): Future[Html] = {

    val section = dynamicSections(sectionNumber.value)

    val graph = DependencyGraph.toGraph(section)

    val graphTopologicalOrder: Either[graph.NodeT, graph.LayeredTopologicalOrder[graph.NodeT]] =
      DependencyGraph.constructDepencyGraph(graph)

    val dependencies: Dependecies = graphTopologicalOrder match {
      case Left(_) => Dependecies(List.empty[FormComponentIdDeps])
      case Right(lto) =>
        val depLayers: Traversable[List[FormComponentId]] = lto.map(_._2.map(_.toOuter).toList)
        val (deps, _) =
          depLayers
            .foldRight((List.empty[FormComponentIdDeps], List.empty[FormComponentId])) {
              case (layer, (deps, acc)) =>
                val newDeps = layer.map { fcId =>
                  FormComponentIdDeps(fcId, acc) // all of acc depends on fcId
                }
                (deps ++ newDeps, acc ++ layer)
            }
        Dependecies(deps)
    }
    val ei = ExtraInfo(
      form._id,
      sectionNumber,
      fieldData,
      formTemplate,
      envelope,
      dynamicSections,
      formMaxAttachmentSizeMB,
      section,
      retrievals)
    val actionForm = uk.gov.hmrc.gform.gform.routes.FormController.updateFormData(form._id, sectionNumber, lang)
    val listResult = errors.map { case (_, validationResult) => validationResult }

    val originSection = new Origin(formTemplate.sections, retrievals).minSectionNumber

    val sectionAtomicFields = RepeatingComponentService.atomicFields(section)
    val allAtomicFields = dynamicSections.flatMap(RepeatingComponentService.atomicFields)
    val javascript =
      createJavascript(dynamicSections.flatMap(_.fields), sectionAtomicFields, allAtomicFields, dependencies)
    val (hiddenTemplateFields, fieldDataUpd) = Fields.getHiddenTemplateFields(section, dynamicSections, fieldData)
    val hiddenSnippets = Fields
      .toFormField(fieldDataUpd, hiddenTemplateFields)
      .map(formField => html.form.snippets.hidden_field(formField))
    val pageLevelErrorHtml = generatePageLevelErrorHtml(listResult)

    for {
      snippetsForFields <- Future.traverse(section.fields)(
                            fieldValue =>
                              htmlFor(
                                fieldValue,
                                formTemplate._id.to4Ga,
                                0,
                                ei,
                                validatedType,
                                lang,
                                fieldValue.onlyShowOnSummary))

    } yield {
      val renderingInfo = SectionRenderingInformation(
        form._id,
        sectionNumber,
        section.title,
        section.description,
        hiddenSnippets,
        snippetsForFields,
        javascript,
        envelopeId,
        actionForm,
        true,
        section.continueLabel.getOrElse("Save and continue"),
        formMaxAttachmentSizeMB,
        contentTypes,
        section.progressIndicator
      )
      html.form.form(
        formTemplate,
        pageLevelErrorHtml,
        renderingInfo,
        form._id,
        shouldDisplayBack = sectionNumber > originSection,
        shouldDisplayBackToSummary = shouldDisplayBackToSummary(form),
        frontendAppConfig
      )
    }
  }

  def generatePageLevelErrorHtml(listValidation: List[FormFieldValidationResult]): HasErrors = {

    val allValidationResults = listValidation.flatMap {
      case componentField: ComponentField => parseFormFieldValidationResult(componentField)
      case others                         => List(others)
    }

    val errorsHtml: List[Html] = allValidationResults
      .filter(_.isNotOk)
      .flatMap { validationResult =>
        validationResult.fieldErrors
          .map(errorMessage => html.form.errors.error_message_component(validationResult, errorMessage))
      }

    if (errorsHtml.nonEmpty)
      Errors(html.form.errors.page_level_error(errorsHtml, listValidation))
    else
      NoErrors
  }

  def parseFormFieldValidationResult(componentField: ComponentField): List[FormFieldValidationResult] = {
    def reassignFieldValue(id: String, validationResult: FormFieldValidationResult): FormFieldValidationResult =
      validationResult match {
        case fieldError: FieldError =>
          val newFieldValue = fieldError.fieldValue.copy(id = FormComponentId(id))
          fieldError.copy(fieldValue = newFieldValue)
        case fieldGlobalError: FieldGlobalError =>
          val newFieldValue = fieldGlobalError.fieldValue.copy(id = FormComponentId(id))
          fieldGlobalError.copy(fieldValue = newFieldValue)
        case err => err
      }

    componentField.data
      .map(field => reassignFieldValue(field._1, field._2))
      .toList
      .sortWith(sortValidationList(componentField))
  }

  private def sortValidationList(
    component: ComponentField)(a: FormFieldValidationResult, b: FormFieldValidationResult): Boolean =
    component.fieldValue.`type` match {
      case _: Address => // currently only required for address as other components are in order
        val indexedFields = Address.fields(component.fieldValue.id).zipWithIndex.toMap
        indexedFields.getOrElse(a.fieldValue.id, -1) < indexedFields.getOrElse(b.fieldValue.id, -1)
      case _ => false // keep the order for other components
    }

  def renderDeclarationSection(
    form: Form,
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    validatedType: ValidatedType,
    fieldData: Map[FormComponentId, Seq[String]],
    errors: List[(FormComponent, FormFieldValidationResult)],
    lang: Option[String]
  )(implicit hc: HeaderCarrier, request: Request[_], messages: Messages): Future[Html] = {

    val ei = ExtraInfo(
      form._id,
      SectionNumber(0),
      fieldData,
      formTemplate,
      Envelope(Nil),
      List(formTemplate.declarationSection),
      0,
      formTemplate.declarationSection,
      retrievals)

    val confirm = formTemplate.formCategory match {
      case Some(HMRCReturnForm) => "Accept and submit return"
      case Some(HMRCClaimForm)  => "Accept and submit claim"
      case _                    => "Accept and submit"
    }

    val listResult = errors.map { case (_, validationResult) => validationResult }
    for {
      snippets <- Future.traverse(formTemplate.declarationSection.fields)(fieldValue =>
                   htmlFor(fieldValue, formTemplate._id.to4Ga, 0, ei, validatedType, lang))
      pageLevelErrorHtml = generatePageLevelErrorHtml(listResult)
      renderingInfo = SectionRenderingInformation(
        form._id,
        SectionNumber(0),
        formTemplate.declarationSection.title,
        formTemplate.declarationSection.description,
        Nil,
        snippets,
        "",
        EnvelopeId(""),
        uk.gov.hmrc.gform.gform.routes.DeclarationController.submitDeclaration(formTemplate._id.to4Ga, form._id, lang),
        false,
        confirm,
        0,
        Nil
      )
    } yield
      html.form.form(
        formTemplate,
        pageLevelErrorHtml,
        renderingInfo,
        form._id,
        shouldDisplayBack = false,
        shouldDisplayBackToSummary = false,
        frontendAppConfig)
  }

  def renderAcknowledgementSection(
    form: Form,
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    lang: Option[String],
    eventId: String)(implicit hc: HeaderCarrier, request: Request[_], messages: Messages): Future[Html] = {

    val ei = ExtraInfo(
      form._id,
      SectionNumber(0),
      Map.empty,
      formTemplate,
      Envelope(Nil),
      List(formTemplate.acknowledgementSection),
      0,
      formTemplate.declarationSection,
      retrievals
    )

    val formCategory = formTemplate.formCategory.getOrElse(Default)
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val now = LocalDateTime.now()
    val timeMessage = s""" at ${now.format(timeFormat)} on ${now.format(dateFormat)}"""
    for {
      snippets <- Future.traverse(formTemplate.acknowledgementSection.fields)(fieldValue =>
                   htmlFor(fieldValue, formTemplate._id.to4Ga, 0, ei, Valid(()), lang))
      renderingInfo = SectionRenderingInformation(
        form._id,
        SectionNumber(0),
        formTemplate.acknowledgementSection.title,
        formTemplate.acknowledgementSection.description,
        Nil,
        snippets,
        "",
        EnvelopeId(""),
        uk.gov.hmrc.gform.gform.routes.DeclarationController.submitDeclaration(formTemplate._id.to4Ga, form._id, lang),
        false,
        "Confirm and send",
        0,
        Nil
      )
    } yield
      uk.gov.hmrc.gform.views.html.hardcoded.pages.partials
        .acknowledgement(timeMessage, renderingInfo, formCategory, formTemplate, lang, eventId, frontendAppConfig)
  }

  def renderEnrolmentSection(
    formTemplate: FormTemplate,
    enrolmentSection: EnrolmentSection,
    fieldData: Map[FormComponentId, Seq[String]],
    errors: List[(FormComponent, FormFieldValidationResult)],
    validatedType: ValidatedType,
    lang: Option[String]
  )(implicit hc: HeaderCarrier, request: Request[_], messages: Messages): Future[Html] = {

    val formId = FormId("")
    val ei = ExtraInfo(
      formId,
      SectionNumber(0),
      fieldData,
      formTemplate,
      Envelope(Nil),
      List(enrolmentSection),
      0,
      enrolmentSection,
      emptyRetrievals)
    val listResult = errors.map { case (_, validationResult) => validationResult }
    for {
      snippets <- Future.traverse(enrolmentSection.fields)(fieldValue =>
                   htmlFor(fieldValue, formTemplate._id.to4Ga, 0, ei, validatedType, lang))
      pageLevelErrorHtml = generatePageLevelErrorHtml(listResult)
      renderingInfo = SectionRenderingInformation(
        formId,
        SectionNumber(0),
        enrolmentSection.title,
        None,
        Nil,
        snippets,
        "",
        EnvelopeId(""),
        uk.gov.hmrc.gform.gform.routes.EnrolmentController.submitEnrolment(formTemplate._id, lang),
        false,
        "Confirm and send",
        0,
        Nil
      )
    } yield html.form.form(formTemplate, pageLevelErrorHtml, renderingInfo, formId, false, false, frontendAppConfig)
  }

  private def createJavascript(
    fieldList: List[FormComponent],
    sectionAtomicFields: List[FormComponent],
    allAtomicFields: List[FormComponent],
    dependencies: Dependecies)(implicit hc: HeaderCarrier): String = {
    val groups: List[(FormComponentId, Group)] = fieldList
      .filter(_.presentationHint.getOrElse(Nil).contains(CollapseGroupUnderLabel))
      .collect {
        case fc @ IsGroup(group) => (fc.id, group)
      }

    val repeatFormComponentIds = RepeatingComponentService.getRepeatFormComponentIds(allAtomicFields)

    groups.map((collapsingGroupJavascript _).tupled).mkString("\n") +
      fieldJavascript(sectionAtomicFields, allAtomicFields, repeatFormComponentIds, dependencies)
  }

  private def htmlFor(
    fieldValue: FormComponent,
    formTemplateId4Ga: FormTemplateId4Ga,
    index: Int,
    ei: ExtraInfo,
    maybeValidated: ValidatedType,
    lang: Option[String],
    isHidden: Boolean = false)(implicit hc: HeaderCarrier, request: Request[_], messages: Messages): Future[Html] =
    fieldValue.`type` match {
      case sortCode @ UkSortCode(expr) =>
        htmlForSortCode(fieldValue, sortCode, expr, index, maybeValidated, ei, isHidden)
      case g @ Group(_, _, _, _, _, _) =>
        htmlForGroup(g, formTemplateId4Ga, fieldValue, index, ei, maybeValidated, lang)
      case Date(_, offset, dateValue) =>
        Future.successful(htmlForDate(fieldValue, offset, dateValue, index, maybeValidated, ei, isHidden))
      case Address(international) =>
        Future.successful(htmlForAddress(fieldValue, international, index, maybeValidated, ei))
      case t @ Text(_, _)     => renderText(implicitly)(t, fieldValue, index, maybeValidated, ei, isHidden)
      case t @ TextArea(_, _) => renderTextArea(implicitly)(t, fieldValue, index, maybeValidated, ei, isHidden)
      case Choice(choice, options, orientation, selections, optionalHelpText) =>
        Future.successful(
          htmlForChoice(
            fieldValue,
            choice,
            options,
            orientation,
            selections,
            optionalHelpText,
            index,
            maybeValidated,
            ei))
      case FileUpload() =>
        Future.successful(htmlForFileUpload(fieldValue, formTemplateId4Ga, index, ei, maybeValidated, lang))
      case InformationMessage(infoType, infoText) =>
        Future.successful(htmlForInformationMessage(fieldValue, infoType, infoText, index, ei))
    }

  private def htmlForInformationMessage(
    fieldValue: FormComponent,
    infoType: InfoType,
    infoText: String,
    index: Int,
    ei: ExtraInfo) = {
    val parsedMarkdownText = markDownParser(infoText)
    html.form.snippets.field_template_info(fieldValue, infoType, Html(parsedMarkdownText), index)
  }

  private def htmlForFileUpload(
    fieldValue: FormComponent,
    formTemplateId4Ga: FormTemplateId4Ga,
    index: Int,
    ei: ExtraInfo,
    validatedType: ValidatedType,
    lang: Option[String]) = {
    val validationResult = buildFormFieldValidationResult(fieldValue, ei, validatedType)

    html.form.snippets.field_template_file_upload(
      ei.formId,
      formTemplateId4Ga,
      ei.sectionNumber,
      sectionTitle4GaFactory(ei.formTemplate.sections(ei.sectionNumber.value).title),
      fieldValue,
      validationResult,
      index,
      ei.formMaxAttachmentSizeMB,
      lang
    )
  }

  private def markDownParser(markDownText: String): String =
    if (markDownText.nonEmpty) {
      val flavour = new GFMFlavourDescriptor
      val parsedTree = new MarkdownParser(flavour).buildMarkdownTreeFromString(markDownText)
      new HtmlGenerator(markDownText, parsedTree, flavour, false).generateHtml
    } else
      markDownText

  private def htmlForChoice(
    fieldValue: FormComponent,
    choice: ChoiceType,
    options: NonEmptyList[String],
    orientation: Orientation,
    selections: List[Int],
    optionalHelpText: Option[List[String]],
    index: Int,
    validatedType: ValidatedType,
    ei: ExtraInfo) = {

    def addTargetToLinks(html: String) = {
      val doc = Jsoup.parse(html)
      doc.getElementsByTag("a").attr("target", "_blank")
      doc.html
    }

    val prepopValues = ei.fieldData.get(fieldValue.id) match {
      case None    => selections.map(_.toString).toSet
      case Some(_) => Set.empty[String] // Don't prepop something we already submitted
    }

    val optionalHelpTextMarkDown: List[Html] = optionalHelpText
      .map(_.map(markDownParser))
      .map(_.map(x =>
        if (x.nonEmpty) {
          Html(addTargetToLinks(x))
        } else {
          Html("")
      }))
      .getOrElse(
        options.toList.map(_ => Html(""))
      )

    val validatedValue = buildFormFieldValidationResult(fieldValue, ei, validatedType)
    choice match {
      case Radio | YesNo =>
        html.form.snippets.choice(
          "radio",
          fieldValue,
          options,
          orientation,
          prepopValues,
          validatedValue,
          optionalHelpTextMarkDown,
          index,
          ei.section.title)
      case Checkbox =>
        html.form.snippets.choice(
          "checkbox",
          fieldValue,
          options,
          orientation,
          prepopValues,
          validatedValue,
          optionalHelpTextMarkDown,
          index,
          ei.section.title)
      case Inline =>
        html.form.snippets.choiceInline(
          fieldValue,
          options,
          prepopValues,
          validatedValue,
          optionalHelpTextMarkDown,
          index,
          ei.section.title)
    }
  }

  private type RenderTemplate[T] =
    (FormComponent, T, Option[String], Option[FormFieldValidationResult], Int, String) => Html

  private def renderTextArea(implicit hc: HeaderCarrier) =
    renderField[TextArea](
      html.form.snippets.field_template_textarea.apply _,
      html.form.snippets.field_template_textarea.apply _,
      _.constraint,
      _.value
    ) _

  private def renderText(implicit hc: HeaderCarrier) =
    renderField[Text](
      html.form.snippets.field_template_text_total.apply _,
      html.form.snippets.field_template_text.apply _,
      _.constraint,
      _.value
    ) _

  private def renderField[T](
    asTotalValue: RenderTemplate[T],
    asStandard: RenderTemplate[T],
    toTextConstraint: T => TextConstraint,
    toExpr: T => Expr
  )(
    t: T,
    fieldValue: FormComponent,
    index: Int,
    validatedType: ValidatedType,
    ei: ExtraInfo,
    isHidden: Boolean
  )(
    implicit hc: HeaderCarrier
  ) =
    for {
      prepopValue <- prepopF(toExpr(t), ei, fieldValue, toTextConstraint(t))
    } yield {
      val validatedValue = buildFormFieldValidationResult(fieldValue, ei, validatedType)
      if (isHidden)
        html.form.snippets
          .hidden_field_populated(List(FormRender(fieldValue.id.value, fieldValue.id.value, prepopValue.getOrElse(""))))
      else {
        fieldValue.presentationHint match {
          case Some(xs) if xs.contains(TotalValue) =>
            asTotalValue(fieldValue, t, prepopValue, validatedValue, index, ei.section.title)
          case _ =>
            asStandard(fieldValue, t, prepopValue, validatedValue, index, ei.section.title)
        }
      }
    }

  private def prepopF(expr: Expr, ei: ExtraInfo, fieldValue: FormComponent, constraint: TextConstraint)(
    implicit hc: HeaderCarrier): Future[Option[String]] = {

    def scale = constraint match {
      case Number(_, maxFractionalDigits, _)         => Some(maxFractionalDigits)
      case PositiveNumber(_, maxFractionalDigits, _) => Some(maxFractionalDigits)
      case Sterling                                  => Some(2)
      case _                                         => None
    }

    def hasNoData(fc: FormComponent): Boolean = ei.fieldData.get(fc.id).fold(true)(_ == Seq(""))

    val fieldsToRecalculate: List[FormComponentId] = ei.section match {
      case s: Section =>
        s.expandSection.toExpandedFormTemplate.allFCs
          .collect {
            case fc @ HasExpr(SingleExpr(_)) if !fc.editable                 => fc.id
            case fc @ HasExpr(SingleExpr(_)) if fc.editable && hasNoData(fc) => fc.id
          }
      case _ => List.empty[FormComponentId]
    }

    fieldsToRecalculate.find(fcId => fcId == fieldValue.id) match {
      case Some(_) =>
        prepopService.prepopData(expr, ei.formTemplate, ei.retrievals, ei.fieldData, ei.section, scale).map { res =>
          Some(res)
        }
      case None => Future.successful(None)
    }
  }

  private def htmlForSortCode(
    fieldValue: FormComponent,
    sC: UkSortCode,
    expr: Expr,
    index: Int,
    validatedType: ValidatedType,
    ei: ExtraInfo,
    isHidden: Boolean)(implicit hc: HeaderCarrier) = {
    val prepopValueF: Future[String] = ei.fieldData.get(fieldValue.id) match {
      case None =>
        prepopService.prepopData(expr, ei.formTemplate, ei.retrievals, ei.fieldData, ei.section)
      case _ => Future.successful("") // Don't prepop something we already submitted
    }

    for {
      prepopValue <- prepopValueF
    } yield {
      val validatedValue = buildFormFieldValidationResult(fieldValue, ei, validatedType)
      if (isHidden)
        html.form.snippets
          .hidden_field_populated(List(FormRender(fieldValue.id.value, fieldValue.id.value, prepopValue)))
      else html.form.snippets.field_template_sort_code(fieldValue, sC, prepopValue, validatedValue, index)
    }
  }

  private def htmlForAddress(
    fieldValue: FormComponent,
    international: Boolean,
    index: Int,
    validatedType: ValidatedType,
    ei: ExtraInfo)(implicit hc: HeaderCarrier) = {
    val fieldValues = buildFormFieldValidationResult(fieldValue, ei, validatedType)
    html.form.snippets.field_template_address(international, fieldValue, fieldValues, index, ei.section.title)
  }

  private def htmlForDate(
    fieldValue: FormComponent,
    offset: Offset,
    dateValue: Option[DateValue],
    index: Int,
    validatedType: ValidatedType,
    ei: ExtraInfo,
    isHidden: Boolean = false) = {
    val prepopValues: Option[DateExpr] = dateValue.map(DateExpr.fromDateValue).map(DateExpr.withOffset(offset, _))

    if (isHidden) {
      html.form.snippets.hidden_field_populated(
        List(
          FormRender(
            fieldValue.id.value + "-day",
            fieldValue.id.value + "-day",
            prepopValues.map(_.day.toString).getOrElse("")),
          FormRender(
            fieldValue.id.value + "-month",
            fieldValue.id.value + "-month",
            prepopValues.map(_.month.toString).getOrElse("")),
          FormRender(
            fieldValue.id.value + "-year",
            fieldValue.id.value + "-year",
            prepopValues.map(_.year.toString).getOrElse(""))
        )
      )
    } else {
      val fieldValues = buildFormFieldValidationResult(fieldValue, ei, validatedType)
      html.form.snippets.field_template_date(fieldValue, fieldValues, prepopValues, index)
    }
  }

  private def htmlForGroup(
    grp: Group,
    formTemplateId4Ga: FormTemplateId4Ga,
    fieldValue: FormComponent,
    index: Int,
    ei: ExtraInfo,
    validatedType: ValidatedType,
    lang: Option[String])(implicit hc: HeaderCarrier, request: Request[_], messages: Messages): Future[Html] = {
    val fgrpHtml = htmlForGroup0(grp, formTemplateId4Ga, fieldValue, index, ei, validatedType, lang)

    val isChecked = FormDataHelpers
      .dataEnteredInGroup(grp, ei.fieldData)

    fieldValue.presentationHint match {
      case Some(list) if list.contains(CollapseGroupUnderLabel) =>
        fgrpHtml.map(grpHtml => html.form.snippets.collapsable(fieldValue.id, fieldValue.label, grpHtml, isChecked))
      case _ => fgrpHtml
    }
  }

  private def htmlForGroup0(
    groupField: Group,
    formTemplateId4Ga: FormTemplateId4Ga,
    fieldValue: FormComponent,
    index: Int,
    ei: ExtraInfo,
    validatedType: ValidatedType,
    lang: Option[String])(implicit hc: HeaderCarrier, request: Request[_], messages: Messages) = {
    val maybeHint = fieldValue.helpText.map(markDownParser).map(Html.apply)
    for {
      (lhtml, limitReached) <- getGroupForRendering(
                                fieldValue,
                                formTemplateId4Ga,
                                groupField,
                                groupField.orientation,
                                validatedType,
                                ei,
                                lang)
    } yield
      html.form.snippets.group(fieldValue, maybeHint, groupField, lhtml, groupField.orientation, limitReached, index)
  }

  private def getRepeatingGroupsForRendering(
    formComponent: FormComponent,
    group: Group,
    fieldData: Map[FormComponentId, Seq[String]]): (List[GroupList], Boolean) = {

    val max = group.repeatsMax.getOrElse(1)

    val existingGroups = getAllFieldsInGroup(formComponent, group, fieldData) match {
      case Nil => List(group.baseGroupList)
      case xs  => xs
    }

    val groupsForRendering = fillToMin(existingGroups, group)

    (groupsForRendering, max == groupsForRendering.size)
  }

  private def getGroupForRendering(
    fieldValue: FormComponent,
    formTemplateId4Ga: FormTemplateId4Ga,
    groupField: Group,
    orientation: Orientation,
    validatedType: ValidatedType,
    ei: ExtraInfo,
    lang: Option[String])(
    implicit hc: HeaderCarrier,
    request: Request[_],
    messsages: Messages): Future[(List[Html], Boolean)] =
    if (groupField.repeatsMax.isDefined) {
      val (groupList, isLimit) =
        getRepeatingGroupsForRendering(fieldValue, groupField, ei.fieldData)
      groupList.zipWithIndex
        .map {
          case (gl, count) =>
            Future
              .traverse(gl.componentList)(fv => htmlFor(fv, formTemplateId4Ga, count + 1, ei, validatedType, lang))
              .map { lhtml =>
                val showButton = {
                  groupField.repeatsMax.getOrElse(0) == groupField.repeatsMin.getOrElse(0) ||
                  groupList.size <= groupField.repeatsMin.getOrElse(1)
                }
                html.form.snippets.group_element(fieldValue, groupField, lhtml, orientation, count + 1, showButton)
              }
        }
        .sequence
        .map(a => (a, isLimit))
    } else {
      Future
        .traverse(groupField.fields)(fv => htmlFor(fv, formTemplateId4Ga, 0, ei, validatedType, lang))
        .map(a => (a, true))
    }

  private def buildFormFieldValidationResult(
    fieldValue: FormComponent,
    ei: ExtraInfo,
    validatedType: ValidatedType): Option[FormFieldValidationResult] = {
    // TODO: Simplify building this result. When this method is called we already know what component we are dealing with
    // TODO: it is possible to get inner fields (if any) and build the result.
    val gformErrors: Map[FormComponentId, Set[String]] = validatedType match {
      case Invalid(errors) => errors
      case Valid(())       => Map.empty[FormComponentId, Set[String]]
    }
    val section: BaseSection = ei.dynamicSections(ei.sectionNumber.value)

    val fieldValues: List[FormComponent] = RepeatingComponentService.atomicFields(section)

    Fields.getValidationResult(ei.fieldData, fieldValues, ei.envelope, gformErrors)(fieldValue)
  }

  private def emptyRetrievals = MaterialisedRetrievals(
    authProviderId = OneTimeLogin,
    enrolments = Enrolments(Set.empty),
    affinityGroup = None,
    internalId = None,
    externalId = None,
    userDetails = UserDetails(
      authProviderId = None,
      authProviderType = None,
      name = "",
      affinityGroup = Individual,
      groupIdentifier = ""
    ),
    credentialStrength = None,
    agentCode = None
  )

  private def shouldDisplayBackToSummary(form: Form): Boolean = form.status == Summary
}
