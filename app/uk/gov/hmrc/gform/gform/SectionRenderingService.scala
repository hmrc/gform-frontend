/*
 * Copyright 2019 HM Revenue & Customs
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

import java.time.{ LocalDate, ZoneId }
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import cats.data.NonEmptyList
import cats.data.Validated.{ Invalid, Valid }
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.validated._
import org.intellij.markdown.flavours.gfm.GFMFlavourDescriptor
import org.intellij.markdown.html.HtmlGenerator
import org.intellij.markdown.parser.MarkdownParser
import org.jsoup.Jsoup
import play.api.i18n.Messages
import play.api.mvc.Request
import play.twirl.api.Html
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.auth.core.AffinityGroup.Individual
import uk.gov.hmrc.auth.core.Enrolments
import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
import uk.gov.hmrc.gform.auth.models.{ AuthenticatedRetrievals, MaterialisedRetrievals, UserDetails }
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.Origin
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.lookup.{ AjaxLookup, LookupRegistry, RadioLookup }
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.models.helpers.{ Fields, TaxPeriodHelper }
import uk.gov.hmrc.gform.models.helpers.Javascript._
import uk.gov.hmrc.gform.models.{ DateExpr, Dependecies, FormComponentIdDeps, SectionRenderingInformation }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Register
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.{ DependencyGraph, GraphNode, SimpleGN }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation._
import uk.gov.hmrc.gform.views.html

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
case class OptionParams(value: String, fromDate: LocalDate, toDate: LocalDate, selected: Boolean)
class SectionRenderingService(frontendAppConfig: FrontendAppConfig, lookupRegistry: LookupRegistry)(
  implicit ec: ExecutionContext
) {

  case class ExtraInfo(
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    fieldData: FormDataRecalculated,
    formTemplate: FormTemplate,
    envelope: Envelope,
    dynamicSections: List[BaseSection],
    formMaxAttachmentSizeMB: Int,
    section: BaseSection,
    retrievals: MaterialisedRetrievals,
    formLevelHeading: Boolean
  )

  def renderSection(
    maybeAccessCode: Option[AccessCode],
    form: Form,
    sectionNumber: SectionNumber,
    fieldData: FormDataRecalculated,
    formTemplate: FormTemplate,
    errors: List[(FormComponent, FormFieldValidationResult)],
    envelope: Envelope,
    envelopeId: EnvelopeId,
    validatedType: ValidatedType[ValidationResult],
    dynamicSections: List[Section],
    formMaxAttachmentSizeMB: Int,
    contentTypes: List[ContentType],
    retrievals: MaterialisedRetrievals,
    visitsIndex: VisitIndex,
    obligations: Obligations
  )(implicit request: Request[_], messages: Messages, l: LangADT): Html = {

    val section = dynamicSections(sectionNumber.value)
    val formLevelHeading = shouldDisplayHeading(section, formTemplate.GFC579Ready.getOrElse("false"))

    val graph = DependencyGraph.toGraphFull(formTemplate)

    val graphTopologicalOrder: Either[graph.NodeT, Traversable[(Int, List[GraphNode])]] =
      DependencyGraph.constructDependencyGraph(graph)

    val dependencies: Dependecies = graphTopologicalOrder match {
      case Left(_) => Dependecies(List.empty[FormComponentIdDeps])
      case Right(lto) =>
        val depLayers: Traversable[List[FormComponentId]] =
          lto.map(_._2).map(_.collect { case SimpleGN(fcId) => fcId })
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
      maybeAccessCode,
      sectionNumber,
      fieldData,
      formTemplate,
      envelope,
      dynamicSections,
      formMaxAttachmentSizeMB,
      section,
      retrievals,
      formLevelHeading)
    val actionForm = uk.gov.hmrc.gform.gform.routes.FormController
      .updateFormData(formTemplate._id, maybeAccessCode, sectionNumber)
    val listResult = errors.map { case (_, validationResult) => validationResult }

    val sectionAtomicFields = RepeatingComponentService.atomicFieldsFullWithCtx(section)
    val allAtomicFields = dynamicSections.flatMap(RepeatingComponentService.atomicFieldsFull)
    val javascript =
      createJavascript(dynamicSections.flatMap(_.fields), sectionAtomicFields, allAtomicFields, dependencies)
    val (hiddenTemplateFields, fieldDataUpd) =
      Fields.getHiddenTemplateFields(section, dynamicSections, fieldData, lookupRegistry.extractors)
    val hiddenSnippets = Fields
      .toFormField(fieldDataUpd, hiddenTemplateFields)
      .map(formField => html.form.snippets.hidden_field(formField))

    val indices = html.form.snippets.hidden_field(visitsIndex.toFormField)

    val pageLevelErrorHtml = generatePageLevelErrorHtml(listResult, List.empty)

    val originSection = Origin(formTemplate.sections, fieldData).minSectionNumber
    val snippetsForFields = section.fields.map(
      formComponent =>
        htmlFor(
          formComponent,
          formTemplate._id,
          0,
          ei,
          fieldData,
          validatedType,
          formComponent.onlyShowOnSummary,
          obligations))
    val renderingInfo = SectionRenderingInformation(
      formTemplate._id,
      maybeAccessCode,
      sectionNumber,
      section.title.value,
      section.description.map(ls => ls.value),
      indices :: hiddenSnippets,
      snippetsForFields,
      javascript,
      envelopeId,
      actionForm,
      retrievals.renderSaveAndComeBackLater,
      section.continueLabel.map(ls => ls.value).getOrElse(messages(retrievals.continueLabelKey)),
      formMaxAttachmentSizeMB,
      contentTypes,
      section.progressIndicator.map(ls => ls.value)
    )
    html.form.form(
      formTemplate,
      pageLevelErrorHtml,
      renderingInfo,
      shouldDisplayBack = sectionNumber > originSection,
      shouldDisplayHeading = formLevelHeading,
      shouldDisplayContinue = !section.continueIf.contains(Stop),
      frontendAppConfig,
      isDeclaration = false
    )

  }

  private def generatePageLevelErrorHtml(
    listValidation: List[FormFieldValidationResult],
    globalErrors: List[Html]
  )(implicit messages: Messages): HasErrors = {

    val allValidationResults = listValidation.flatMap {
      case componentField: ComponentField => parseFormFieldValidationResult(componentField)
      case others                         => List(others)
    }

    val errorsHtml: List[Html] = globalErrors ++ allValidationResults
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
        val indexedFields = Address.fields(component.fieldValue.id).toList.zipWithIndex.toMap
        indexedFields.getOrElse(a.fieldValue.id, -1) < indexedFields.getOrElse(b.fieldValue.id, -1)
      case _ => false // keep the order for other components
    }

  def renderDeclarationSection(
    maybeAccessCode: Option[AccessCode],
    form: Form,
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    validatedType: ValidatedType[ValidationResult],
    fieldData: FormDataRecalculated,
    errors: List[(FormComponent, FormFieldValidationResult)]
  )(implicit hc: HeaderCarrier, request: Request[_], messages: Messages, l: LangADT): Html = {

    val ei = ExtraInfo(
      maybeAccessCode,
      SectionNumber(0),
      fieldData,
      formTemplate,
      Envelope(Nil),
      List(formTemplate.declarationSection),
      0,
      formTemplate.declarationSection,
      retrievals,
      formLevelHeading = true
    )

    val confirm = formTemplate.formCategory match {
      case HMRCReturnForm => messages("button.acceptAndSubmitForm", messages("formCategory.return"))
      case HMRCClaimForm  => messages("button.acceptAndSubmitForm", messages("formCategory.claim"))
      case _              => messages("button.acceptAndSubmit")
    }

    val listResult = errors.map { case (_, validationResult) => validationResult }

    val snippets = formTemplate.declarationSection.fields.map(formComponent =>
      htmlFor(formComponent, formTemplate._id, 0, ei, fieldData, validatedType, obligations = NotChecked))
    val pageLevelErrorHtml = generatePageLevelErrorHtml(listResult, List.empty)
    val renderingInfo = SectionRenderingInformation(
      formTemplate._id,
      maybeAccessCode,
      SectionNumber(0),
      formTemplate.declarationSection.title.value,
      formTemplate.declarationSection.description.map(ls => ls.value),
      Nil,
      snippets,
      "",
      EnvelopeId(""),
      uk.gov.hmrc.gform.gform.routes.DeclarationController
        .submitDeclaration(formTemplate._id, maybeAccessCode),
      false,
      confirm,
      0,
      Nil
    )
    html.form.form(
      formTemplate,
      pageLevelErrorHtml,
      renderingInfo,
      shouldDisplayBack = true,
      shouldDisplayHeading = true,
      shouldDisplayContinue = true,
      frontendAppConfig,
      isDeclaration = true
    )
  }

  def renderAcknowledgementSection(
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    eventId: String,
    envelopeId: EnvelopeId)(
    implicit hc: HeaderCarrier,
    request: Request[_],
    messages: Messages,
    l: LangADT): Future[Html] = {

    val ei = ExtraInfo(
      maybeAccessCode,
      SectionNumber(0),
      FormDataRecalculated.empty,
      formTemplate,
      Envelope(Nil),
      List(formTemplate.acknowledgementSection),
      0,
      formTemplate.declarationSection,
      retrievals,
      formLevelHeading = false
    )

    val formCategory = formTemplate.formCategory
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val now = ZonedDateTime.now(ZoneId.of("Europe/London"))
    val timeMessage = s""" at ${now.format(timeFormat)} on ${now.format(dateFormat)}"""
    for {
      snippets <- Future.traverse(formTemplate.acknowledgementSection.fields)(
                   formComponent =>
                     Future.successful(
                       htmlFor(
                         formComponent,
                         formTemplate._id,
                         0,
                         ei,
                         FormDataRecalculated.empty,
                         ValidationResult.empty.valid,
                         obligations = NotChecked)))
      renderingInfo = SectionRenderingInformation(
        formTemplate._id,
        maybeAccessCode,
        SectionNumber(0),
        formTemplate.acknowledgementSection.title.value,
        formTemplate.acknowledgementSection.description.map(ls => ls.value),
        Nil,
        snippets,
        "",
        envelopeId,
        uk.gov.hmrc.gform.gform.routes.DeclarationController
          .submitDeclaration(formTemplate._id, maybeAccessCode),
        false,
        "Confirm and send",
        0,
        Nil
      )
    } yield
      uk.gov.hmrc.gform.views.html.hardcoded.pages.partials
        .acknowledgement(timeMessage, renderingInfo, formCategory, formTemplate, eventId, frontendAppConfig)
  }

  def renderEnrolmentSection(
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    enrolmentSection: EnrolmentSection,
    fieldData: FormDataRecalculated,
    errors: List[(FormComponent, FormFieldValidationResult)],
    globalErrors: List[Html],
    validatedType: ValidatedType[ValidationResult]
  )(implicit hc: HeaderCarrier, request: Request[_], messages: Messages, l: LangADT): Html = {

    val maybeAccessCode = None
    // This is only used for a file upload component, which should not appear in an enrollment section
    val ei = ExtraInfo(
      maybeAccessCode,
      SectionNumber(0),
      fieldData,
      formTemplate,
      Envelope(Nil),
      List(enrolmentSection),
      0,
      enrolmentSection,
      emptyRetrievals,
      formLevelHeading = true)
    val listResult = errors.map { case (_, validationResult) => validationResult }
    val snippets =
      enrolmentSection.fields.map(formComponent =>
        htmlFor(formComponent, formTemplate._id, 0, ei, fieldData, validatedType, obligations = NotChecked))
    val pageLevelErrorHtml = generatePageLevelErrorHtml(listResult, globalErrors)
    val renderingInfo = SectionRenderingInformation(
      formTemplate._id,
      maybeAccessCode,
      SectionNumber(0),
      enrolmentSection.title.value,
      None,
      Nil,
      snippets,
      "",
      EnvelopeId(""),
      uk.gov.hmrc.gform.gform.routes.EnrolmentController.submitEnrolment(formTemplate._id),
      false,
      "Confirm and send",
      0,
      Nil
    )
    html.form.form(formTemplate, pageLevelErrorHtml, renderingInfo, false, true, true, frontendAppConfig, false)
  }

  private def createJavascript(
    fieldList: List[FormComponent],
    sectionAtomicFields: List[FormComponentWithCtx],
    allAtomicFields: List[FormComponent],
    dependencies: Dependecies): String = {
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
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    index: Int,
    ei: ExtraInfo,
    data: FormDataRecalculated,
    maybeValidated: ValidatedType[ValidationResult],
    isHidden: Boolean = false,
    obligations: Obligations)(implicit request: Request[_], messages: Messages, l: LangADT): Html =
    formComponent.`type` match {
      case sortCode @ UkSortCode(expr) =>
        htmlForSortCode(formComponent, sortCode, expr, formComponent.id, index, maybeValidated, ei, data, isHidden)
      case g @ Group(_, _, _, _, _, _) =>
        htmlForGroup(g, formTemplateId, formComponent, index, ei, data, maybeValidated, obligations)
      case Date(_, offset, dateValue) =>
        htmlForDate(formComponent, offset, dateValue, index, maybeValidated, ei, data, isHidden)
      case Address(international) => htmlForAddress(formComponent, international, index, maybeValidated, ei, data)
      case Text(Lookup(register), _, _, _) =>
        renderLookup(formComponent, register, index, maybeValidated, ei, data, isHidden)
      case t @ Text(_, _, _, _) => renderText(messages, l)(t, formComponent, index, maybeValidated, ei, data, isHidden)
      case t @ TextArea(_, _, _) =>
        renderTextArea(messages, l)(t, formComponent, index, maybeValidated, ei, data, isHidden)
      case Choice(choice, options, orientation, selections, optionalHelpText) =>
        htmlForChoice(
          formComponent,
          choice,
          options,
          orientation,
          selections,
          optionalHelpText,
          index,
          maybeValidated,
          ei,
          data)
      case RevealingChoice(options) =>
        htmlForRevealingChoice(formComponent, formTemplateId, options, index, maybeValidated, ei, data, obligations)
      case FileUpload() =>
        htmlForFileUpload(formComponent, formTemplateId, index, ei, data, ei.retrievals, maybeValidated)
      case InformationMessage(infoType, infoText) =>
        htmlForInformationMessage(formComponent, infoType, infoText, index, ei)
      case htp @ HmrcTaxPeriod(idType, idNumber, regimeType) =>
        htmlForHmrcTaxPeriod(formComponent, index, ei, maybeValidated, data, obligations, htp)
    }

  private def htmlForHmrcTaxPeriod(
    formComponent: FormComponent,
    index: Int,
    ei: ExtraInfo,
    validatedType: ValidatedType[ValidationResult],
    data: FormDataRecalculated,
    obligations: Obligations,
    hmrcTP: HmrcTaxPeriod)(implicit messages: Messages, l: LangADT) = {

    val taxPeriodOptions: List[OptionParams] = obligations match {
      case RetrievedObligations(listOfObligations) =>
        listOfObligations
          .find(_.id.recalculatedTaxPeriodKey.hmrcTaxPeriod === hmrcTP)
          .map(_.obligation.obligations.flatMap(_.obligationDetails.map(od =>
            OptionParams(od.periodKey, od.inboundCorrespondenceFromDate, od.inboundCorrespondenceToDate, false))))
          .getOrElse(List.empty[OptionParams])
      case _ => List.empty[OptionParams]
    }

    val validatedValue = buildFormFieldValidationResult(formComponent, ei, validatedType, data)

    val setValue = TaxPeriodHelper.formatTaxPeriodOutput(validatedValue)

    html.form.snippets
      .hmrc_tax_period("radio", formComponent, taxPeriodOptions, Set[String](), validatedValue, index, true, setValue)
  }

  private def htmlForInformationMessage(
    formComponent: FormComponent,
    infoType: InfoType,
    infoText: LocalisedString,
    index: Int,
    ei: ExtraInfo)(implicit messages: Messages, l: LangADT) = {
    val parsedMarkdownText = markDownParser(infoText.value)
    val parsedContent = htmlBodyContents(parsedMarkdownText)
    html.form.snippets.field_template_info(formComponent, infoType, Html(parsedContent), index)
  }

  private def htmlForFileUpload(
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    index: Int,
    ei: ExtraInfo,
    data: FormDataRecalculated,
    materialisedRetrievals: MaterialisedRetrievals,
    validatedType: ValidatedType[ValidationResult])(implicit messages: Messages, l: LangADT) = {
    val validationResult = buildFormFieldValidationResult(formComponent, ei, validatedType, data)

    html.form.snippets.field_template_file_upload(
      FormId(materialisedRetrievals, formTemplateId, ei.maybeAccessCode),
      ei.maybeAccessCode,
      formTemplateId,
      ei.sectionNumber,
      formComponent,
      validationResult,
      index,
      ei.formMaxAttachmentSizeMB
    )
  }

  private def htmlBodyContents(html: String): String = {
    val doc = Jsoup.parse(html)
    doc.body().html()
  }

  private def markDownParser(markDownText: String): String =
    if (markDownText.nonEmpty) {
      val flavour = new GFMFlavourDescriptor
      val parsedTree = new MarkdownParser(flavour).buildMarkdownTreeFromString(markDownText)
      new HtmlGenerator(markDownText, parsedTree, flavour, false).generateHtml
    } else
      markDownText

  private def htmlForChoice(
    formComponent: FormComponent,
    choice: ChoiceType,
    options: NonEmptyList[LocalisedString],
    orientation: Orientation,
    selections: List[Int],
    optionalHelpText: Option[List[LocalisedString]],
    index: Int,
    validatedType: ValidatedType[ValidationResult],
    ei: ExtraInfo,
    data: FormDataRecalculated)(implicit messages: Messages, l: LangADT) = {

    def addTargetToLinks(html: String) = {
      val doc = Jsoup.parse(html)
      doc.getElementsByTag("a").attr("target", "_blank")
      doc.body().html()
    }

    val prepopValues = ei.fieldData.data.get(formComponent.id) match {
      case None    => selections.map(_.toString).toSet
      case Some(_) => Set.empty[String] // Don't prepop something we already submitted
    }

    val optionalHelpTextMarkDown: List[Html] = optionalHelpText
      .map(_.map(ls => markDownParser(ls.value)))
      .map(_.map(x =>
        if (x.nonEmpty) {
          Html(addTargetToLinks(x))
        } else {
          Html("")
      }))
      .getOrElse(
        options.toList.map(_ => Html(""))
      )

    val validatedValue = buildFormFieldValidationResult(formComponent, ei, validatedType, data)
    choice match {
      case Radio | YesNo =>
        html.form.snippets.choice(
          "radio",
          formComponent,
          options,
          orientation,
          prepopValues,
          validatedValue,
          optionalHelpTextMarkDown,
          index,
          ei.section.title.value,
          ei.formLevelHeading
        )
      case Checkbox =>
        html.form.snippets.choice(
          "checkbox",
          formComponent,
          options,
          orientation,
          prepopValues,
          validatedValue,
          optionalHelpTextMarkDown,
          index,
          ei.section.title.value,
          ei.formLevelHeading
        )
      case Inline =>
        html.form.snippets.choiceInline(
          formComponent,
          options.map(ls => ls.value),
          prepopValues,
          validatedValue,
          optionalHelpTextMarkDown,
          index,
          ei.section.title.value)
    }
  }

  private def htmlForRevealingChoice(
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    options: NonEmptyList[RevealingChoiceElement],
    index: Int,
    validatedType: ValidatedType[ValidationResult],
    ei: ExtraInfo,
    data: FormDataRecalculated,
    obligations: Obligations)(implicit request: Request[_], message: Messages, l: LangADT) = {
    val validatedValue = buildFormFieldValidationResult(fieldValue, ei, validatedType, data)
    val nestedEi = ei.copy(formLevelHeading = true)
    val revealingChoicesList =
      options.map { o =>
        val isSelected: Int => Boolean =
          index => ei.fieldData.data.get(fieldValue.id).flatMap(_.headOption).fold(o.selected)(_ === index.toString)
        (
          o.choice,
          isSelected,
          o.revealingFields.map(
            htmlFor(_, formTemplateId, index, nestedEi, data, validatedType, obligations = obligations)))
      }

    html.form.snippets
      .revealingChoice(fieldValue, revealingChoicesList, validatedValue, index, ei.section.title, ei.formLevelHeading)
  }
  case class RevealingChoiceComponents(option: String, hiddenField: List[FormComponent])

  private def renderLookup(
    fieldValue: FormComponent,
    register: Register,
    index: Int,
    validatedType: ValidatedType[ValidationResult],
    ei: ExtraInfo,
    data: FormDataRecalculated,
    isHidden: Boolean
  )(implicit l: LangADT): Html = {

    val prepopValue = ei.fieldData.data.get(fieldValue.id).flatMap(_.headOption)
    val validatedValue = buildFormFieldValidationResult(fieldValue, ei, validatedType, data)
    if (isHidden)
      html.form.snippets
        .hidden_field_populated(List(FormRender(fieldValue.id.value, fieldValue.id.value, prepopValue.getOrElse(""))))
    else
      lookupRegistry.get(register) match {
        case None => Html("") // Ups
        case Some(AjaxLookup(_, _, showAll)) =>
          html.form.snippets.lookup_autosuggest(
            fieldValue,
            showAll,
            register,
            ei.formTemplate._id,
            ei.formLevelHeading,
            prepopValue,
            validatedValue,
            index
          )
        case Some(RadioLookup(options)) =>
          html.form.snippets.lookup_radios(
            fieldValue,
            options.keys.toList.sortBy(_.label),
            ei.formLevelHeading,
            prepopValue,
            validatedValue,
            index
          )
      }
  }

  private type RenderTemplate[T] =
    (FormComponent, T, Option[String], Option[FormFieldValidationResult], Int, String, Boolean) => Html

  private def renderTextArea(implicit messages: Messages, l: LangADT) =
    renderField[TextArea](
      html.form.snippets.field_template_textarea.apply _,
      html.form.snippets.field_template_textarea.apply _
    ) _

  private def renderText(implicit messages: Messages, l: LangADT) =
    renderField[Text](
      html.form.snippets.field_template_text_total.apply _,
      html.form.snippets.field_template_text.apply _
    ) _

  private def renderField[T](
    asTotalValue: RenderTemplate[T],
    asStandard: RenderTemplate[T]
  )(
    t: T,
    formComponent: FormComponent,
    index: Int,
    validatedType: ValidatedType[ValidationResult],
    ei: ExtraInfo,
    data: FormDataRecalculated,
    isHidden: Boolean
  )(implicit messages: Messages, l: LangADT) = {
    val prepopValue = ei.fieldData.data.get(formComponent.id).flatMap(_.headOption)
    val validatedValue = buildFormFieldValidationResult(formComponent, ei, validatedType, data)
    if (isHidden)
      html.form.snippets
        .hidden_field_populated(
          List(FormRender(formComponent.id.value, formComponent.id.value, prepopValue.getOrElse(""))))
    else {
      formComponent.presentationHint match {
        case Some(xs) if xs.contains(TotalValue) =>
          asTotalValue(
            formComponent,
            t,
            prepopValue,
            validatedValue,
            index,
            ei.section.title.value,
            ei.formLevelHeading)
        case _ =>
          asStandard(formComponent, t, prepopValue, validatedValue, index, ei.section.title.value, ei.formLevelHeading)
      }
    }
  }

  private def htmlForSortCode(
    formComponent: FormComponent,
    sC: UkSortCode,
    expr: Expr,
    fcId: FormComponentId,
    index: Int,
    validatedType: ValidatedType[ValidationResult],
    ei: ExtraInfo,
    data: FormDataRecalculated,
    isHidden: Boolean)(implicit messages: Messages, l: LangADT) = {
    val prepopValue = ei.fieldData.data.get(formComponent.id).flatMap(_.headOption).getOrElse("")
    val validatedValue = buildFormFieldValidationResult(formComponent, ei, validatedType, data)
    if (isHidden)
      html.form.snippets
        .hidden_field_populated(List(FormRender(formComponent.id.value, formComponent.id.value, prepopValue)))
    else
      html.form.snippets
        .field_template_sort_code(formComponent, sC, prepopValue, validatedValue, index, ei.formLevelHeading)

  }

  private def htmlForAddress(
    formComponent: FormComponent,
    international: Boolean,
    index: Int,
    validatedType: ValidatedType[ValidationResult],
    ei: ExtraInfo,
    data: FormDataRecalculated)(implicit messages: Messages, l: LangADT) = {
    val fieldValues = buildFormFieldValidationResult(formComponent, ei, validatedType, data)
    html.form.snippets
      .field_template_address(
        international,
        formComponent,
        fieldValues,
        index,
        ei.section.title.value,
        ei.formLevelHeading)
  }

  private def htmlForDate(
    formComponent: FormComponent,
    offset: Offset,
    dateValue: Option[DateValue],
    index: Int,
    validatedType: ValidatedType[ValidationResult],
    ei: ExtraInfo,
    data: FormDataRecalculated,
    isHidden: Boolean = false)(implicit messages: Messages, l: LangADT) = {
    val prepopValues: Option[DateExpr] = dateValue.map(DateExpr.fromDateValue).map(DateExpr.withOffset(offset, _))

    if (isHidden) {
      html.form.snippets.hidden_field_populated(
        List(
          FormRender(
            formComponent.id.value + "-day",
            formComponent.id.value + "-day",
            prepopValues.map(_.day.toString).getOrElse("")),
          FormRender(
            formComponent.id.value + "-month",
            formComponent.id.value + "-month",
            prepopValues.map(_.month.toString).getOrElse("")),
          FormRender(
            formComponent.id.value + "-year",
            formComponent.id.value + "-year",
            prepopValues.map(_.year.toString).getOrElse(""))
        )
      )
    } else {
      val fieldValues = buildFormFieldValidationResult(formComponent, ei, validatedType, data)
      html.form.snippets.field_template_date(formComponent, fieldValues, prepopValues, index, ei.formLevelHeading)
    }
  }

  private def htmlForGroup(
    grp: Group,
    formTemplateId: FormTemplateId,
    formComponent: FormComponent,
    index: Int,
    ei: ExtraInfo,
    data: FormDataRecalculated,
    validatedType: ValidatedType[ValidationResult],
    obligations: Obligations)(implicit request: Request[_], messages: Messages, l: LangADT): Html = {
    val grpHtml = htmlForGroup0(grp, formTemplateId, formComponent, index, ei, data, validatedType, obligations)

    val isChecked = FormDataHelpers
      .dataEnteredInGroup(grp, ei.fieldData.data)

    formComponent.presentationHint match {
      case Some(list) if list.contains(CollapseGroupUnderLabel) =>
        html.form.snippets.collapsable(formComponent.id, formComponent.label.value, grpHtml, isChecked)
      case _ => grpHtml
    }
  }

  private def htmlForGroup0(
    groupField: Group,
    formTemplateId: FormTemplateId,
    formComponent: FormComponent,
    index: Int,
    ei: ExtraInfo,
    data: FormDataRecalculated,
    validatedType: ValidatedType[ValidationResult],
    obligations: Obligations)(implicit request: Request[_], messages: Messages, l: LangADT) = {
    val maybeHint =
      formComponent.helpText.map(localisedString => localisedString.value).map(markDownParser).map(Html.apply)

    val (lhtml, limitReached) =
      getGroupForRendering(
        formComponent,
        formTemplateId,
        groupField,
        groupField.orientation,
        validatedType,
        ei,
        data,
        obligations)

    html.form.snippets.group(formComponent, maybeHint, groupField, lhtml, groupField.orientation, limitReached, index)
  }

  private def getRepeatingGroupsForRendering(
    formComponent: FormComponent,
    group: Group,
    fieldData: FormDataRecalculated): (List[GroupList], Boolean) = {

    val max = group.repeatsMax.getOrElse(1)

    val existingGroups = getAllFieldsInGroup(formComponent, group, fieldData) match {
      case Nil => List(group.baseGroupList)
      case xs  => xs
    }

    val groupsForRendering = fillToMin(existingGroups, group)

    (groupsForRendering, max == groupsForRendering.size)
  }

  private def getGroupForRendering(
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    groupField: Group,
    orientation: Orientation,
    validatedType: ValidatedType[ValidationResult],
    ei: ExtraInfo,
    data: FormDataRecalculated,
    obligations: Obligations)(implicit request: Request[_], messages: Messages, l: LangADT): (List[Html], Boolean) =
    if (groupField.repeatsMax.isDefined) {
      val (groupList, isLimit) = getRepeatingGroupsForRendering(formComponent, groupField, ei.fieldData)
      val gl: List[GroupList] = groupList
      val htmls = groupList.zipWithIndex
        .map {
          case (gl, count) =>
            val lhtml = gl.componentList
              .map(fv => htmlFor(fv, formTemplateId, count + 1, ei, data, validatedType, obligations = obligations))

            val showButton = {
              groupField.repeatsMax.getOrElse(0) == groupField.repeatsMin.getOrElse(0) ||
              groupList.size <= groupField.repeatsMin.getOrElse(1)
            }
            html.form.snippets.group_element(formComponent, groupField, lhtml, orientation, count + 1, showButton)

        }

      (htmls, isLimit)
    } else {
      val htmls =
        groupField.fields.map(fv => htmlFor(fv, formTemplateId, 0, ei, data, validatedType, obligations = obligations))
      (htmls, true)
    }

  private def buildFormFieldValidationResult(
    formComponent: FormComponent,
    ei: ExtraInfo,
    validatedType: ValidatedType[ValidationResult],
    data: FormDataRecalculated): Option[FormFieldValidationResult] = {
    // TODO: Simplify building this result. When this method is called we already know what component we are dealing with
    // TODO: it is possible to get inner fields (if any) and build the result.
    val gformErrors: Map[FormComponentId, Set[String]] = validatedType match {
      case Invalid(errors) => errors
      case Valid(_)        => Map.empty[FormComponentId, Set[String]]
    }
    val section: BaseSection = ei.dynamicSections(ei.sectionNumber.value)

    val fieldValues: List[FormComponent] = RepeatingComponentService.atomicFields(section, data.data)

    Fields.getValidationResult(ei.fieldData, fieldValues, ei.envelope, gformErrors)(formComponent)
  }

  private def emptyRetrievals = AuthenticatedRetrievals(
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

  private def shouldDisplayHeading(section: Section, GFC579Ready: String): Boolean =
    section.fields
      .filter {
        case IsGroup(g)     => false
        case IsFileUpload() => false
        case _              => true
      }
      .count(field => field.editable && field.label == section.title) != 1 || GFC579Ready == "false"
}
