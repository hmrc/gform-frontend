/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.service

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.inject.{ Inject, Singleton }

import uk.gov.hmrc.gform.views.html.form._
import uk.gov.hmrc.gform.views.html.hardcoded
import cats.data.NonEmptyList
import cats.data.Validated.{ Invalid, Valid }
import org.intellij.markdown.flavours.gfm.GFMFlavourDescriptor
import org.intellij.markdown.html.HtmlGenerator
import org.intellij.markdown.parser.MarkdownParser
import org.jsoup.Jsoup
import play.api.Logger
import play.api.i18n.Messages
import play.api.mvc.Request
import play.twirl.api.Html
import uk.gov.hmrc.auth.core.authorise.AffinityGroup.Individual
import uk.gov.hmrc.auth.core.authorise.Enrolments
import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
import uk.gov.hmrc.gform.auth.models.{ Retrievals, UserDetails }
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.models.helpers.Javascript._
import uk.gov.hmrc.gform.models.{ DateExpr, SectionRenderingInformation }
import uk.gov.hmrc.gform.models.{ DateExpr, SectionRenderingInformation }
import uk.gov.hmrc.gform.prepop.{ PrepopModule, PrepopService }
import uk.gov.hmrc.gform.validation._
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.Equals
import scala.concurrent.duration._
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import scala.concurrent.Future

@Singleton
class SectionRenderingService @Inject() (repeatService: RepeatingComponentService, prePopModule: PrepopModule) {
  val prepopService: PrepopService = prePopModule.prepopService

  case class ExtraInfo(
    formId: FormId,
    sectionNumber: SectionNumber,
    fieldData: Map[FormComponentId, Seq[String]],
    formTemplate: FormTemplate,
    envelope: Envelope,
    dynamicSections: List[BaseSection],
    formMaxAttachmentSizeMB: Int,
    retrievals: Retrievals
  )

  def renderSection(
    formId: FormId,
    sectionNumber: SectionNumber,
    fieldData: Map[FormComponentId, Seq[String]],
    formTemplate: FormTemplate,
    errors: Option[Map[FormComponent, FormFieldValidationResult]],
    envelope: Envelope,
    envelopeId: EnvelopeId,
    validatedType: Option[ValidatedType],
    dynamicSections: List[Section],
    formMaxAttachmentSizeMB: Int,
    contentTypes: List[ContentType],
    retrievals: Retrievals,
    lang: Option[String]
  )(implicit hc: HeaderCarrier, request: Request[_], messages: Messages): Future[Html] = {

    val ei = ExtraInfo(formId, sectionNumber, fieldData, formTemplate, envelope, dynamicSections, formMaxAttachmentSizeMB, retrievals)
    val section = dynamicSections(sectionNumber.value)
    val actionForm = uk.gov.hmrc.gform.controllers.routes.FormController.updateFormData(formId, sectionNumber, lang)
    val listResult = errors.map(error => error.values.toList).getOrElse(Nil)

    for {
      snippetsForFields <- Future.sequence(section.fields.map(fieldValue => htmlFor(fieldValue, formTemplate._id, 0, ei, dynamicSections.size, validatedType, lang)))
      javascript <- createJavascript(dynamicSections.flatMap(_.fields), dynamicSections.flatMap(repeatService.atomicFields))
      hiddenTemplateFields = dynamicSections.filterNot(_ == section).flatMap(repeatService.atomicFields)
      hiddenSnippets = Fields.toFormField(fieldData, hiddenTemplateFields).map(formField => snippets.hidden_field(formField))
      bec = pageLevel(listResult)
      renderingInfo = SectionRenderingInformation(formId, sectionNumber, section.title, section.description, hiddenSnippets, snippetsForFields, javascript, envelopeId, actionForm, true, "Save and Continue", formMaxAttachmentSizeMB, contentTypes)
    } yield form(formTemplate, bec, renderingInfo, formId)
  }

  def pageLevel(listValidation: List[FormFieldValidationResult]): Html = {
    val nestedResult: List[FormFieldValidationResult] = listValidation.collect { case componentField: ComponentField => componentField }.flatMap(parseFormFieldValidationResult)
    val list: List[FormFieldValidationResult] = listValidation.filter {
      case x: ComponentField => false
      case _ => true
    }.toList
    val newList: Seq[FormFieldValidationResult] = (nestedResult ::: list)
    val sec: List[Html] = newList.filter(_.isNotOk).flatMap { x =>
      x.fieldErrors.map(y =>
        errors.error_message_component(x, y))
    }.toList
    if (listValidation.exists(_.isNotOk))
      errors.page_level_error(sec, listValidation)
    else
      errors.empty_html()
  }

  def parseFormFieldValidationResult(result: ComponentField): List[FormFieldValidationResult] = {
    def reassignFieldValue(id: String, x: FormFieldValidationResult): FormFieldValidationResult = x match {
      case x: FieldError =>
        val newFieldValue = x.fieldValue.copy(id = FormComponentId(id))
        x.copy(fieldValue = newFieldValue)
      case y: FieldGlobalError =>
        val newFieldValue = y.fieldValue.copy(id = FormComponentId(id))
        y.copy(fieldValue = newFieldValue)
      case err => err
    }
    result.data.map(field => reassignFieldValue(field._1, field._2)).toList
  }

  def renderDeclarationSection(formId: FormId, formTemplate: FormTemplate, retrievals: Retrievals, maybeValidatedType: Option[ValidatedType], errors: Option[Map[FormComponent, FormFieldValidationResult]], lang: Option[String])(implicit hc: HeaderCarrier, request: Request[_], messages: Messages): Future[Html] = {

    val ei = ExtraInfo(formId, SectionNumber(0), Map.empty, formTemplate, Envelope(Nil), List(formTemplate.declarationSection), 0, retrievals)

    val listResult = errors.map(error => error.values.toList).getOrElse(Nil)
    for {
      snippets <- Future.sequence(formTemplate.declarationSection.fields.map(fieldValue => htmlFor(fieldValue, formTemplate._id, 0, ei, formTemplate.sections.size, maybeValidatedType, lang)))
      bec = pageLevel(listResult)
      renderingInfo = SectionRenderingInformation(formId, SectionNumber(0), formTemplate.declarationSection.title, formTemplate.declarationSection.description, Nil, snippets, "", EnvelopeId(""), uk.gov.hmrc.gform.controllers.routes.DeclarationController.submitDeclaration(formTemplate._id, formId, lang), false, "Confirm and send", 0, Nil)
    } yield form(formTemplate, bec, renderingInfo, formId)
  }

  def renderAcknowledgementSection(formId: FormId, formTemplate: FormTemplate, retrievals: Retrievals, lang: Option[String])(implicit hc: HeaderCarrier, request: Request[_], messages: Messages): Future[Html] = {

    val ei = ExtraInfo(formId, SectionNumber(0), Map.empty, formTemplate, Envelope(Nil), List(formTemplate.acknowledgementSection), 0, retrievals)

    val formCategory = formTemplate.formCategory.getOrElse(Default)
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val now = LocalDateTime.now()
    val timeMessage = s""" at ${now.format(timeFormat)} on ${now.format(dateFormat)}"""
    for {
      snippets <- Future.sequence(formTemplate.acknowledgementSection.fields.map(fieldValue => htmlFor(fieldValue, formTemplate._id, 0, ei, formTemplate.sections.size, None, lang)))
      renderingInfo = SectionRenderingInformation(formId, SectionNumber(0), formTemplate.acknowledgementSection.title, formTemplate.acknowledgementSection.description, Nil, snippets, "", EnvelopeId(""), uk.gov.hmrc.gform.controllers.routes.DeclarationController.submitDeclaration(formTemplate._id, formId, lang), false, "Confirm and send", 0, Nil)
    } yield uk.gov.hmrc.gform.views.html.hardcoded.pages.partials.acknowledgement(timeMessage, renderingInfo, formCategory, formTemplate, lang)
  }

  def renderEnrolmentSection(formTemplate: FormTemplate, enrolmentSection: EnrolmentSection, validatedType: Option[ValidatedType], lang: Option[String])(implicit hc: HeaderCarrier, request: Request[_], messages: Messages): Future[Html] = {
    val formId = FormId("")
    val ei = ExtraInfo(formId, SectionNumber(0), Map.empty, formTemplate, Envelope(Nil), List(enrolmentSection), 0, emptyRetrievals)
    for {
      snippets <- Future.sequence(enrolmentSection.fields.map(fieldValue => htmlFor(fieldValue, formTemplate._id, 0, ei, formTemplate.sections.size, validatedType, lang)))
      renderingInfo = SectionRenderingInformation(formId, SectionNumber(0), enrolmentSection.title, None, Nil, snippets, "", EnvelopeId(""), uk.gov.hmrc.gform.controllers.routes.EnrolmentController.submitEnrolment(formTemplate._id, lang), false, "Confirm and send", 0, Nil)
    } yield form(formTemplate, errors.empty_html(), renderingInfo, formId)
  }

  private def createJavascript(fieldList: List[FormComponent], atomicFields: List[FormComponent])(implicit hc: HeaderCarrier): Future[String] = {
    val groups: List[(FormComponentId, Group)] = fieldList.filter(_.presentationHint.getOrElse(Nil).contains(CollapseGroupUnderLabel)).map(fv => (fv.id, fv.`type`)).collect {
      case (fieldId, group: Group) => (fieldId, group)
    }

    val cacheMap: Future[CacheMap] = repeatService.getAllRepeatingGroups
    val repeatingSections: Future[List[List[List[FormComponent]]]] = Future.sequence(fieldList.map(fv => (fv.id, fv.`type`)).collect {
      case (fieldId, group: Group) => cacheMap.map(_.getEntry[List[List[FormComponent]]](fieldId.value).getOrElse(Nil))
    })
    fieldJavascript(atomicFields, repeatingSections).flatMap { x =>
      Future.sequence(groups.map { case (fieldId, group) => Future.successful(collapsingGroupJavascript(fieldId, group)) }).map(_.mkString("\n")).map(y => y + x)
    }
  }

  private def htmlFor(fieldValue: FormComponent, formTemplateId4Ga: FormTemplateId, index: Int, ei: ExtraInfo, totalSections: Int, maybeValidated: Option[ValidatedType], lang: Option[String])(implicit hc: HeaderCarrier, request: Request[_], messages: Messages): Future[Html] = {
    fieldValue.`type` match {
      case sortCode @ UkSortCode(expr) => htmlForSortCode(fieldValue, sortCode, expr, index, maybeValidated, ei)
      case g @ Group(_, _, _, _, _, _) => htmlForGroup(g, formTemplateId4Ga, fieldValue, index, ei, maybeValidated, lang)
      case Date(_, offset, dateValue) => Future.successful(htmlForDate(fieldValue, offset, dateValue, index, maybeValidated, ei))
      case Address(international) => Future.successful(htmlForAddress(fieldValue, international, index, maybeValidated, ei))
      case t @ Text(_, expr) => htmlForText(fieldValue, t, expr, index, maybeValidated, ei)
      case Choice(choice, options, orientation, selections, optionalHelpText) => Future.successful(htmlForChoice(fieldValue, choice, options, orientation, selections, optionalHelpText, index, maybeValidated, ei))
      case FileUpload() => Future.successful(htmlForFileUpload(fieldValue, formTemplateId4Ga, index, ei, totalSections, maybeValidated, lang))
      case InformationMessage(infoType, infoText) => htmlForInformationMessage(fieldValue, infoType, infoText, index, ei)
    }
  }

  private def htmlForInformationMessage(fieldValue: FormComponent, infoType: InfoType, infoText: String, index: Int, ei: ExtraInfo) = {
    val parsedMarkdownText = markDownParser(infoText)
    Future.successful(snippets.field_template_info(fieldValue, infoType, Html(parsedMarkdownText), index))
  }

  private def htmlForFileUpload(fieldValue: FormComponent, formTemplateId4Ga: FormTemplateId, index: Int, ei: ExtraInfo, totalSections: Int, validatedType: Option[ValidatedType], lang: Option[String])(implicit hc: HeaderCarrier) = {
    snippets.field_template_file_upload(ei.formId, formTemplateId4Ga, ei.sectionNumber, fieldValue, validate(fieldValue, ei, validatedType), index, ei.formMaxAttachmentSizeMB, totalSections, lang)
  }

  private def markDownParser(markDownText: String): String = {
    if (markDownText.nonEmpty) {
      val flavour = new GFMFlavourDescriptor
      val parsedTree = new MarkdownParser(flavour).buildMarkdownTreeFromString(markDownText)
      new HtmlGenerator(markDownText, parsedTree, flavour, false).generateHtml
    } else
      markDownText
  }

  private def htmlForChoice(fieldValue: FormComponent, choice: ChoiceType, options: NonEmptyList[String], orientation: Orientation, selections: List[Int], optionalHelpText: Option[List[String]], index: Int, validatedType: Option[ValidatedType], ei: ExtraInfo)(implicit hc: HeaderCarrier) = {

    def addTargetToLinks(html: String) = {
      val doc = Jsoup.parse(html)
      doc.getElementsByTag("a").attr("target", "_blank")
      doc.html
    }

    val prepopValues = ei.fieldData.get(fieldValue.id) match {
      case None => selections.map(_.toString).toSet
      case Some(_) => Set.empty[String] // Don't prepop something we already submitted
    }

    val optionalHelpTextMarkDown: List[Html] = optionalHelpText.map(_.map(markDownParser)).map(_.map(x =>
      if (x.nonEmpty) {
        Html(addTargetToLinks(x))
      } else {
        Html("")
      })).getOrElse(
      options.toList.map(_ => Html("Daniel TODO"))
    )

    val validatedValue = validate(fieldValue, ei, validatedType)

    Logger.debug(optionalHelpTextMarkDown.toString + "THIS IS A VALUE")
    choice match {
      case Radio | YesNo => snippets.choice("radio", fieldValue, options, orientation, prepopValues, validatedValue, optionalHelpTextMarkDown, index)
      case Checkbox => snippets.choice("checkbox", fieldValue, options, orientation, prepopValues, validatedValue, optionalHelpTextMarkDown, index)
      case Inline => snippets.choiceInline(fieldValue, options, prepopValues, validatedValue, optionalHelpTextMarkDown, index)
    }
  }

  private def htmlForText(fieldValue: FormComponent, t: Text, expr: Expr, index: Int, validatedType: Option[ValidatedType], ei: ExtraInfo)(implicit hc: HeaderCarrier) = {
    val prepopValueF = ei.fieldData.get(fieldValue.id) match {
      case None => prepopService.prepopData(expr, ei.formTemplate._id, ei.retrievals)
      case _ => Future.successful("") // Don't prepop something we already submitted
    }
    val validatedValue = validate(fieldValue, ei, validatedType)

    val isStirling = fieldValue.`type` match {
      case Text(Sterling, _) => true
      case _ => false
    }

    for {
      prepopValue <- prepopValueF
    } yield snippets.field_template_text(fieldValue, t, prepopValue, validatedValue, index)
  }

  private def htmlForSortCode(fieldValue: FormComponent, sC: UkSortCode, expr: Expr, index: Int, validatedType: Option[ValidatedType], ei: ExtraInfo)(implicit hc: HeaderCarrier) = {
    val prepopValueF = ei.fieldData.get(fieldValue.id) match {
      case None => prepopService.prepopData(expr, ei.formTemplate._id, ei.retrievals)
      case _ => Future.successful("") // Don't prepop something we already submitted
    }
    val validatedValue = validate(fieldValue, ei, validatedType)

    for {
      prepopValue <- prepopValueF
    } yield snippets.field_template_sort_code(fieldValue, sC, prepopValue, validatedValue, index)

  }

  private def htmlForAddress(fieldValue: FormComponent, international: Boolean, index: Int, validatedType: Option[ValidatedType], ei: ExtraInfo)(implicit hc: HeaderCarrier) = {
    snippets.field_template_address(international, fieldValue, validate(fieldValue, ei, validatedType), index)
  }

  private def htmlForDate(fieldValue: FormComponent, offset: Offset, dateValue: Option[DateValue], index: Int, validatedType: Option[ValidatedType], ei: ExtraInfo)(implicit hc: HeaderCarrier) = {
    val prepopValues = dateValue.map(DateExpr.fromDateValue).map(DateExpr.withOffset(offset, _))
    snippets.field_template_date(fieldValue, validate(fieldValue, ei, validatedType), prepopValues, index)
  }

  private def htmlForGroup(grp: Group, formTemplateId4Ga: FormTemplateId, fieldValue: FormComponent, index: Int, ei: ExtraInfo, validatedType: Option[ValidatedType], lang: Option[String])(implicit hc: HeaderCarrier, request: Request[_], messages: Messages): Future[Html] = {
    val fgrpHtml = htmlForGroup0(grp, formTemplateId4Ga, fieldValue, index, ei, validatedType, lang)

    validatedType.map(_.isInvalid)
    val isChecked = {
      validatedType.exists(_.isInvalid) || FormDataHelpers.dataEnteredInGroup(grp, ei.fieldData)
    }

    fieldValue.presentationHint.map(_.contains(CollapseGroupUnderLabel)) match {
      case Some(true) => fgrpHtml.map(grpHtml => snippets.collapsable(fieldValue.id, fieldValue.label, grpHtml, isChecked))
      case _ => fgrpHtml
    }
  }

  private def htmlForGroup0(groupField: Group, formTemplateId4Ga: FormTemplateId, fieldValue: FormComponent, index: Int, ei: ExtraInfo, validatedType: Option[ValidatedType], lang: Option[String])(implicit hc: HeaderCarrier, request: Request[_], messages: Messages) = {
    for {
      (lhtml, limitReached) <- getGroupForRendering(fieldValue, formTemplateId4Ga, groupField, groupField.orientation, validatedType, ei, lang)
    } yield snippets.group(fieldValue, groupField, lhtml, groupField.orientation, limitReached, index)
  }

  private def getGroupForRendering(fieldValue: FormComponent, formTemplateId4Ga: FormTemplateId, groupField: Group, orientation: Orientation, validatedType: Option[ValidatedType], ei: ExtraInfo, lang: Option[String])(implicit hc: HeaderCarrier, request: Request[_], messsages: Messages): Future[(List[Html], Boolean)] = {
    if (groupField.repeatsMax.isDefined) {
      repeatService.getRepeatingGroupsForRendering(fieldValue, groupField).flatMap {
        case (groupList, isLimit) =>
          Future.sequence((1 to groupList.size).map { count =>
            Future.sequence(groupList(count - 1).map(fv =>
              htmlFor(fv, formTemplateId4Ga, count, ei, ei.dynamicSections.size, validatedType, lang))).map { lhtml =>
              snippets.group_element(fieldValue, groupField, lhtml, orientation, count, count == 1)
            }
          }.toList).map(a => (a, isLimit))
      }
    } else {
      Future.sequence(groupField.fields.map(fv => htmlFor(fv, formTemplateId4Ga, 0, ei, ei.dynamicSections.size, validatedType, lang))).map(a => (a, true))
    }
  }

  private def validate(fieldValue: FormComponent, ei: ExtraInfo, validatedType: Option[ValidatedType])(implicit hc: HeaderCarrier): Option[FormFieldValidationResult] = {
    val gformErrors = validatedType.fold[ValidatedType](Valid(()))(identity) match {
      case Invalid(errors) => errors
      case Valid(()) => Map.empty[FormComponentId, Set[String]]
    }
    val section = ei.dynamicSections(ei.sectionNumber.value)
    lazy val okF: FormComponent => Option[FormFieldValidationResult] =
      Fields.valuesValidate(ei.fieldData, repeatService.atomicFields(section), ei.envelope, gformErrors)
    okF(fieldValue)
  }

  private def emptyRetrievals = Retrievals(
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
}

