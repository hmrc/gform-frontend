/*
 * Copyright 2020 HM Revenue & Customs
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
import play.api.i18n.Messages
import play.api.mvc.{ Request, RequestHeader }
import play.twirl.api.Html

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.auth.core.AffinityGroup.Individual
import uk.gov.hmrc.auth.core.Enrolments
import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
import uk.gov.hmrc.gform.auth.models.{ AuthenticatedRetrievals, GovernmentGatewayId, MaterialisedRetrievals }
import uk.gov.hmrc.gform.commons.MarkDownUtil.markDownParser
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.Origin
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.lookup.{ AjaxLookup, LookupRegistry, RadioLookup }
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.models.javascript.JavascriptMaker
import uk.gov.hmrc.gform.models.helpers.{ Fields, TaxPeriodHelper }
import uk.gov.hmrc.gform.models.{ DateExpr, SectionRenderingInformation }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Register
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.gform.ops.FormComponentOps
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation._
import uk.gov.hmrc.gform.views.summary.TextFormatter
import uk.gov.hmrc.gform.views.{ ViewHelpersAlgebra, html }
import uk.gov.hmrc.gform.views.components.TotalText
import uk.gov.hmrc.govukfrontend.views.html.components
import uk.gov.hmrc.govukfrontend.views.viewmodels.charactercount.CharacterCount
import uk.gov.hmrc.govukfrontend.views.viewmodels.input.Input
import uk.gov.hmrc.govukfrontend.views.viewmodels.textarea.{ Textarea => govukTextArea }
import uk.gov.hmrc.govukfrontend.views.viewmodels.hint.Hint
import uk.gov.hmrc.govukfrontend.views.viewmodels.label.Label
import uk.gov.hmrc.govukfrontend.views.viewmodels.fieldset.{ Fieldset, Legend }
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.Radios
import uk.gov.hmrc.govukfrontend.views.viewmodels.errormessage.ErrorMessage
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.{ ErrorLink, ErrorSummary }

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
  implicit ec: ExecutionContext,
  viewHelpers: ViewHelpersAlgebra
) {

  case class ExtraInfo(
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    fieldData: FormDataRecalculated,
    formTemplate: FormTemplate,
    envelope: Envelope,
    dynamicSections: List[BaseSection],
    formMaxAttachmentSizeMB: Int,
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
    obligations: Obligations
  )(implicit request: Request[_], messages: Messages, l: LangADT, sse: SmartStringEvaluator): Html = {

    val section = dynamicSections(sectionNumber.value)
    val formLevelHeading = shouldDisplayHeading(section, formTemplate.GFC579Ready.getOrElse("false"))

    val ei = ExtraInfo(
      maybeAccessCode,
      sectionNumber,
      fieldData,
      formTemplate,
      envelope,
      dynamicSections,
      formMaxAttachmentSizeMB,
      retrievals,
      formLevelHeading)
    val actionForm = uk.gov.hmrc.gform.gform.routes.FormController
      .updateFormData(formTemplate._id, maybeAccessCode, sectionNumber)
    val listResult = errors.map { case (_, validationResult) => validationResult }

    val javascript = JavascriptMaker.generateJs(sectionNumber, dynamicSections, formTemplate)

    val (hiddenTemplateFields, fieldDataUpd) =
      Fields.getHiddenTemplateFields(section, dynamicSections, fieldData, lookupRegistry.extractors)
    val hiddenSnippets = Fields
      .toFormField(fieldDataUpd, hiddenTemplateFields)
      .map(formField => html.form.snippets.hidden_field(formField))

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
      hiddenSnippets,
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
    globalErrors: List[ErrorLink]
  )(implicit messages: Messages): HasErrors = {

    val allValidationResults = listValidation.flatMap {
      case componentField: ComponentField => parseFormFieldValidationResult(componentField)
      case others                         => List(others)
    }

    val errorsHtml: List[ErrorLink] = globalErrors ++ allValidationResults
      .filter(_.isNotOk)
      .flatMap { validationResult =>
        validationResult.fieldErrors
          .map(
            errorMessage =>
              ErrorLink(
                href = Some("#" + validationResult.fieldValue.id.value),
                content = content.Text(errorMessage)
            ))
      }

    if (errorsHtml.nonEmpty) {

      val errorSummary = ErrorSummary(
        errorList = errorsHtml,
        title = content.Text(messages("error.summary.heading"))
      )

      val errorHtml: Html = new components.govukErrorSummary()(errorSummary)
      Errors(errorHtml)
    } else
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
    declarationSectionValue: DeclarationSection,
    retrievals: MaterialisedRetrievals,
    validatedType: ValidatedType[ValidationResult],
    fieldData: FormDataRecalculated,
    errors: List[(FormComponent, FormFieldValidationResult)]
  )(
    implicit hc: HeaderCarrier,
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator): Html = {

    val ei = ExtraInfo(
      maybeAccessCode,
      SectionNumber(0),
      fieldData,
      formTemplate,
      Envelope.empty,
      List(declarationSectionValue),
      0,
      retrievals,
      formLevelHeading = true
    )

    val confirm = formTemplate.formCategory match {
      case HMRCReturnForm => messages("button.acceptAndSubmitForm", messages("formCategory.return"))
      case HMRCClaimForm  => messages("button.acceptAndSubmitForm", messages("formCategory.claim"))
      case _              => messages("button.acceptAndSubmit")
    }

    val listResult = errors.map { case (_, validationResult) => validationResult }

    val snippets = declarationSectionValue.fields.map(formComponent =>
      htmlFor(formComponent, formTemplate._id, 0, ei, fieldData, validatedType, obligations = NotChecked))
    val pageLevelErrorHtml = generatePageLevelErrorHtml(listResult, List.empty)
    val renderingInfo = SectionRenderingInformation(
      formTemplate._id,
      maybeAccessCode,
      SectionNumber(0),
      declarationSectionValue.title.value,
      declarationSectionValue.description.map(ls => ls.value),
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

  def renderPrintSection(
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate,
    destinationPrint: DestinationPrint)(
    implicit hc: HeaderCarrier,
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator): Html = {
    val parsedTitle = destinationPrint.page.title
    val parsedSummaryPdf = markDownParser(destinationPrint.page.instructions)

    uk.gov.hmrc.gform.views.html.hardcoded.pages.partials
      .print_section(
        formTemplate,
        frontendAppConfig,
        parsedTitle,
        parsedSummaryPdf,
        maybeAccessCode
      )
  }

  def renderAcknowledgementSection(
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate,
    destinationList: DestinationList,
    retrievals: MaterialisedRetrievals,
    envelopeId: EnvelopeId)(
    implicit hc: HeaderCarrier,
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator): Future[Html] = {

    val ei = ExtraInfo(
      maybeAccessCode,
      SectionNumber(0),
      FormDataRecalculated.empty,
      formTemplate,
      Envelope.empty,
      List(destinationList.acknowledgementSection),
      0,
      retrievals,
      formLevelHeading = false
    )

    val formCategory = formTemplate.formCategory
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val now = ZonedDateTime.now(ZoneId.of("Europe/London"))
    val timeMessage = s""" at ${now.format(timeFormat)} on ${now.format(dateFormat)}"""
    for {
      snippets <- Future.traverse(destinationList.acknowledgementSection.fields)(
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
        destinationList.acknowledgementSection.title.value,
        destinationList.acknowledgementSection.description.map(ls => ls.value),
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
        .acknowledgement(timeMessage, renderingInfo, formCategory, formTemplate, frontendAppConfig)
  }

  def renderEnrolmentSection(
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    enrolmentSection: EnrolmentSection,
    fieldData: FormDataRecalculated,
    errors: List[(FormComponent, FormFieldValidationResult)],
    globalErrors: List[ErrorLink],
    validatedType: ValidatedType[ValidationResult]
  )(
    implicit hc: HeaderCarrier,
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator): Html = {

    val maybeAccessCode = None
    // This is only used for a file upload component, which should not appear in an enrollment section
    val ei = ExtraInfo(
      maybeAccessCode,
      SectionNumber(0),
      fieldData,
      formTemplate,
      Envelope.empty,
      List(enrolmentSection),
      0,
      emptyRetrievals,
      formLevelHeading = true
    )
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
      messages("button.confirmAndSend"),
      0,
      Nil
    )
    html.form
      .form(formTemplate, pageLevelErrorHtml, renderingInfo, false, true, true, frontendAppConfig, false)
  }

  private def htmlFor(
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    index: Int,
    ei: ExtraInfo,
    data: FormDataRecalculated,
    maybeValidated: ValidatedType[ValidationResult],
    isHidden: Boolean = false,
    obligations: Obligations)(
    implicit request: RequestHeader,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator): Html =
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
      case t @ Text(_, _, _, _) =>
        renderText(t, formComponent, index, maybeValidated, ei, data, isHidden)
      case t @ TextArea(_, _, _) =>
        renderTextArea(t, formComponent, index, maybeValidated, ei, data, isHidden)
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
      case RevealingChoice(options, multiValue) =>
        htmlForRevealingChoice(
          formComponent,
          multiValue,
          formTemplateId,
          options,
          index,
          maybeValidated,
          ei,
          data,
          obligations)
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
    hmrcTP: HmrcTaxPeriod)(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) = {

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
    infoText: SmartString,
    index: Int,
    ei: ExtraInfo)(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) = {
    val parsedContent = markDownParser(infoText)
    html.form.snippets.field_template_info(formComponent, infoType, parsedContent, index)
  }

  private def htmlForFileUpload(
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    index: Int,
    ei: ExtraInfo,
    data: FormDataRecalculated,
    materialisedRetrievals: MaterialisedRetrievals,
    validatedType: ValidatedType[ValidationResult])(
    implicit messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator) = {
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

  private def htmlForChoice(
    formComponent: FormComponent,
    choice: ChoiceType,
    options: NonEmptyList[SmartString],
    orientation: Orientation,
    selections: List[Int],
    optionalHelpText: Option[NonEmptyList[SmartString]],
    index: Int,
    validatedType: ValidatedType[ValidationResult],
    ei: ExtraInfo,
    data: FormDataRecalculated)(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) = {

    val prepopValues =
      if (ei.fieldData.data.contains(formComponent.id)) Set.empty[String] // Don't prepop something we already submitted
      else selections.map(_.toString).toSet

    val optionalHelpTextMarkDown: NonEmptyList[Html] =
      optionalHelpText.fold(options.map(_ => Html(""))) { helpTexts =>
        helpTexts.map(ht => markDownParser(ht))
      }

    val validatedValue = buildFormFieldValidationResult(formComponent, ei, validatedType, data)
    choice match {
      case Radio | YesNo =>
        val items = options.zipWithIndex.map {
          case (option, index) =>
            RadioItem(
              id = Some(formComponent.id.appendIndex(index).value),
              value = Some(index.toString),
              content = content.Text(option.value),
              checked = validatedValue
                .flatMap(_.getOptionalCurrentValue(formComponent.id.value + index.toString))
                .orElse(prepopValues.find(_ === index.toString))
                .isDefined
            )
        }

        val hint = formComponent.helpText.map { ls =>
          Hint(
            content = content.Text(ls.value)
          )
        }

        val fieldset = Some(
          Fieldset(
            legend = Some(
              Legend(
                content = content.Text(formComponent.label.value),
                isPageHeading = ei.formLevelHeading
              ))
          ))

        val map: Map[String, Set[String]] =
          validatedValue.map(x => ValidationUtil.renderErrors("", x)).getOrElse(Map.empty)
        val errors: Option[String] = ValidationUtil.printErrors(map).headOption

        val radios = Radios(
          fieldset = fieldset,
          hint = hint,
          errorMessage = errors.map(
            error =>
              ErrorMessage(
                content = content.Text(error)
            )),
          name = formComponent.id.value,
          items = items.toList
        )

        val govukErrorMessage: components.govukErrorMessage = new components.govukErrorMessage()
        val govukFieldset: components.govukFieldset = new components.govukFieldset()
        val govukHint: components.govukHint = new components.govukHint()
        val govukLabel: components.govukLabel = new components.govukLabel()
        new components.govukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)

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
          ei.formLevelHeading
        )
      case Inline =>
        html.form.snippets
          .choiceInline(formComponent, options, prepopValues, validatedValue, optionalHelpTextMarkDown, index)
    }
  }

  private def htmlForRevealingChoice(
    fieldValue: FormComponent,
    multiValue: Boolean,
    formTemplateId: FormTemplateId,
    options: NonEmptyList[RevealingChoiceElement],
    index: Int,
    validatedType: ValidatedType[ValidationResult],
    extraInfo: ExtraInfo,
    data: FormDataRecalculated,
    obligations: Obligations)(
    implicit request: RequestHeader,
    message: Messages,
    l: LangADT,
    sse: SmartStringEvaluator) = {
    val validatedValue = buildFormFieldValidationResult(fieldValue, extraInfo, validatedType, data)
    val nestedEi = extraInfo.copy(formLevelHeading = true)
    val revealingChoicesList =
      options.map { o =>
        val isSelected: Int => Boolean =
          index =>
            extraInfo.fieldData.data
              .get(fieldValue.id)
              .fold(o.selected)(_.contains(index.toString))
        (
          o.choice,
          isSelected,
          o.revealingFields
            .filterNot(_.onlyShowOnSummary)
            .map(htmlFor(_, formTemplateId, index, nestedEi, data, validatedType, obligations = obligations)))
      }

    html.form.snippets
      .revealingChoice(
        fieldValue,
        if (multiValue) "checkbox" else "radio",
        revealingChoicesList,
        validatedValue,
        index,
        extraInfo.formLevelHeading)
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
  )(implicit l: LangADT, sse: SmartStringEvaluator): Html = {

    val prepopValue = ei.fieldData.data.one(fieldValue.id)
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
            options.process(_.sorted),
            ei.formLevelHeading,
            prepopValue,
            validatedValue,
            index
          )
      }
  }

  private def renderTextArea(
    text: TextArea,
    formComponent: FormComponent,
    index: Int,
    validatedType: ValidatedType[ValidationResult],
    ei: ExtraInfo,
    data: FormDataRecalculated,
    isHidden: Boolean
  )(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) = {
    val prepopValue = ei.fieldData.data.one(formComponent.id)
    val validatedValue = buildFormFieldValidationResult(formComponent, ei, validatedType, data)
    if (isHidden)
      html.form.snippets
        .hidden_field_populated(
          List(FormRender(formComponent.id.value, formComponent.id.value, prepopValue.getOrElse(""))))
    else {
      val labelContent = content.Text(LabelHelper.buildRepeatingLabel(formComponent.label, index).value)

      val map: Map[String, Set[String]] =
        validatedValue.map(x => ValidationUtil.renderErrors("", x)).getOrElse(Map.empty)
      val errors: Option[String] = ValidationUtil.printErrors(map).headOption

      val errorMessage: Option[ErrorMessage] = errors.map(
        error =>
          ErrorMessage(
            content = content.Text(error)
        ))

      val hint: Option[Hint] = formComponent.helpText.map { ls =>
        Hint(
          content = content.Text(ls.value)
        )
      }

      val characterMaxLength = text.constraint match {
        case ShortText(_, max)            => Some(max)
        case TextWithRestrictions(_, max) => Some(max)
        case _                            => None
      }

      val maybeCurrentValue: Option[String] = prepopValue.orElse(validatedValue.flatMap(_.getCurrentValue))

      val sizeClasses = text.displayWidth match {
        case DisplayWidth.XS      => "govuk-input--width-3"
        case DisplayWidth.S       => "govuk-input--width-10"
        case DisplayWidth.M       => "govuk-input--width-20"
        case DisplayWidth.L       => "govuk-input--width-30"
        case DisplayWidth.XL      => "govuk-input--width-40"
        case DisplayWidth.XXL     => "govuk-input--width-50"
        case DisplayWidth.DEFAULT => "govuk-input--width-30"
      }

      val isPageHeading = !ei.formLevelHeading

      val label = Label(
        isPageHeading = isPageHeading,
        classes = if (isPageHeading) "govuk-label--xl" else "",
        content = labelContent
      )

      val govukErrorMessage: components.govukErrorMessage = new components.govukErrorMessage()
      val govukHint: components.govukHint = new components.govukHint()
      val govukLabel: components.govukLabel = new components.govukLabel()
      val govukTextarea = new components.govukTextarea(govukErrorMessage, govukHint, govukLabel)

      characterMaxLength match {
        case Some(maxLength) =>
          val characterCount = CharacterCount(
            id = formComponent.id.value,
            name = formComponent.id.value,
            label = label,
            hint = hint,
            value = maybeCurrentValue,
            maxLength = Some(maxLength),
            errorMessage = errorMessage,
            classes = sizeClasses
          )

          new components.govukCharacterCount(govukTextarea, govukHint)(characterCount)

        case _ =>
          val textArea = govukTextArea(
            id = formComponent.id.value,
            name = formComponent.id.value,
            label = label,
            hint = hint,
            value = maybeCurrentValue,
            errorMessage = errorMessage,
            classes = sizeClasses
          )

          govukTextarea(textArea)
      }
    }
  }

  private def renderText(
    text: Text,
    formComponent: FormComponent,
    index: Int,
    validatedType: ValidatedType[ValidationResult],
    ei: ExtraInfo,
    data: FormDataRecalculated,
    isHidden: Boolean
  )(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) = {
    val prepopValue = ei.fieldData.data.one(formComponent.id)
    val validatedValue = buildFormFieldValidationResult(formComponent, ei, validatedType, data)
    if (isHidden)
      html.form.snippets
        .hidden_field_populated(
          List(FormRender(formComponent.id.value, formComponent.id.value, prepopValue.getOrElse(""))))
    else {

      val maybeUnit = TextFormatter.appendUnit(text.constraint)
      val labelContent = content.Text(LabelHelper.buildRepeatingLabel(formComponent.label, index).value)

      val map: Map[String, Set[String]] =
        validatedValue.map(x => ValidationUtil.renderErrors("", x)).getOrElse(Map.empty)
      val errors: Option[String] = ValidationUtil.printErrors(map).headOption

      val errorMessage: Option[ErrorMessage] = errors.map(
        error =>
          ErrorMessage(
            content = content.Text(error)
        ))

      val hint: Option[Hint] = formComponent.helpText.map { ls =>
        Hint(
          content = content.Text(ls.value)
        )
      }

      val maybeCurrentValue: Option[String] = prepopValue.orElse(validatedValue.flatMap(_.getCurrentValue))

      formComponent.presentationHint match {
        case Some(xs) if xs.contains(TotalValue) =>
          val totalText = new TotalText(formComponent, labelContent, maybeUnit, hint, errorMessage, maybeCurrentValue)

          html.form.snippets.field_template_text_total(totalText)

        case _ =>
          val sizeClasses = text.displayWidth match {
            case DisplayWidth.XS  => "govuk-input--width-2"
            case DisplayWidth.S   => "govuk-input--width-3"
            case DisplayWidth.M   => "govuk-input--width-4"
            case DisplayWidth.L   => "govuk-input--width-10"
            case DisplayWidth.XL  => "govuk-input--width-20"
            case DisplayWidth.XXL => "govuk-input--width-30"
            case DisplayWidth.DEFAULT =>
              if (TextFormatter.isNumber(formComponent) || formComponent.isSterling) "govuk-input--width-10"
              else "govuk-input--width-5"
          }

          val isPageHeading = !ei.formLevelHeading
          val label = Label(
            isPageHeading = isPageHeading,
            classes = if (isPageHeading) "govuk-label--xl" else "",
            content = labelContent
          )

          val input = Input(
            id = formComponent.id.value,
            name = formComponent.id.value,
            label = label,
            hint = hint,
            value = maybeCurrentValue,
            errorMessage = errorMessage,
            classes = sizeClasses
          )
          val govukErrorMessage: components.govukErrorMessage = new components.govukErrorMessage()
          val govukHint: components.govukHint = new components.govukHint()
          val govukLabel: components.govukLabel = new components.govukLabel()
          val govukInput: Html = new components.govukInput(govukErrorMessage, govukHint, govukLabel)(input)

          maybeUnit.fold(govukInput)(GovukExtensions.insertUnit(govukInput))
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
    isHidden: Boolean)(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) = {
    val prepopValue = ei.fieldData.data.oneOrElse(formComponent.id, "")
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
    data: FormDataRecalculated)(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) = {
    val fieldValues = buildFormFieldValidationResult(formComponent, ei, validatedType, data)
    html.form.snippets
      .field_template_address(international, formComponent, fieldValues, index, ei.formLevelHeading)
  }

  private def htmlForDate(
    formComponent: FormComponent,
    offset: Offset,
    dateValue: Option[DateValue],
    index: Int,
    validatedType: ValidatedType[ValidationResult],
    ei: ExtraInfo,
    data: FormDataRecalculated,
    isHidden: Boolean = false)(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) = {
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
    obligations: Obligations)(
    implicit request: RequestHeader,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator): Html = {
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
    obligations: Obligations)(
    implicit request: RequestHeader,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator) = {
    val maybeHint =
      formComponent.helpText.map(markDownParser)

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
    obligations: Obligations)(
    implicit request: RequestHeader,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator): (List[Html], Boolean) =
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
    governmentGatewayId = GovernmentGatewayId(""),
    enrolments = Enrolments(Set.empty),
    affinityGroup = Individual,
    groupIdentifier = "",
    maybeNino = None
  )

  private def shouldDisplayHeading(section: Section, GFC579Ready: String): Boolean = {
    val filteredSections = section.fields
      .filter {
        case IsGroup(g)     => false
        case IsFileUpload() => false
        case _              => true
      }

    filteredSections.count(field => field.editable && field.label == section.title) != 1 || filteredSections.size != 1 || GFC579Ready == "false"
  }
}
