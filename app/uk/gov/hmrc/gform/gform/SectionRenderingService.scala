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

import java.time.{ LocalDate, LocalTime, ZoneId, ZonedDateTime }
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit.MINUTES

import cats.data.NonEmptyList
import cats.data.Validated.{ Invalid, Valid }
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.validated._
import play.api.i18n.Messages
import play.api.mvc.{ Request, RequestHeader }
import play.twirl.api.{ Html, HtmlFormat }

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
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.models.GroupHelper
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
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.gform.views.components.TotalText
import uk.gov.hmrc.govukfrontend.views.html.components
import uk.gov.hmrc.govukfrontend.views.html.components.govukInput
import uk.gov.hmrc.govukfrontend.views.viewmodels.charactercount.CharacterCount
import uk.gov.hmrc.govukfrontend.views.viewmodels.checkboxes.{ CheckboxItem, Checkboxes }
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.{ Content, Empty, HtmlContent }
import uk.gov.hmrc.govukfrontend.views.viewmodels.dateinput.DateInput
import uk.gov.hmrc.govukfrontend.views.viewmodels.errormessage.ErrorMessage
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.{ ErrorLink, ErrorSummary }
import uk.gov.hmrc.govukfrontend.views.viewmodels.fieldset.{ Fieldset, Legend }
import uk.gov.hmrc.govukfrontend.views.viewmodels.fileupload
import uk.gov.hmrc.govukfrontend.views.viewmodels.hint.Hint
import uk.gov.hmrc.govukfrontend.views.viewmodels.input.Input
import uk.gov.hmrc.govukfrontend.views.viewmodels.label.Label
import uk.gov.hmrc.govukfrontend.views.viewmodels.dateinput.InputItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.{ RadioItem, Radios }
import uk.gov.hmrc.govukfrontend.views.viewmodels.select.{ Select, SelectItem }
import uk.gov.hmrc.govukfrontend.views.viewmodels.textarea.Textarea
import uk.gov.hmrc.govukfrontend.views.viewmodels.warningtext.WarningText
import uk.gov.hmrc.hmrcfrontend.views.html.components.hmrcCurrencyInput
import uk.gov.hmrc.hmrcfrontend.views.viewmodels.currencyinput.CurrencyInput

import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier

import scala.annotation.tailrec

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
    val formLevelHeading = shouldDisplayHeading(section)

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
      shouldDisplayHeading = !formLevelHeading,
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

    def isRadioLookup(register: Option[LookupType]): Boolean = register match {
      case Some(RadioLookup(_)) => true
      case _                    => false
    }

    val errorsHtml: List[ErrorLink] = globalErrors ++ allValidationResults
      .filter(_.isNotOk)
      .flatMap { validationResult =>
        val fieldId = validationResult match {
          case _: FieldGlobalError =>
            validationResult.fieldValue.`type` match {
              case _: UkSortCode => UkSortCode.fields(validationResult.fieldValue.id).toList.head.value
              case _: Date       => Date.fields(validationResult.fieldValue.id).toList.head.value
              case _             => validationResult.fieldValue.id.value
            }

          case _: FieldError =>
            validationResult.fieldValue.`type` match {
              case HmrcTaxPeriod(_, _, _) | Choice(_, _, _, _, _) | RevealingChoice(_, _) =>
                validationResult.fieldValue.id.value + "0"
              case Text(Lookup(register), _, _, _) if isRadioLookup(lookupRegistry.get(register)) =>
                validationResult.fieldValue.id.value + "0"
              case _ => validationResult.fieldValue.id.value
            }

          case _ => validationResult.fieldValue.id.value
        }

        val dataContext = validationResult.fieldValue.id.value
          .replace("-day", "")
          .replace("-month", "")
          .replace("-year", "")

        validationResult.fieldErrors
          .map(
            errorMessage =>
              ErrorLink(
                href = Some("#" + fieldId),
                content = content.Text(errorMessage),
                attributes = Map(
                  "data-context" -> dataContext,
                  "class"        -> "js-hidden"
                )
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
      formLevelHeading = false
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

    val htmlContent: Content =
      if (destinationList.acknowledgementSection.showReference) {
        HtmlContent(
          uk.gov.hmrc.gform.views.html.hardcoded.pages.partials.submission_reference(SubmissionRef(envelopeId)))
      } else {
        Empty
      }

    val formCategory = formTemplate.formCategory
    val now = ZonedDateTime.now(ZoneId.of("Europe/London"))
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
        .acknowledgement(renderingInfo, htmlContent, formCategory, formTemplate, frontendAppConfig)
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
      formLevelHeading = false
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
      case g @ Group(_, _, _, _, _) =>
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
      case t @ Time(_, _) =>
        renderTime(t, formComponent, index, maybeValidated, ei, data, isHidden)
    }

  private def htmlForHmrcTaxPeriod(
    formComponent: FormComponent,
    index: Int,
    ei: ExtraInfo,
    validatedType: ValidatedType[ValidationResult],
    data: FormDataRecalculated,
    obligations: Obligations,
    hmrcTP: HmrcTaxPeriod)(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) = {

    val maybeTaxPeriodOptions: Option[NonEmptyList[OptionParams]] = obligations match {
      case RetrievedObligations(listOfObligations) =>
        val params: Option[List[OptionParams]] = listOfObligations
          .find(_.id.recalculatedTaxPeriodKey.hmrcTaxPeriod === hmrcTP)
          .map(_.obligation.obligations.flatMap(_.obligationDetails.map(od =>
            OptionParams(od.periodKey, od.inboundCorrespondenceFromDate, od.inboundCorrespondenceToDate, false))))

        params match {
          case Some(x :: xs) => Some(NonEmptyList(x, xs))
          case _             => None
        }
      case _ => None
    }

    val validatedValue = buildFormFieldValidationResult(formComponent, ei, validatedType, data)

    val setValue = TaxPeriodHelper.formatTaxPeriodOutput(validatedValue)
    val errors: Option[String] = {
      val lookup: Map[String, Set[String]] =
        validatedValue.map(x => ValidationUtil.renderErrors("", x)).getOrElse(Map.empty)
      ValidationUtil.printErrors(lookup).headOption
    }

    val errorMessage = errors.map(
      error =>
        ErrorMessage(
          content = content.Text(error)
      ))

    val label = formComponent.label.value

    val isPageHeading = ei.formLevelHeading

    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = content.Text(label),
            isPageHeading = isPageHeading,
            classes = if (isPageHeading) "govuk-label--l" else ""
          ))
      ))

    val hint: Option[Hint] = formComponent.helpText.map { ls =>
      Hint(
        content = content.Text(ls.value)
      )
    }

    def dateRangeLabel(optionParams: OptionParams): String =
      messages("generic.From") + " " + TaxPeriodHelper.formatDate(optionParams.fromDate) + " " + messages("generic.to") + " " + TaxPeriodHelper
        .formatDate(optionParams.toDate)

    def renderOption(optionParams: OptionParams, index: Int) = RadioItem(
      id = Some(formComponent.id.value + index),
      value = Some(optionParams.value),
      content = content.Text(dateRangeLabel(optionParams)),
      checked = optionParams.value === setValue
    )

    def renderOptions(optionParams: NonEmptyList[OptionParams]) = {
      val items = optionParams.zipWithIndex.map {
        case (optionParams, index) => renderOption(optionParams, index)
      }

      val radios = Radios(
        idPrefix = Some(formComponent.id.value),
        fieldset = fieldset,
        hint = hint,
        errorMessage = errorMessage,
        name = formComponent.id.value,
        items = items.toList
      )

      new components.govukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)

    }

    val warningText = WarningText(content = content.Text(messages("taxPeriod.noResults.warning")))

    val labelContent =
      if (isPageHeading) {
        content.HtmlContent(s"""<h1 class="govuk-label--l">$label</h1>""")
      } else {
        content.HtmlContent(s"""<p class="govuk-body">$label</p>""")
      }

    maybeTaxPeriodOptions.fold(html.form.snippets.no_open_tax_period(labelContent, warningText))(renderOptions)
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

    val hint = formComponent.helpText.map { ls =>
      Hint(
        content = content.Text(ls.value)
      )
    }

    val errors: Option[String] = {
      val lookup: Map[String, Set[String]] =
        validationResult.map(x => ValidationUtil.renderErrors("", x)).getOrElse(Map.empty)
      ValidationUtil.printErrors(lookup).headOption
    }

    val errorMessage = errors.map(
      error =>
        ErrorMessage(
          content = content.Text(error)
      ))

    val currentValue =
      for {
        vr <- validationResult
        cv <- vr.getCurrentValue.filterNot(_ === "")
      } yield cv

    val labelContent = content.Text(LabelHelper.buildRepeatingLabel(formComponent.label, index).value)

    val isPageHeading = ei.formLevelHeading

    val label = Label(
      isPageHeading = isPageHeading,
      classes = if (isPageHeading) "govuk-label--l" else "",
      content = labelContent
    )

    val fileUpload: fileupload.FileUpload = fileupload.FileUpload(
      id = formComponent.id.value,
      name = formComponent.id.value,
      value = currentValue,
      label = label,
      hint = hint,
      errorMessage = errorMessage,
      attributes = Map(
        "data-form-template-id" -> formTemplateId.value,
        "data-max-file-size-MB" -> ei.formMaxAttachmentSizeMB.toString,
        "data-access-code"      -> ei.maybeAccessCode.fold("-")(_.value)
      )
    )

    val fileInput: Html = new components.govukFileUpload(govukErrorMessage, govukHint, govukLabel)(fileUpload)

    val uploadedFiles: Html =
      html.form.snippets.uploaded_files(ei.maybeAccessCode, formTemplateId, formComponent, currentValue)

    HtmlFormat.fill(List(fileInput, uploadedFiles))

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

    val optionsWithHelpText: NonEmptyList[(SmartString, Option[Html])] =
      optionalHelpText
        .map(_.zipWith(options)((helpText, option) =>
          (option, if (helpText.isEmpty) None else Some(markDownParser(helpText)))))
        .getOrElse(options.map(option => (option, None)))

    val validatedValue = buildFormFieldValidationResult(formComponent, ei, validatedType, data)

    val hint = formComponent.helpText.map { ls =>
      Hint(
        content = content.Text(ls.value)
      )
    }

    val errors: Option[String] = {
      val lookup: Map[String, Set[String]] =
        validatedValue.map(x => ValidationUtil.renderErrors("", x)).getOrElse(Map.empty)
      ValidationUtil.printErrors(lookup).headOption
    }

    val errorMessage = errors.map(
      error =>
        ErrorMessage(
          content = content.Text(error)
      ))

    val isPageHeading = ei.formLevelHeading
    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = content.Text(formComponent.label.value),
            isPageHeading = isPageHeading,
            classes = if (isPageHeading) "govuk-label--l" else ""
          ))
      ))

    def isChecked(index: Int): Boolean =
      validatedValue
        .flatMap(_.getOptionalCurrentValue(formComponent.id.appendIndex(index).value))
        .orElse(prepopValues.find(_ === index.toString))
        .isDefined

    def helpTextHtml(maybeHelpText: Option[Html]): Option[Html] =
      maybeHelpText.map(helpText => html.form.snippets.markdown_wrapper(helpText))

    choice match {
      case Radio | YesNo =>
        val items = optionsWithHelpText.zipWithIndex.map {
          case ((option, maybeHelpText), index) =>
            RadioItem(
              id = Some(formComponent.id.value + index),
              value = Some(index.toString),
              content = content.Text(option.value),
              checked = isChecked(index),
              conditionalHtml = helpTextHtml(maybeHelpText)
            )
        }

        val radios = Radios(
          idPrefix = Some(formComponent.id.value),
          fieldset = fieldset,
          hint = hint,
          errorMessage = errorMessage,
          name = formComponent.id.value,
          items = items.toList,
          classes = if (orientation === Horizontal) "govuk-radios--inline" else ""
        )

        new components.govukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)

      case Checkbox =>
        val items = optionsWithHelpText.zipWithIndex.map {
          case ((option, maybeHelpText), index) =>
            CheckboxItem(
              id = Some(formComponent.id.value + index),
              value = index.toString,
              content = content.Text(option.value),
              checked = isChecked(index),
              conditionalHtml = helpTextHtml(maybeHelpText)
            )
        }

        val checkboxes: Checkboxes = Checkboxes(
          idPrefix = Some(formComponent.id.value),
          fieldset = fieldset,
          hint = hint,
          errorMessage = errorMessage,
          name = formComponent.id.value,
          items = items.toList,
          classes = if (orientation === Horizontal && optionalHelpText.isEmpty) "gform-checkbox--inline" else ""
        )

        new components.govukCheckboxes(govukErrorMessage, govukFieldset, govukHint, govukLabel)(checkboxes)
    }
  }

  private def htmlForRevealingChoice(
    formComponent: FormComponent,
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
    val validatedValue = buildFormFieldValidationResult(formComponent, extraInfo, validatedType, data)
    val nestedEi = extraInfo.copy(formLevelHeading = false)
    val revealingChoicesList: NonEmptyList[(SmartString, Int => Boolean, Option[NonEmptyList[Html]])] =
      options.map { o =>
        val isSelected: Int => Boolean =
          index =>
            extraInfo.fieldData.data
              .get(formComponent.id)
              .fold(o.selected)(_.contains(index.toString))

        val revealingFieldsHtml = o.revealingFields
          .filterNot(_.onlyShowOnSummary)
          .map(htmlFor(_, formTemplateId, index, nestedEi, data, validatedType, obligations = obligations))

        val maybeRevealingFieldsHtml = revealingFieldsHtml match {
          case x :: xs => Some(NonEmptyList(x, xs))
          case Nil     => None
        }

        (o.choice, isSelected, maybeRevealingFieldsHtml)
      }

    val hint = formComponent.helpText.map { ls =>
      Hint(
        content = content.Text(ls.value)
      )
    }

    val errors: Option[String] = {
      val lookup: Map[String, Set[String]] =
        validatedValue.map(x => ValidationUtil.renderErrors("", x)).getOrElse(Map.empty)
      ValidationUtil.printErrors(lookup).headOption
    }

    val errorMessage = errors.map(
      error =>
        ErrorMessage(
          content = content.Text(error)
      ))

    val isPageHeading = extraInfo.formLevelHeading
    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = content.Text(formComponent.label.value),
            isPageHeading = isPageHeading,
            classes = if (isPageHeading) "govuk-label--l" else ""
          ))
      ))

    def revealingFieldsHtml(maybeRevealingFieldsHtml: Option[NonEmptyList[Html]]): Option[Html] =
      maybeRevealingFieldsHtml.map(htmls => html.form.snippets.markdown_wrapper(HtmlFormat.fill(htmls.toList)))

    if (multiValue) {
      val items = revealingChoicesList.zipWithIndex.map {
        case ((option, isChecked, maybeRevealingFieldsHtml), index) =>
          CheckboxItem(
            id = Some(formComponent.id.value + index),
            value = index.toString,
            content = content.Text(option.value),
            checked = isChecked(index),
            conditionalHtml = revealingFieldsHtml(maybeRevealingFieldsHtml)
          )
      }

      val checkboxes = Checkboxes(
        idPrefix = Some(formComponent.id.value),
        fieldset = fieldset,
        hint = hint,
        errorMessage = errorMessage,
        name = formComponent.id.value,
        items = items.toList
      )

      new components.govukCheckboxes(govukErrorMessage, govukFieldset, govukHint, govukLabel)(checkboxes)
    } else {

      val items = revealingChoicesList.zipWithIndex.map {
        case ((option, isChecked, maybeRevealingFieldsHtml), index) =>
          RadioItem(
            id = Some(formComponent.id.value + index),
            value = Some(index.toString),
            content = content.Text(option.value),
            checked = isChecked(index),
            conditionalHtml = revealingFieldsHtml(maybeRevealingFieldsHtml)
          )
      }

      val radios = Radios(
        idPrefix = Some(formComponent.id.value),
        fieldset = fieldset,
        hint = hint,
        errorMessage = errorMessage,
        name = formComponent.id.value,
        items = items.toList
      )

      new components.govukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)
    }

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

    val labelString = LabelHelper.buildRepeatingLabel(fieldValue.label, index).value
    val isPageHeading = ei.formLevelHeading

    val errors: Option[String] = {
      val lookup: Map[String, Set[String]] =
        validatedValue.map(x => ValidationUtil.renderErrors("", x)).getOrElse(Map.empty)
      ValidationUtil.printErrors(lookup).headOption
    }

    val hiddenClass =
      if (fieldValue.derived && !fieldValue.presentationHint.exists(_.contains(TotalValue)))
        "govuk-visually-hidden"
      else
        ""

    val errorMessage = errors.map(
      error =>
        ErrorMessage(
          content = content.Text(error),
          classes = hiddenClass
      ))

    val label = Label(
      forAttr = Some(fieldValue.id.value),
      isPageHeading = isPageHeading,
      classes = if (isPageHeading) s"govuk-label--l $hiddenClass" else hiddenClass,
      content = content.Text(labelString)
    )

    val hint: Option[Hint] = fieldValue.helpText.map { ls =>
      Hint(
        content = content.Text(ls.value)
      )
    }

    if (isHidden)
      html.form.snippets
        .hidden_field_populated(List(FormRender(fieldValue.id.value, fieldValue.id.value, prepopValue.getOrElse(""))))
    else
      lookupRegistry.get(register) match {
        case None => Html("") // Ups
        case Some(AjaxLookup(_, _, showAll)) =>
          html.form.snippets.lookup_autosuggest(
            label,
            fieldValue,
            showAll,
            register,
            ei.formTemplate._id,
            prepopValue,
            validatedValue,
            hint,
            errorMessage
          )
        case Some(RadioLookup(options)) =>
          val isPageHeading = ei.formLevelHeading
          val fieldset = Some(
            Fieldset(
              legend = Some(
                Legend(
                  content = content.Text(fieldValue.label.value),
                  isPageHeading = isPageHeading,
                  classes = if (isPageHeading) s"govuk-label--l $hiddenClass" else hiddenClass
                ))
            ))

          val currentValue = validatedValue.flatMap(_.getCurrentValue)

          val selectedValue = prepopValue.orElse(currentValue).getOrElse("")

          def renderOption(lookupLabel: LookupLabel, index: Int) = RadioItem(
            id = Some(fieldValue.id.value + index),
            value = Some(lookupLabel.label),
            content = content.Text(lookupLabel.label),
            checked = lookupLabel.label === selectedValue
          )

          val lookupLabels: List[LookupLabel] = options.process(_.sortLookupByIdx).map(_._1)

          val items = lookupLabels.zipWithIndex.map {
            case (lookupLabel, index) => renderOption(lookupLabel, index)
          }

          val radios = Radios(
            idPrefix = Some(fieldValue.id.value),
            fieldset = fieldset,
            hint = hint,
            errorMessage = errorMessage,
            classes = hiddenClass,
            name = fieldValue.id.value,
            items = items.toList
          )

          new components.govukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)

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

      val isPageHeading = ei.formLevelHeading

      val label = Label(
        isPageHeading = isPageHeading,
        classes = if (isPageHeading) "govuk-label--l" else "",
        content = labelContent
      )

      val govukTextarea = new components.govukTextarea(govukErrorMessage, govukHint, govukLabel)

      val attributes =
        if (formComponent.editable)
          Map.empty[String, String]
        else
          Map("readonly" -> "")

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
            classes = sizeClasses,
            attributes = attributes
          )

          new components.govukCharacterCount(govukTextarea, govukHint)(characterCount)

        case _ =>
          val textArea = Textarea(
            id = formComponent.id.value,
            name = formComponent.id.value,
            label = label,
            hint = hint,
            value = maybeCurrentValue,
            errorMessage = errorMessage,
            classes = sizeClasses,
            attributes = attributes
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
          val sizeClasses = TextConstraint.getSizeClass(text.constraint, text.displayWidth)

          val hiddenClass =
            if (formComponent.derived)
              "govuk-visually-hidden"
            else
              ""

          val hiddenErrorMessage = errorMessage.map(e => e.copy(classes = e.classes + hiddenClass))

          val isPageHeading = ei.formLevelHeading
          val label = Label(
            isPageHeading = isPageHeading,
            classes = if (isPageHeading) s"govuk-label--l $hiddenClass" else hiddenClass,
            content = labelContent
          )

          val attributes =
            if (formComponent.editable)
              Map.empty[String, String]
            else
              Map("readonly" -> "")

          if (formComponent.isSterling) {
            val currencyInput = CurrencyInput(
              id = formComponent.id.value,
              name = formComponent.id.value,
              label = label,
              hint = hint,
              value = maybeCurrentValue,
              errorMessage = hiddenErrorMessage,
              classes = s"$hiddenClass $sizeClasses",
              attributes = attributes
            )

            new hmrcCurrencyInput(govukErrorMessage, govukHint, govukLabel)(currencyInput)

          } else {
            val input = Input(
              id = formComponent.id.value,
              name = formComponent.id.value,
              label = label,
              hint = hint,
              value = maybeCurrentValue,
              errorMessage = hiddenErrorMessage,
              classes = s"$hiddenClass $sizeClasses",
              attributes = attributes
            )
            val govukInput: Html = new components.govukInput(govukErrorMessage, govukHint, govukLabel)(input)

            maybeUnit.fold(govukInput)(GovukExtensions.insertUnit(govukInput))

          }

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
      val validatedValue = buildFormFieldValidationResult(formComponent, ei, validatedType, data)

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

      def safeId(id: String): String =
        formComponent.id.withSuffix(id).toString

      val hasErrors = validatedValue.exists(_.isNotOk)

      val inputClasses = if (hasErrors) "govuk-input--error" else ""

      val attributes =
        if (formComponent.editable)
          Map.empty[String, String]
        else
          Map("readonly" -> "")

      val items = Seq(
        InputItem(
          id = s"${formComponent.id.value}-day",
          name = s"${formComponent.id.value}-day",
          label = Some(messages("date.Day")),
          value = validatedValue
            .flatMap(_.getOptionalCurrentValue(safeId("day")))
            .orElse(prepopValues.map(_.day.toString)),
          classes = s"$inputClasses govuk-input--width-2",
          attributes = attributes
        ),
        InputItem(
          id = s"${formComponent.id.value}-month",
          name = s"${formComponent.id.value}-month",
          label = Some(messages("date.Month")),
          value = Some(
            validatedValue
              .flatMap(_.getOptionalCurrentValue(safeId("month")))
              .orElse(prepopValues.map(_.month.toString))
              .getOrElse("")),
          classes = s"$inputClasses govuk-input--width-2",
          attributes = attributes
        ),
        InputItem(
          id = s"${formComponent.id.value}-year",
          name = s"${formComponent.id.value}-year",
          label = Some(messages("date.Year")),
          value = Some(
            validatedValue
              .flatMap(_.getOptionalCurrentValue(safeId("year")))
              .orElse(prepopValues.map(_.year.toString))
              .getOrElse("")),
          classes = s"$inputClasses govuk-input--width-4",
          attributes = attributes
        )
      )

      val isPageHeading = ei.formLevelHeading

      val fieldset = Fieldset(
        legend = Some(
          Legend(
            content = content.Text(formComponent.label.value),
            classes = if (isPageHeading) "govuk-label--l" else "",
            isPageHeading = isPageHeading
          ))
      )

      val dateInput = DateInput(
        id = formComponent.id.value,
        items = items,
        hint = hint,
        errorMessage = errorMessage,
        fieldset = Some(fieldset)
      )

      new components.govukDateInput(govukErrorMessage, govukHint, govukFieldset, govukInput)(dateInput)
    }
  }

  private def htmlForGroup(
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
      formComponent.helpText.map(markDownParser).map(markDown => Hint(content = content.HtmlContent(markDown)))

    val (lhtml, limitReached) =
      getGroupForRendering(formComponent, formTemplateId, groupField, validatedType, ei, data, obligations)

    html.form.snippets.group(formComponent, maybeHint, groupField, lhtml, limitReached, index)
  }

  private def renderTime(
    time: Time,
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

      val hiddenClass =
        if (formComponent.derived)
          "govuk-visually-hidden"
        else
          ""

      val hiddenErrorMessage = errorMessage.map(e => e.copy(classes = e.classes + hiddenClass))

      val isPageHeading = ei.formLevelHeading
      val label = Label(
        isPageHeading = isPageHeading,
        classes = if (isPageHeading) s"govuk-label--l $hiddenClass" else hiddenClass,
        content = labelContent
      )

      val attributes =
        if (formComponent.editable)
          Map.empty[String, String]
        else
          Map("readonly" -> "")

      val maybeCurrentValue = prepopValue.orElse(validatedValue.flatMap(_.getCurrentValue)).getOrElse("")

      val emptySelectItem = SelectItem(
        value = Some(""),
        text = "",
        selected = "" === maybeCurrentValue
      )

      val selectItems = Range.timeSlots(time) map { t =>
        SelectItem(
          value = Some(t),
          text = t,
          selected = t === maybeCurrentValue
        )
      }

      val select = Select(
        id = formComponent.id.value,
        name = formComponent.id.value,
        items = emptySelectItem +: selectItems,
        label = label,
        hint = hint,
        errorMessage = hiddenErrorMessage,
        classes = s"$hiddenClass",
        attributes = attributes
      )

      new components.govukSelect(govukErrorMessage, govukHint, govukLabel)(select)
    }
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

            val instance = count + 1

            val label =
              if (groupField.repeatLabel.isDefined) GroupHelper.buildRepeatLabel(groupField, instance).value else ""

            val removeButtonHtml = html.form.snippets.delete_group_link(formComponent.id, label, instance, showButton)
            val dividerHtml = html.form.snippets.divider()

            val fieldSet = Fieldset(
              legend = Some(
                Legend(
                  content = content.Text(label),
                  classes = "govuk-label--m"
                )
              ),
              html = HtmlFormat.fill(lhtml ++ List(removeButtonHtml, dividerHtml))
            )

            govukFieldset(fieldSet)

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

  private def shouldDisplayHeading(section: Section)(implicit l: LangADT, sse: SmartStringEvaluator): Boolean =
    section.fields match {
      case IsGroup(g) :: _              => false
      case IsInformationMessage(_) :: _ => false
      case formComponent :: IsNilOrInfoOnly() =>
        formComponent.editable && formComponent.label.value() === section.title.value()
      case _ => false
    }

  private val govukErrorMessage: components.govukErrorMessage = new components.govukErrorMessage()
  private val govukFieldset: components.govukFieldset = new components.govukFieldset()
  private val govukHint: components.govukHint = new components.govukHint()
  private val govukLabel: components.govukLabel = new components.govukLabel()
  private val govukInput: components.govukInput = new components.govukInput(govukErrorMessage, govukHint, govukLabel)

}

object IsNilOrInfoOnly {
  def unapply(xs: List[FormComponent]): Boolean =
    xs match {
      case Nil                                                      => true
      case IsInformationMessage(_) :: tail                          => unapply(tail)
      case head :: tail if head.onlyShowOnSummary || !head.editable => unapply(tail)
      case _                                                        => false
    }
}
