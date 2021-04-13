/*
 * Copyright 2021 HM Revenue & Customs
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

import java.time.LocalDate
import cats.data.NonEmptyList
import cats.instances.int._
import cats.instances.string._
import cats.syntax.eq._
import org.jsoup.Jsoup
import shapeless.syntax.typeable._
import play.api.i18n.Messages
import play.api.mvc.{ Request, RequestHeader }
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.auth.core.AffinityGroup.Individual
import uk.gov.hmrc.auth.core.Enrolments
import uk.gov.hmrc.gform.auth.models.{ AuthenticatedRetrievals, GovernmentGatewayId, MaterialisedRetrievals }
import uk.gov.hmrc.gform.commons.MarkDownUtil.markDownParser
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.{ GformFlashKeys, Origin, SaveAndContinue }
import uk.gov.hmrc.gform.fileupload.routes.FileUploadController
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.gform.handlers.FormHandlerResult
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.models.{ AddToListSummaryRecord, Atom, Bracket, DataExpanded, DateExpr, FastForward, FormModel, PageModel, Repeater, SectionRenderingInformation, Singleton }
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.javascript.JavascriptMaker
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelRenderPageOptics }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.config.{ ContentType, FileExtension }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.gform.lookup.LookupOptions.filterBySelectionCriteria
import uk.gov.hmrc.gform.ops.FormComponentOps
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations._
import uk.gov.hmrc.gform.validation.HtmlFieldId
import uk.gov.hmrc.gform.validation._
import uk.gov.hmrc.gform.views.summary.TextFormatter
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.gform.views.html.specimen
import uk.gov.hmrc.gform.views.components.TotalText
import uk.gov.hmrc.govukfrontend.views.html.components
import uk.gov.hmrc.govukfrontend.views.viewmodels.button.Button
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
import uk.gov.hmrc.govukfrontend.views.viewmodels.input.{ Input, PrefixOrSuffix }
import uk.gov.hmrc.govukfrontend.views.viewmodels.label.Label
import uk.gov.hmrc.govukfrontend.views.viewmodels.dateinput.InputItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.{ RadioItem, Radios }
import uk.gov.hmrc.govukfrontend.views.viewmodels.select.{ Select, SelectItem }
import uk.gov.hmrc.govukfrontend.views.viewmodels.textarea.Textarea
import uk.gov.hmrc.govukfrontend.views.viewmodels.warningtext.WarningText
import uk.gov.hmrc.hmrcfrontend.views.html.components.hmrcCurrencyInput
import uk.gov.hmrc.hmrcfrontend.views.viewmodels.currencyinput.CurrencyInput

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
class SectionRenderingService(frontendAppConfig: FrontendAppConfig, lookupRegistry: LookupRegistry) {

  case class ExtraInfo(
    singleton: Singleton[DataExpanded],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    formTemplate: FormTemplate,
    envelopeId: EnvelopeId,
    envelope: EnvelopeWithMapping,
    formMaxAttachmentSizeMB: Int,
    retrievals: MaterialisedRetrievals,
    formLevelHeading: Boolean,
    specialAttributes: Map[String, String]
  ) {
    private val modelComponentIds: List[ModelComponentId] =
      singleton.allFormComponents.flatMap(_.multiValueId.toModelComponentIds)

    val valueLookup: Map[ModelComponentId, Option[VariadicValue]] =
      modelComponentIds
        .map(modelComponentId => (modelComponentId, formModelOptics.pageOpticsData.get(modelComponentId)))
        .toMap

  }

  def renderAddToList(
    repeater: Repeater[DataExpanded],
    bracket: Bracket.AddToList[DataExpanded],
    formModel: FormModel[DataExpanded],
    maybeAccessCode: Option[AccessCode],
    form: Form,
    sectionNumber: SectionNumber,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    formTemplate: FormTemplate,
    validationResult: ValidationResult,
    retrievals: MaterialisedRetrievals
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html = {

    val listResult = validationResult.formFieldValidationResults
    val pageLevelErrorHtml = generatePageLevelErrorHtml(listResult, List.empty)
    val actionForm = uk.gov.hmrc.gform.gform.routes.FormController
      .updateFormData(formTemplate._id, maybeAccessCode, sectionNumber, FastForward.Yes, SaveAndContinue)

    val formComponent = repeater.addAnotherQuestion

    val descriptions: NonEmptyList[SmartString] = bracket.repeaters.map(_.expandedDescription)

    val recordTable: NonEmptyList[AddToListSummaryRecord] = descriptions.zipWithIndex.map { case (description, index) =>
      val html = markDownParser(description)
      AddToListSummaryRecord(html, index, Jsoup.parse(html.body).text())
    }

    val choice = formComponent.`type`.cast[Choice].get

    val formFieldValidationResult = validationResult(formComponent)
    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage: Option[ErrorMessage] = errors.map(error =>
      ErrorMessage(
        content = content.Text(error)
      )
    )

    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = content.Text(formComponent.label.value)
          )
        )
      )
    )

    def isChecked(index: Int): Boolean =
      formFieldValidationResult
        .getOptionalCurrentValue(HtmlFieldId.indexed(formComponent.id, index))
        .isDefined

    val items = choice.options.zipWithIndex.map { case (option, index) =>
      RadioItem(
        id = Some(formComponent.id.value + index),
        value = Some(index.toString),
        content = content.Text(option.value),
        checked = isChecked(index),
        attributes = dataLabelAttribute(option)
      )
    }

    val radios = Radios(
      idPrefix = Some(formComponent.id.value),
      fieldset = fieldset,
      errorMessage = errorMessage,
      name = formComponent.id.value,
      items = items.toList
    )

    val addAnotherQuestion: Html =
      new components.govukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)

    val shouldDisplayBack: Boolean =
      Origin(DataOrigin.unSwapDataOrigin(formModelOptics))
        .filteredSectionNumbers(sectionNumber)
        .sorted
        .exists(_ < sectionNumber)

    html.form.addToList(
      repeater,
      bracket,
      formTemplate,
      recordTable,
      pageLevelErrorHtml,
      frontendAppConfig,
      actionForm,
      retrievals.renderSaveAndComeBackLater,
      retrievals.continueLabelKey,
      shouldDisplayBack,
      addAnotherQuestion,
      specimenNavigation(formTemplate, sectionNumber, formModelOptics.formModelRenderPageOptics),
      maybeAccessCode,
      sectionNumber
    )
  }

  def renderSection(
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    formHandlerResult: FormHandlerResult,
    formTemplate: FormTemplate,
    envelopeId: EnvelopeId,
    singleton: Singleton[DataExpanded],
    formMaxAttachmentSizeMB: Int,
    contentTypes: List[ContentType],
    restrictedFileExtensions: List[FileExtension],
    retrievals: MaterialisedRetrievals,
    obligations: Obligations,
    fastForward: FastForward,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html = {

    val FormHandlerResult(validationResult, envelope) = formHandlerResult

    val formLevelHeading = shouldDisplayHeading(singleton)

    val ei = ExtraInfo(
      singleton,
      maybeAccessCode,
      sectionNumber,
      formModelOptics,
      formTemplate,
      envelopeId,
      envelope,
      formMaxAttachmentSizeMB,
      retrievals,
      formLevelHeading,
      specialAttributes = Map.empty
    )
    val actionForm = uk.gov.hmrc.gform.gform.routes.FormController
      .updateFormData(formTemplate._id, maybeAccessCode, sectionNumber, fastForward, SaveAndContinue)

    val page = singleton.page

    val listResult = validationResult.formFieldValidationResults(singleton)

    val javascript =
      JavascriptMaker.generateJs(sectionNumber, formModelOptics)

    val pageLevelErrorHtml = request.flash.get(GformFlashKeys.FileUploadError) match {
      case Some(message) => noJSFileUploadError(message, request.flash.get(GformFlashKeys.FileUploadFileId))
      case None          => generatePageLevelErrorHtml(listResult, List.empty)
    }

    val originSection = Origin(DataOrigin.unSwapDataOrigin(formModelOptics)).minSectionNumber
    val renderUnits: List[RenderUnit] = page.renderUnits
    val snippetsForFields = renderUnits
      .map(renderUnit => htmlFor(renderUnit, formTemplate._id, ei, validationResult, obligations))
    val renderingInfo = SectionRenderingInformation(
      formTemplate._id,
      maybeAccessCode,
      sectionNumber,
      page.title.value,
      page.description.map(ls => ls.value),
      snippetsForFields,
      javascript,
      envelopeId,
      actionForm,
      retrievals.renderSaveAndComeBackLater && page.continueIf.fold(true)(_ === Continue),
      page.continueLabel.map(ls => ls.value).getOrElse(messages(retrievals.continueLabelKey)),
      formMaxAttachmentSizeMB,
      contentTypes,
      restrictedFileExtensions,
      page.progressIndicator.map(ls => ls.value)
    )
    html.form.form(
      formTemplate,
      pageLevelErrorHtml,
      renderingInfo,
      shouldDisplayBack = sectionNumber > originSection,
      shouldDisplayHeading = !formLevelHeading,
      shouldDisplayContinue = !page.isTerminationPage,
      frontendAppConfig,
      specimenNavigation = specimenNavigation(formTemplate, sectionNumber, formModelOptics.formModelRenderPageOptics),
      isDeclaration = false,
      maybeAccessCode,
      sectionNumber,
      fastForward
    )

  }

  private def specimenNavigation(
    formTemplate: FormTemplate,
    sectionNumber: SectionNumber,
    formModelRenderPageOptics: FormModelRenderPageOptics[DataOrigin.Mongo]
  )(implicit
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html =
    if (formTemplate.isSpecimen) {
      val pages: NonEmptyList[(PageModel[DataExpanded], SectionNumber)] =
        formModelRenderPageOptics.formModel.pagesWithIndex
      specimen.navigation(formTemplate, sectionNumber, pages.toList)
    } else HtmlFormat.empty

  private val toErrorLink: PartialFunction[(HtmlFieldId, FormFieldValidationResult), ErrorLink] = {
    case (multiFieldId, ffvr) if ffvr.isNotOk =>
      ErrorLink(
        href = Some("#" + multiFieldId.toHtmlId),
        content = content.Text(ffvr.fieldErrors.headOption.getOrElse(""))
      )
  }

  private def addressFieldSorted(
    fields: NonEmptyList[ModelComponentId.Atomic],
    data: Map[HtmlFieldId, FormFieldValidationResult]
  ) =
    // We need to sort errors based on elements position on screen
    fields.toList
      .flatMap { modelComponentId =>
        val multiFieldId = HtmlFieldId.pure(modelComponentId)
        data.get(multiFieldId).map(multiFieldId -> _)
      }
      .collect(toErrorLink)

  private def noJSFileUploadError(message: String, fileId: Option[String])(implicit messages: Messages): HasErrors = {
    val errorSummary = ErrorSummary(
      errorList = List(
        ErrorLink(href = fileId.map("#" + _), content = content.Text(message))
      ),
      title = content.Text(messages("error.summary.heading"))
    )

    val errorHtml: Html = new components.govukErrorSummary()(errorSummary)

    Errors(errorHtml)
  }

  private def generatePageLevelErrorHtml(
    listValidation: List[FormFieldValidationResult],
    globalErrors: List[ErrorLink]
  )(implicit
    messages: Messages
  ): HasErrors = {

    val errorsHtml: List[ErrorLink] = globalErrors ++ listValidation
      .filter(_.isNotOk)
      .flatMap { formFieldValidationResult =>
        formFieldValidationResult match {
          case ComponentField(formComponent @ IsAddress(_), data) =>
            addressFieldSorted(Address.fields(formComponent.modelComponentId.indexedComponentId), data)
          case ComponentField(formComponent @ IsOverseasAddress(overseasAddress), data) =>
            addressFieldSorted(OverseasAddress.fields(formComponent.modelComponentId.indexedComponentId), data)
          case ComponentField(_, data) => data.toList.collectFirst(toErrorLink)
          case otherwise =>
            otherwise.fieldErrors
              .map { errorMessage =>
                val formComponent = otherwise.formComponent
                val multiFieldId =
                  otherwise.formComponent match {
                    case IsChoice(_) | IsRevealingChoice(_) => HtmlFieldId.indexed(formComponent.id, 0)
                    case _                                  => HtmlFieldId.pure(formComponent.modelComponentId)
                  }
                ErrorLink(
                  href = Some("#" + multiFieldId.toHtmlId),
                  content = content.Text(errorMessage)
                )
              }
        }
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

  def renderDeclarationSection(
    maybeAccessCode: Option[AccessCode],
    form: Form,
    formTemplate: FormTemplate,
    singleton: Singleton[DataExpanded],
    retrievals: MaterialisedRetrievals,
    validationResult: ValidationResult,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html = {

    val ei = ExtraInfo(
      singleton,
      maybeAccessCode,
      SectionNumber(0),
      formModelOptics,
      formTemplate,
      EnvelopeId(""),
      EnvelopeWithMapping.empty,
      0,
      retrievals,
      formLevelHeading = false,
      specialAttributes = Map.empty
    )

    val declarationPage = singleton.page

    val continueLabel = declarationPage.continueLabel.map(_.value()).getOrElse {
      formTemplate.formCategory match {
        case HMRCReturnForm => messages("button.acceptAndSubmitForm", messages("formCategory.return"))
        case HMRCClaimForm  => messages("button.acceptAndSubmitForm", messages("formCategory.claim"))
        case _              => messages("button.acceptAndSubmit")
      }
    }

    val listResult = validationResult.formFieldValidationResults(singleton)
    val snippets = declarationPage.renderUnits.map(renderUnit =>
      htmlFor(renderUnit, formTemplate._id, ei, validationResult, obligations = NotChecked)
    )
    val pageLevelErrorHtml = generatePageLevelErrorHtml(listResult, List.empty)
    val renderingInfo = SectionRenderingInformation(
      formTemplate._id,
      maybeAccessCode,
      SectionNumber(0),
      declarationPage.title.value,
      declarationPage.description.map(ls => ls.value),
      snippets,
      "",
      EnvelopeId(""),
      uk.gov.hmrc.gform.gform.routes.DeclarationController
        .submitDeclaration(formTemplate._id, maybeAccessCode, uk.gov.hmrc.gform.controllers.Continue),
      false,
      continueLabel,
      0,
      Nil,
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
      isDeclaration = true,
      maybeAccessCode = maybeAccessCode,
      sectionNumber = SectionNumber(0),
      fastForward = FastForward.Yes
    )
  }

  def renderPrintSection(
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate,
    destinationPrint: DestinationPrint
  )(implicit request: Request[_], messages: Messages, l: LangADT, sse: SmartStringEvaluator): Html = {
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
    envelopeId: EnvelopeId,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html = {
    val ackSection = destinationList.acknowledgementSection.toSection
    val ei = ExtraInfo(
      Singleton(ackSection.page.asInstanceOf[Page[DataExpanded]]),
      maybeAccessCode,
      SectionNumber(0),
      formModelOptics,
      formTemplate,
      envelopeId,
      EnvelopeWithMapping.empty,
      0,
      retrievals,
      formLevelHeading = false,
      specialAttributes = Map.empty
    )

    val htmlContent: Content =
      if (destinationList.acknowledgementSection.showReference) {
        HtmlContent(
          uk.gov.hmrc.gform.views.html.hardcoded.pages.partials.submission_reference(SubmissionRef(envelopeId))
        )
      } else {
        Empty
      }

    val formCategory = formTemplate.formCategory
    val snippets = destinationList.acknowledgementSection.toPage.renderUnits.map(renderUnit =>
      htmlFor(renderUnit, formTemplate._id, ei, ValidationResult.empty, obligations = NotChecked)
    )
    val renderingInfo = SectionRenderingInformation(
      formTemplate._id,
      maybeAccessCode,
      SectionNumber(0),
      destinationList.acknowledgementSection.title.value,
      destinationList.acknowledgementSection.description.map(ls => ls.value),
      snippets,
      "",
      envelopeId,
      uk.gov.hmrc.gform.gform.routes.DeclarationController
        .submitDeclaration(formTemplate._id, maybeAccessCode, uk.gov.hmrc.gform.controllers.Continue),
      false,
      "Confirm and send",
      0,
      Nil,
      Nil
    )
    uk.gov.hmrc.gform.views.html.hardcoded.pages.partials
      .acknowledgement(renderingInfo, htmlContent, formCategory, formTemplate, frontendAppConfig)
  }

  def renderEnrolmentSection(
    formTemplate: FormTemplate,
    singleton: Singleton[DataExpanded],
    retrievals: MaterialisedRetrievals,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    globalErrors: List[ErrorLink],
    validationResult: ValidationResult
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html = {

    val maybeAccessCode = None
    val ei = ExtraInfo(
      singleton,
      maybeAccessCode,
      SectionNumber(0),
      formModelOptics,
      formTemplate,
      EnvelopeId(""),
      EnvelopeWithMapping.empty,
      0,
      emptyRetrievals,
      formLevelHeading = false,
      specialAttributes = Map.empty
    )
    val page = singleton.page
    val listResult = validationResult.formFieldValidationResults(singleton)
    val snippets =
      page.renderUnits.map { renderUnit =>
        htmlFor(renderUnit, formTemplate._id, ei, validationResult, obligations = NotChecked)
      }
    val pageLevelErrorHtml = generatePageLevelErrorHtml(listResult, globalErrors)
    val renderingInfo = SectionRenderingInformation(
      formTemplate._id,
      maybeAccessCode,
      SectionNumber(0),
      page.title.value,
      None,
      snippets,
      "",
      EnvelopeId(""),
      uk.gov.hmrc.gform.gform.routes.EnrolmentController
        .submitEnrolment(formTemplate._id, uk.gov.hmrc.gform.controllers.Continue),
      false,
      messages("button.confirmAndSend"),
      0,
      Nil,
      Nil
    )
    html.form
      .form(
        formTemplate,
        pageLevelErrorHtml,
        renderingInfo,
        false,
        true,
        true,
        frontendAppConfig,
        maybeAccessCode = maybeAccessCode,
        sectionNumber = SectionNumber(0),
        fastForward = FastForward.Yes
      )
  }

  private def htmlFor(
    renderUnit: RenderUnit,
    formTemplateId: FormTemplateId,
    ei: ExtraInfo,
    validationResult: ValidationResult,
    obligations: Obligations
  )(implicit
    request: RequestHeader,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html =
    renderUnit.fold { case RenderUnit.Pure(formComponent) =>
      val isVisible = formComponent.includeIf.fold(true) { includeIf =>
        ei.formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None)
      }

      if (!isVisible || formComponent.onlyShowOnSummary) {
        HtmlFormat.empty
      } else {

        formComponent.`type` match {
          case UkSortCode(_) =>
            htmlForSortCode(formComponent, validationResult, ei)
          case Group(_, _, _, _, _) =>
            throw new IllegalArgumentException(s"Group '${formComponent.id}' cannot be rendered as RenderUnit.Pure")
          case Date(_, offset, dateValue) =>
            htmlForDate(formComponent, offset, dateValue, validationResult, ei)
          case CalendarDate =>
            htmlForCalendarDate(formComponent, validationResult, ei)
          case t @ Time(_, _) =>
            renderTime(t, formComponent, validationResult, ei)
          case Address(international) => htmlForAddress(formComponent, international, validationResult, ei)
          case o @ OverseasAddress(_, _, _) =>
            htmlForOverseasAddress(formComponent, o, validationResult, ei)
          case Text(Lookup(register, _), _, _, _, _, _) =>
            renderLookup(formComponent, register, validationResult, ei)
          case t @ Text(_, _, _, _, _, _) =>
            renderText(t, formComponent, validationResult, ei)
          case t @ TextArea(_, _, _, _, _) =>
            renderTextArea(t, formComponent, validationResult, ei)
          case Choice(choice, options, orientation, selections, hints, optionalHelpText) =>
            htmlForChoice(
              formComponent,
              choice,
              options,
              orientation,
              selections,
              hints,
              optionalHelpText,
              validationResult,
              ei
            )
          case RevealingChoice(options, multiValue) =>
            htmlForRevealingChoice(
              formComponent,
              formTemplateId,
              multiValue,
              options,
              validationResult,
              ei,
              obligations
            )
          case FileUpload() =>
            htmlForFileUpload(formComponent, formTemplateId, ei, validationResult)
          case InformationMessage(infoType, infoText) =>
            htmlForInformationMessage(formComponent, infoType, infoText, ei)
          case htp @ HmrcTaxPeriod(idType, idNumber, regimeType) =>
            htmlForHmrcTaxPeriod(formComponent, ei, validationResult, obligations, htp)
        }
      }
    } { case r @ RenderUnit.Group(_, _) =>
      htmlForGroup(r, formTemplateId, ei, validationResult, obligations)
    }

  private def htmlForHmrcTaxPeriod(
    formComponent: FormComponent,
    ei: ExtraInfo,
    validationResult: ValidationResult,
    obligations: Obligations,
    hmrcTP: HmrcTaxPeriod
  )(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) = {

    val maybeTaxPeriodOptions: Option[NonEmptyList[OptionParams]] = obligations match {
      case RetrievedObligations(listOfObligations) =>
        val params: Option[List[OptionParams]] = listOfObligations
          .find(_.id.recalculatedTaxPeriodKey.hmrcTaxPeriod === hmrcTP)
          .map(
            _.obligation.obligations.flatMap(
              _.obligationDetails.map(od =>
                OptionParams(od.periodKey, od.inboundCorrespondenceFromDate, od.inboundCorrespondenceToDate, false)
              )
            )
          )

        params match {
          case Some(x :: xs) => Some(NonEmptyList(x, xs))
          case _             => None
        }
      case _ => None
    }

    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)

    val setValue = TaxPeriodHelper.formatTaxPeriodOutput(formFieldValidationResult, ei.envelope)
    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage = errors.map(error =>
      ErrorMessage(
        content = content.Text(error)
      )
    )

    val label = formComponent.label.value

    val isPageHeading = ei.formLevelHeading

    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = content.Text(label),
            isPageHeading = isPageHeading,
            classes = if (isPageHeading) "govuk-label--l" else ""
          )
        )
      )
    )

    val hint: Option[Hint] = formComponent.helpText.map { ls =>
      Hint(
        content = content.Text(ls.value)
      )
    }

    def dateRangeLabel(optionParams: OptionParams): String =
      messages("generic.From") + " " + TaxPeriodHelper.formatDate(optionParams.fromDate) + " " + messages(
        "generic.to"
      ) + " " + TaxPeriodHelper
        .formatDate(optionParams.toDate)

    def renderOption(optionParams: OptionParams, index: Int) = RadioItem(
      id = Some(formComponent.id.value + index),
      value = Some(optionParams.value),
      content = content.Text(dateRangeLabel(optionParams)),
      checked = optionParams.value === setValue,
      attributes = dataLabelAttribute(dateRangeLabel(optionParams))
    )

    def renderOptions(optionParams: NonEmptyList[OptionParams]) = {
      val items = optionParams.zipWithIndex.map { case (optionParams, index) =>
        renderOption(optionParams, index)
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
    ei: ExtraInfo
  )(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) =
    html.form.snippets.field_template_info(formComponent, infoType, markDownParser(infoText))

  private def htmlForFileUpload(
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    ei: ExtraInfo,
    validationResult: ValidationResult
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ) = {

    val formFieldValidationResult = validationResult(formComponent)

    val hint = formComponent.helpText.map { ls =>
      Hint(
        content = content.Text(ls.value)
      )
    }

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage = errors.map(error =>
      ErrorMessage(
        content = content.Text(error)
      )
    )

    val currentValue = formFieldValidationResult.getCurrentValue.filterNot(_ === "")

    val labelContent = content.Text(formComponent.label.value)

    val isPageHeading = ei.formLevelHeading

    val label = Label(
      isPageHeading = isPageHeading,
      classes = if (isPageHeading) "govuk-label--l" else "",
      content = labelContent
    )

    val fileId: FileId =
      ei.envelope
        .find(formComponent.modelComponentId)
        .map(_.fileId)
        .getOrElse(ei.envelope.mapping.fileIdFor(formComponent.id))

    val fileUpload: fileupload.FileUpload = fileupload.FileUpload(
      id = formComponent.id.value,
      name = formComponent.id.value,
      label = label,
      hint = hint,
      errorMessage = errorMessage,
      attributes = Map(
        "data-file-id"          -> fileId.value,
        "data-form-template-id" -> formTemplateId.value,
        "data-max-file-size-MB" -> ei.formMaxAttachmentSizeMB.toString,
        "data-access-code"      -> ei.maybeAccessCode.fold("-")(_.value)
      )
    )

    val successUrl =
      frontendAppConfig.gformFrontendBaseUrl + FileUploadController.noJsSuccessCallback(
        formTemplateId,
        ei.sectionNumber,
        ei.maybeAccessCode,
        formComponent.id,
        fileId
      )

    val errorUrl =
      frontendAppConfig.gformFrontendBaseUrl + FileUploadController.noJsErrorCallback(
        formTemplateId,
        ei.sectionNumber,
        ei.maybeAccessCode,
        formComponent.id,
        fileId
      )

    val deleteUrl =
      frontendAppConfig.gformFrontendBaseUrl + FileUploadController.deleteFile(
        formTemplateId,
        ei.maybeAccessCode,
        formComponent.id
      )

    val fileInput: Html = new components.govukFileUpload(govukErrorMessage, govukHint, govukLabel)(fileUpload)

    val noJsButton: Button = Button(
      content = content.Text(messages("file.upload")),
      inputType = Some("submit"),
      classes = "govuk-button--secondary",
      attributes = Map(
        "formaction"  -> s"/file-upload/upload/envelopes/${ei.envelopeId.value}/files/${fileId.value}?redirect-success-url=$successUrl&redirect-error-url=$errorUrl",
        "formenctype" -> "multipart/form-data"
      ),
      preventDoubleClick = true
    )

    val uploadedFiles: Html =
      html.form.snippets
        .uploaded_files(
          ei.maybeAccessCode,
          formTemplateId,
          formComponent.id,
          fileId,
          currentValue,
          noJsButton,
          deleteUrl
        )

    HtmlFormat.fill(List(fileInput, uploadedFiles))

  }

  private def htmlForChoice(
    formComponent: FormComponent,
    choice: ChoiceType,
    options: NonEmptyList[SmartString],
    orientation: Orientation,
    selections: List[Int],
    hints: Option[NonEmptyList[SmartString]],
    optionalHelpText: Option[NonEmptyList[SmartString]],
    validationResult: ValidationResult,
    ei: ExtraInfo
  )(implicit
    l: LangADT,
    sse: SmartStringEvaluator
  ) = {

    val prepopValues =
      if (ei.formModelOptics.pageOpticsData.contains(formComponent.modelComponentId))
        Set.empty[String] // Don't prepop something we already submitted
      else selections.map(_.toString).toSet

    val optionsWithHelpText: NonEmptyList[(SmartString, Option[Html])] =
      optionalHelpText
        .map(
          _.zipWith(options)((helpText, option) =>
            (
              option,
              if (helpText.isEmpty) None
              else Some(markDownParser(helpText))
            )
          )
        )
        .getOrElse(options.map(option => (option, None)))

    val optionsWithHintAndHelpText: NonEmptyList[(SmartString, Option[Hint], Option[Html])] =
      hints
        .map(_.zipWith(optionsWithHelpText) { case (hint, (option, helpText)) =>
          (option, if (hint.isEmpty) None else toHint(Some(hint)), helpText)
        })
        .getOrElse(optionsWithHelpText.map { case (option, helpText) => (option, None, helpText) })

    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)

    val hint = formComponent.helpText.map { ls =>
      Hint(
        content = content.Text(ls.value)
      )
    }

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage = errors.map(error =>
      ErrorMessage(
        content = content.Text(error)
      )
    )

    val isPageHeading = ei.formLevelHeading
    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = content.Text(formComponent.label.value),
            isPageHeading = isPageHeading,
            classes = if (isPageHeading) "govuk-label--l" else ""
          )
        )
      )
    )

    def isChecked(index: Int): Boolean =
      formFieldValidationResult
        .getOptionalCurrentValue(HtmlFieldId.indexed(formComponent.id, index))
        .orElse(prepopValues.find(_ === index.toString))
        .isDefined

    def helpTextHtml(maybeHelpText: Option[Html]): Option[Html] =
      maybeHelpText.map(helpText => html.form.snippets.markdown_wrapper(helpText))

    choice match {
      case Radio | YesNo =>
        val items = optionsWithHintAndHelpText.zipWithIndex.map { case ((option, maybeHint, maybeHelpText), index) =>
          RadioItem(
            id = Some(formComponent.id.value + index),
            value = Some(index.toString),
            content = content.Text(option.value),
            checked = isChecked(index),
            conditionalHtml = helpTextHtml(maybeHelpText),
            attributes = dataLabelAttribute(option),
            hint = maybeHint
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
        val items = optionsWithHintAndHelpText.zipWithIndex.map { case ((option, maybeHint, maybeHelpText), index) =>
          CheckboxItem(
            id = Some(formComponent.id.value + index),
            value = index.toString,
            content = content.Text(option.value),
            checked = isChecked(index),
            conditionalHtml = helpTextHtml(maybeHelpText),
            attributes = dataLabelAttribute(option),
            hint = maybeHint
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

  private def toHint(maybeHint: Option[SmartString])(implicit
    sse: SmartStringEvaluator
  ): Option[Hint] =
    maybeHint.map(hint =>
      Hint(
        content = content.HtmlContent(markDownParser(hint))
      )
    )

  private def htmlForRevealingChoice(
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    multiValue: Boolean,
    options: List[RevealingChoiceElement],
    validationResult: ValidationResult,
    extraInfo: ExtraInfo,
    obligations: Obligations
  )(implicit request: RequestHeader, message: Messages, l: LangADT, sse: SmartStringEvaluator) = {
    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)
    val nestedEi: FormComponentId => Int => ExtraInfo = formComponentId =>
      index =>
        extraInfo
          .copy(
            formLevelHeading = false,
            specialAttributes =
              Map("data-checkbox" -> (formComponent.id.value + index)) // Used by javascript for dynamic calculations
          )
    val revealingChoicesList
      : List[(SmartString, Option[Hint], Int => Boolean, FormComponentId => Int => Option[NonEmptyList[Html]])] =
      options.map { o =>
        val isSelected: Int => Boolean =
          index =>
            extraInfo.formModelOptics.pageOpticsData
              .get(formComponent.modelComponentId)
              .fold(o.selected)(_.contains(index.toString))

        val revealingFieldsHtml: FormComponentId => Int => List[Html] = controlledBy =>
          index =>
            o.revealingFields
              .filterNot(_.onlyShowOnSummary)
              .map(fc =>
                htmlFor(
                  RenderUnit.pure(fc),
                  formTemplateId,
                  nestedEi(controlledBy)(index),
                  validationResult,
                  obligations = obligations
                )
              )

        val maybeRevealingFieldsHtml: FormComponentId => Int => Option[NonEmptyList[Html]] = controlledBy =>
          index =>
            revealingFieldsHtml(controlledBy)(index) match {
              case x :: xs => Some(NonEmptyList(x, xs))
              case Nil     => None
            }

        (o.choice, toHint(o.hint), isSelected, maybeRevealingFieldsHtml)
      }

    val hint = formComponent.helpText.map { ls =>
      Hint(
        content = content.Text(ls.value)
      )
    }

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage = errors.map(error =>
      ErrorMessage(
        content = content.Text(error)
      )
    )

    val isPageHeading = extraInfo.formLevelHeading
    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = content.Text(formComponent.label.value),
            isPageHeading = isPageHeading,
            classes = if (isPageHeading) "govuk-label--l" else ""
          )
        )
      )
    )

    def revealingFieldsHtml(maybeRevealingFieldsHtml: Option[NonEmptyList[Html]]): Option[Html] =
      maybeRevealingFieldsHtml.map(htmls => html.form.snippets.markdown_wrapper(HtmlFormat.fill(htmls.toList)))

    if (multiValue) {
      val items = revealingChoicesList.zipWithIndex.map {
        case ((option, maybeHint, isChecked, maybeRevealingFieldsHtml), index) =>
          CheckboxItem(
            id = Some(formComponent.id.value + index),
            value = index.toString,
            content = content.Text(option.value),
            checked = isChecked(index),
            conditionalHtml = revealingFieldsHtml(maybeRevealingFieldsHtml(formComponent.id)(index)),
            attributes = dataLabelAttribute(option),
            hint = maybeHint
          )
      }

      val checkboxes = Checkboxes(
        idPrefix = Some(formComponent.id.value),
        fieldset = fieldset,
        hint = hint,
        errorMessage = errorMessage,
        name = formComponent.id.value,
        items = items
      )

      new components.govukCheckboxes(govukErrorMessage, govukFieldset, govukHint, govukLabel)(checkboxes)
    } else {

      val items = revealingChoicesList.zipWithIndex.map {
        case ((option, maybeHint, isChecked, maybeRevealingFieldsHtml), index) =>
          RadioItem(
            id = Some(formComponent.id.value + index),
            value = Some(index.toString),
            content = content.Text(option.value),
            checked = isChecked(index),
            conditionalHtml = revealingFieldsHtml(maybeRevealingFieldsHtml(formComponent.id)(index)),
            attributes = dataLabelAttribute(option),
            hint = maybeHint
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

  private def renderLookup(
    formComponent: FormComponent,
    register: Register,
    validationResult: ValidationResult,
    ei: ExtraInfo
  )(implicit
    message: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html = {

    val prepopValue = ei.formModelOptics.pageOpticsData.one(formComponent.modelComponentId)
    val formFieldValidationResult = validationResult(formComponent)

    val labelString = formComponent.label.value
    val isPageHeading = ei.formLevelHeading

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val hiddenClass =
      if (formComponent.derived && !formComponent.presentationHint.exists(_.contains(TotalValue)))
        "govuk-visually-hidden"
      else
        ""

    val errorMessage = errors.map(error =>
      ErrorMessage(
        content = content.Text(error),
        classes = hiddenClass
      )
    )

    val label = Label(
      forAttr = Some(formComponent.id.value),
      isPageHeading = isPageHeading,
      classes = if (isPageHeading) s"govuk-label--l $hiddenClass" else hiddenClass,
      content = content.Text(labelString)
    )

    val hint: Option[Hint] = formComponent.helpText.map { ls =>
      Hint(
        content = content.Text(ls.value)
      )
    }

    lookupRegistry.get(register) match {
      case None => Html("") // Ups
      case Some(AjaxLookup(options, _, showAll)) =>
        html.form.snippets.lookup_autosuggest(
          label,
          formComponent,
          showAll,
          register,
          ei.formTemplate._id,
          ei.maybeAccessCode,
          prepopValue,
          formFieldValidationResult,
          hint,
          getSelectItemsForLookup(formComponent, register, ei, options, prepopValue),
          errorMessage
        )
      case Some(RadioLookup(options)) =>
        val isPageHeading = ei.formLevelHeading
        val fieldset = Some(
          Fieldset(
            legend = Some(
              Legend(
                content = content.Text(formComponent.label.value),
                isPageHeading = isPageHeading,
                classes = if (isPageHeading) s"govuk-label--l $hiddenClass" else hiddenClass
              )
            )
          )
        )

        val currentValue = formFieldValidationResult.getCurrentValue

        val selectedValue = prepopValue.orElse(currentValue).getOrElse("")

        def renderOption(lookupLabel: LookupLabel, index: Int) = RadioItem(
          id = Some(formComponent.id.value + index),
          value = Some(lookupLabel.label),
          content = content.Text(lookupLabel.label),
          checked = lookupLabel.label === selectedValue,
          attributes = dataLabelAttribute(lookupLabel.label)
        )

        val lookupLabels: List[LookupLabel] = options.process(_.sortLookupByIdx)

        val items = lookupLabels.zipWithIndex.map { case (lookupLabel, index) =>
          renderOption(lookupLabel, index)
        }

        val radios = Radios(
          idPrefix = Some(formComponent.id.value),
          fieldset = fieldset,
          hint = hint,
          errorMessage = errorMessage,
          classes = hiddenClass,
          name = formComponent.id.value,
          items = items
        )

        new components.govukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)

    }
  }

  private def renderTextArea(
    text: TextArea,
    formComponent: FormComponent,
    validationResult: ValidationResult,
    ei: ExtraInfo
  )(implicit
    sse: SmartStringEvaluator
  ) = {
    val prepopValue = ei.formModelOptics.pageOpticsData.one(formComponent.modelComponentId)
    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)

    val labelContent = content.Text(formComponent.label.value)

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage: Option[ErrorMessage] = errors.map(error =>
      ErrorMessage(
        content = content.Text(error)
      )
    )

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

    val maybeCurrentValue: Option[String] = prepopValue.orElse(formFieldValidationResult.getCurrentValue)

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

    (characterMaxLength, text.displayCharCount) match {
      case (Some(maxLength), true) =>
        val characterCount = CharacterCount(
          id = formComponent.id.value,
          name = formComponent.id.value,
          rows = text.rows,
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
          rows = text.rows,
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

  private def renderText(
    text: Text,
    formComponent: FormComponent,
    validationResult: ValidationResult,
    ei: ExtraInfo
  )(implicit l: LangADT, sse: SmartStringEvaluator) = {
    def prepopValue: String =
      ei.formModelOptics.formModelVisibilityOptics
        .evalAndApplyTypeInfoExplicit(text.value, formComponent.id)
        .stringRepresentation

    val formFieldValidationResult = validationResult(formComponent)

    val maybeUnit = TextFormatter.appendUnit(text.constraint)
    val labelContent = content.Text(formComponent.label.value)

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage: Option[ErrorMessage] = errors.map(error =>
      ErrorMessage(
        content = content.Text(error)
      )
    )

    val hint: Option[Hint] = formComponent.helpText.map { ls =>
      Hint(
        content = content.Text(ls.value)
      )
    }

    val maybeCurrentValue: Option[String] =
      formFieldValidationResult.getCurrentValue
        .orElse(Some(prepopValue))
        .map { cv =>
          TextFormatter
            .componentTextForRendering(cv, text.constraint, formComponent.presentationHint, formComponent.editable)
        }

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

        val maybeSuffix: Option[SmartString] =
          text.suffix.orElse(maybeUnit.map(u => SmartString(u, Nil)))

        if (formComponent.isSterling) {
          val currencyInput = CurrencyInput(
            id = formComponent.id.value,
            name = formComponent.id.value,
            label = label,
            hint = hint,
            value = maybeCurrentValue,
            errorMessage = hiddenErrorMessage,
            classes = s"$hiddenClass $sizeClasses",
            attributes = ei.specialAttributes ++ attributes
          )

          new hmrcCurrencyInput(govukErrorMessage, govukHint, govukLabel)(currencyInput)

        } else {
          val inputType = formComponent match {
            case IsTelephone() => "tel"
            case IsEmail()     => "email"
            case _             => "text"
          }
          val input = Input(
            id = formComponent.id.value,
            inputType = inputType,
            name = formComponent.id.value,
            label = label,
            hint = hint,
            value = maybeCurrentValue,
            errorMessage = hiddenErrorMessage,
            classes = s"$hiddenClass $sizeClasses",
            attributes = ei.specialAttributes ++ attributes,
            prefix = text.prefix.map(s => PrefixOrSuffix(content = content.Text(s.value))),
            suffix = maybeSuffix.map(s => PrefixOrSuffix(content = content.Text(s.value)))
          )

          new components.govukInput(govukErrorMessage, govukHint, govukLabel)(input)
        }
    }
  }

  private def htmlForSortCode(
    formComponent: FormComponent,
    validationResult: ValidationResult,
    ei: ExtraInfo
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ) = {
    val formFieldValidationResult = validationResult(formComponent)
    html.form.snippets
      .field_template_sort_code(formComponent, formFieldValidationResult, ei.formLevelHeading)

  }

  private def htmlForAddress(
    formComponent: FormComponent,
    international: Boolean,
    validationResult: ValidationResult,
    ei: ExtraInfo
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ) = {
    val formFieldValidationResult = validationResult(formComponent)
    html.form.snippets
      .field_template_address(international, formComponent, formFieldValidationResult, ei.formLevelHeading)
  }

  private def htmlForOverseasAddress(
    formComponent: FormComponent,
    overseasAddress: OverseasAddress,
    validationResult: ValidationResult,
    ei: ExtraInfo
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ) = {
    val formFieldValidationResult = validationResult(formComponent)

    def fetchValue(key: HtmlFieldId, atom: Atom): String =
      formFieldValidationResult.getOptionalCurrentValue(key).getOrElse {
        overseasAddress.value.fold("")(_.getPrepopValue(atom))
      }

    html.form.snippets
      .field_template_overseas_address(
        overseasAddress,
        formComponent,
        formFieldValidationResult,
        ei.formLevelHeading,
        fetchValue
      )
  }

  private def htmlForCalendarDate(
    formComponent: FormComponent,
    validationResult: ValidationResult,
    ei: ExtraInfo
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {

    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage: Option[ErrorMessage] = errors.map(error =>
      ErrorMessage(
        content = content.Text(error)
      )
    )

    val hint: Option[Hint] = formComponent.helpText.map { ls =>
      Hint(
        content = content.Text(ls.value)
      )
    }

    val hasErrors = formFieldValidationResult.isNotOk

    val inputClasses = if (hasErrors) "govuk-input--error" else ""

    val attributes =
      if (formComponent.editable)
        Map.empty[String, String]
      else
        Map("readonly" -> "")

    val items =
      CalendarDate
        .fields(formComponent.modelComponentId.indexedComponentId)
        .map { modelComponentId =>
          val prepop = ei.formModelOptics.pageOpticsData.one(modelComponentId)
          val atom = modelComponentId.atom
          InputItem(
            id = modelComponentId.toMongoIdentifier,
            name = modelComponentId.toMongoIdentifier,
            value = formFieldValidationResult
              .getOptionalCurrentValue(HtmlFieldId.pure(modelComponentId))
              .orElse(prepop),
            label = Some(messages("date." + atom.value.capitalize)),
            classes = s"$inputClasses govuk-input--width-2",
            attributes = attributes
          )
        }

    val isPageHeading = ei.formLevelHeading

    val fieldset = Fieldset(
      legend = Some(
        Legend(
          content = content.Text(formComponent.label.value),
          classes = if (isPageHeading) "govuk-label--l" else "",
          isPageHeading = isPageHeading
        )
      )
    )

    val dateInput = DateInput(
      id = formComponent.id.value,
      items = items.toList,
      hint = hint,
      errorMessage = errorMessage,
      fieldset = Some(fieldset)
    )

    new components.govukDateInput(govukErrorMessage, govukHint, govukFieldset, govukInput)(dateInput)
  }

  private def htmlForDate(
    formComponent: FormComponent,
    offset: Offset,
    dateValue: Option[DateValue],
    validationResult: ValidationResult,
    ei: ExtraInfo
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val prepopValues: Option[DateExpr] = dateValue.map(DateExpr.fromDateValue).map(DateExpr.withOffset(offset, _))

    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage: Option[ErrorMessage] = errors.map(error =>
      ErrorMessage(
        content = content.Text(error)
      )
    )

    val hint: Option[Hint] = formComponent.helpText.map { ls =>
      Hint(
        content = content.Text(ls.value)
      )
    }

    val hasErrors = formFieldValidationResult.isNotOk

    val inputClasses = if (hasErrors) "govuk-input--error" else ""

    val attributes =
      if (formComponent.editable)
        Map.empty[String, String]
      else
        Map("readonly" -> "")

    def sizeForAtom(atom: Atom): String = atom match {
      case Date.year => "govuk-input--width-4"
      case _         => "govuk-input--width-2"
    }

    val items =
      Date
        .fields(formComponent.modelComponentId.indexedComponentId)
        .map { modelComponentId =>
          val prepop = ei.formModelOptics.pageOpticsData.one(modelComponentId)
          val atom = modelComponentId.atom
          InputItem(
            id = modelComponentId.toMongoIdentifier,
            name = modelComponentId.toMongoIdentifier,
            label = Some(messages("date." + atom.value.capitalize)),
            value = formFieldValidationResult
              .getOptionalCurrentValue(HtmlFieldId.pure(modelComponentId))
              .orElse(prepopValues.map(_.valueForAtom(atom)))
              .orElse(prepop),
            classes = s"$inputClasses ${sizeForAtom(atom)}",
            attributes = attributes
          )
        }

    val isPageHeading = ei.formLevelHeading

    val fieldset = Fieldset(
      legend = Some(
        Legend(
          content = content.Text(formComponent.label.value),
          classes = if (isPageHeading) "govuk-label--l" else "",
          isPageHeading = isPageHeading
        )
      )
    )

    val dateInput = DateInput(
      id = formComponent.id.value,
      items = items.toList,
      hint = hint,
      errorMessage = errorMessage,
      fieldset = Some(fieldset)
    )

    new components.govukDateInput(govukErrorMessage, govukHint, govukFieldset, govukInput)(dateInput)
  }

  private def renderTime(
    time: Time,
    formComponent: FormComponent,
    validationResult: ValidationResult,
    ei: ExtraInfo
  )(implicit sse: SmartStringEvaluator) = {
    val prepopValue = ei.formModelOptics.pageOpticsData.one(formComponent.modelComponentId)
    val formFieldValidationResult = validationResult(formComponent)

    val labelContent = content.Text(formComponent.label.value)

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage: Option[ErrorMessage] = errors.map(error =>
      ErrorMessage(
        content = content.Text(error)
      )
    )

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

    val maybeCurrentValue: String = prepopValue.orElse(formFieldValidationResult.getCurrentValue).getOrElse("")

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

  private def htmlForGroup(
    renderUnitGroup: RenderUnit.Group,
    formTemplateId: FormTemplateId,
    ei: ExtraInfo,
    validationResult: ValidationResult,
    obligations: Obligations
  )(implicit
    request: RequestHeader,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ) = {
    val formComponent = renderUnitGroup.formComponent
    val maybeHint =
      formComponent.helpText.map(markDownParser).map(markDown => Hint(content = content.HtmlContent(markDown)))

    val canAddAnother: Option[ModelComponentId] =
      renderUnitGroup.group.repeatsMax.fold(Option.empty[ModelComponentId])(max =>
        if (renderUnitGroup.formComponents.size < max) Some(renderUnitGroup.formComponents.last._2.modelComponentId)
        else None
      )

    val lhtml =
      renderUnitGroup.formComponents.toList.flatMap { case (group, formComponent) =>
        getGroupForRendering(formComponent, formTemplateId, group, validationResult, ei, obligations)
      }

    html.form.snippets.group(
      formComponent,
      maybeHint,
      renderUnitGroup.group,
      lhtml,
      canAddAnother,
      formTemplateId,
      ei.maybeAccessCode,
      ei.sectionNumber
    )
  }

  private def getGroupForRendering(
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    group: Group,
    validationResult: ValidationResult,
    ei: ExtraInfo,
    obligations: Obligations
  )(implicit
    request: RequestHeader,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): List[Html] =
    if (group.repeatsMax.isDefined) {
      val index = formComponent.modelComponentId.maybeIndex.getOrElse(
        throw new IllegalArgumentException(s"Expected group index, but got ${formComponent.modelComponentId}")
      )

      val isLast = !ei.formModelOptics.pageOpticsData.contains(formComponent.modelComponentId.increment)

      val lhtml = group.fields.map(formComponent =>
        htmlFor(RenderUnit.pure(formComponent), formTemplateId, ei, validationResult, obligations = obligations)
      )

      val removeButton: Option[ModelComponentId] =
        if (
          group.repeatsMax.getOrElse(0) === group.repeatsMin.getOrElse(0) ||
          (index === 1 && isLast)
        ) None
        else Some(formComponent.modelComponentId)

      val label = group.repeatLabel.map(_.value).getOrElse("")

      val removeButtonHtml =
        html.form.snippets.delete_group_link(formTemplateId, label, removeButton, ei.maybeAccessCode, ei.sectionNumber)

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

      govukFieldset(fieldSet) :: Nil

    } else {
      val htmls =
        group.fields.map(formComponent =>
          htmlFor(RenderUnit.pure(formComponent), formTemplateId, ei, validationResult, obligations = obligations)
        )
      htmls
    }

  private def emptyRetrievals = AuthenticatedRetrievals(
    governmentGatewayId = GovernmentGatewayId(""),
    enrolments = Enrolments(Set.empty),
    affinityGroup = Individual,
    groupIdentifier = "",
    maybeNino = None
  )

  private def shouldDisplayHeading(singleton: Singleton[DataExpanded])(implicit sse: SmartStringEvaluator): Boolean = {
    val page = singleton.page
    page.fields match {
      case IsGroup(g) :: _              => false
      case IsInformationMessage(_) :: _ => false
      case formComponent :: IsNilOrInfoOnly() =>
        formComponent.editable && formComponent.label.value === page.title.value
      case _ => false
    }
  }

  private def dataLabelAttribute(label: SmartString): Map[String, String] =
    dataLabelAttribute(label.localised.value(LangADT.En))
  private def dataLabelAttribute(label: String): Map[String, String] =
    Map("data-label" -> label.replaceAll("''", "'")) // Unescape single-quote

  private def getSelectItemsForLookup(
    formComponent: FormComponent,
    register: Register,
    ei: ExtraInfo,
    options: LocalisedLookupOptions,
    prepopValue: Option[String]
  )(implicit
    messages: Messages,
    l: LangADT
  ): Option[List[SelectItem]] = {
    val aFormComponents = ei.formModelOptics.formModelVisibilityOptics.formModel.allFormComponents

    val oFormComponent = aFormComponents.find(_.id.baseComponentId === formComponent.id.baseComponentId)

    val selectionCriteria: Option[List[SimplifiedSelectionCriteria]] = oFormComponent flatMap {
      case IsText(Text(Lookup(_, sc), _, _, _, _, _)) => sc
      case _                                          => None
    } map {
      SimplifiedSelectionCriteria
        .convertToSimplifiedSelectionCriteria(_, lookupRegistry, ei.formModelOptics.formModelVisibilityOptics)
    }

    val oLookupLabels: Option[List[LookupLabel]] = selectionCriteria match {
      case Some(sc) =>
        options.m
          .get(l)
          .map(r => LookupOptions(filterBySelectionCriteria(sc, r.options)))
          .map(_.options.keys.toList)

      case None =>
        options.m.get(l).map(_.options.keys.toList)
    }

    oLookupLabels.map { lookupLabels =>
      SelectItem(None, s"${messages("lookup.select.default.option.text")} ${register.asString}") +: lookupLabels
        .sortBy(_.label)
        .map { lookupLabel =>
          SelectItem(
            Some(lookupLabel.label),
            lookupLabel.label,
            if (prepopValue.contains(lookupLabel.label)) true else false
          )
        }
    }
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
