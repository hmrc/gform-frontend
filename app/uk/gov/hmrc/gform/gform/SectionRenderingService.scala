/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.data.NonEmptyList

import java.time.LocalDate
import cats.instances.int._
import cats.instances.string._
import cats.syntax.all._
import org.jsoup.Jsoup
import play.api.mvc.Call
import shapeless.syntax.typeable._
import play.api.i18n.Messages
import play.api.mvc.{ Request, RequestHeader }
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.auth.core.Enrolments
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.gform.models.{ AddToListSummaryRow, Atom, Basic, Bracket, CheckYourAnswers, DataExpanded, DateExpr, FastForward, FileUploadUtils, FormModel, PageMode, PageModel, Repeater, SectionRenderingInformation, Singleton, Visibility }
import uk.gov.hmrc.gform.monoidHtml
import uk.gov.hmrc.gform.sharedmodel.AffinityGroup.Individual
import uk.gov.hmrc.gform.auth.models.{ AuthenticatedRetrievals, GovernmentGatewayId, MaterialisedRetrievals, OtherRetrievals }
import uk.gov.hmrc.gform.commons.MarkDownUtil.markDownParser
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, EditAddToList, GformFlashKeys, Navigator, Origin, SaveAndContinue }
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.gform.handlers.FormHandlerResult
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.javascript.JavascriptMaker
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelRenderPageOptics, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.config.FileExtension
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.gform.lookup.LookupOptions.filterBySelectionCriteria
import uk.gov.hmrc.gform.ops.FormComponentOps
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations._
import uk.gov.hmrc.gform.summary.{ AddToListCYARender, AddressRecordLookup, FormComponentRenderDetails, FormComponentSummaryRenderer, SummaryRender }
import uk.gov.hmrc.gform.tasklist.TaskListUtils
import uk.gov.hmrc.gform.upscan.{ FormMetaData, UpscanData, UpscanInitiate }
import uk.gov.hmrc.gform.validation.HtmlFieldId
import uk.gov.hmrc.gform.validation._
import uk.gov.hmrc.gform.views.summary.TextFormatter
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.gform.views.html.specimen
import uk.gov.hmrc.gform.views.html.summary.header
import uk.gov.hmrc.gform.views.components.TotalText
import uk.gov.hmrc.govukfrontend.views.html.components
import uk.gov.hmrc.govukfrontend.views.html.components.{ GovukCharacterCount, GovukPanel, GovukSummaryList, GovukTable, Panel }
import uk.gov.hmrc.govukfrontend.views.viewmodels.backlink.BackLink
import uk.gov.hmrc.govukfrontend.views.viewmodels.button.{ Button => GovukButton }
import uk.gov.hmrc.govukfrontend.views.viewmodels.checkboxes.{ CheckboxItem, Checkboxes, ExclusiveCheckbox }
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.{ Content, Empty, HtmlContent }
import uk.gov.hmrc.govukfrontend.views.viewmodels.dateinput.{ DateInput, InputItem }
import uk.gov.hmrc.govukfrontend.views.viewmodels.errormessage.ErrorMessage
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.ErrorLink
import uk.gov.hmrc.govukfrontend.views.viewmodels.fieldset.{ Fieldset, Legend }
import uk.gov.hmrc.govukfrontend.views.viewmodels.fileupload
import uk.gov.hmrc.govukfrontend.views.viewmodels.hint.Hint
import uk.gov.hmrc.govukfrontend.views.viewmodels.input.{ Input, PrefixOrSuffix }
import uk.gov.hmrc.govukfrontend.views.viewmodels.label.Label
import uk.gov.hmrc.hmrcfrontend.views.viewmodels.listwithactions.{ ListWithActions, ListWithActionsAction, ListWithActionsItem }
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.{ RadioItem, Radios }
import uk.gov.hmrc.govukfrontend.views.viewmodels.select.{ Select, SelectItem }
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{ ActionItem, Actions, SummaryList, SummaryListRow, Value }
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.{ HeadCell, Table, TableRow => GovukTableRow }
import uk.gov.hmrc.govukfrontend.views.viewmodels.textarea.Textarea
import uk.gov.hmrc.govukfrontend.views.viewmodels.warningtext.WarningText
import uk.gov.hmrc.hmrcfrontend.views.Aliases.CharacterCount
import uk.gov.hmrc.hmrcfrontend.views.html.components.{ HmrcCharacterCount, HmrcListWithActions }
import uk.gov.hmrc.gform.views.summary.SummaryListRowHelper
import MiniSummaryRow._
import uk.gov.hmrc.auth.core.ConfidenceLevel
import uk.gov.hmrc.gform.sharedmodel.formtemplate.KeyDisplayWidth.KeyDisplayWidth
import uk.gov.hmrc.gform.models.helpers.MiniSummaryListHelper
import uk.gov.hmrc.gform.payment.PaymentReference
import uk.gov.hmrc.govukfrontend.views.Aliases.Key
import uk.gov.hmrc.govukfrontend.views.html.helpers.{ GovukFormGroup, GovukHintAndErrorMessage }

import scala.annotation.tailrec

case class FormRender(id: String, name: String, value: String)
case class OptionParams(value: String, fromDate: LocalDate, toDate: LocalDate, selected: Boolean)

class SectionRenderingService(
  frontendAppConfig: FrontendAppConfig,
  lookupRegistry: LookupRegistry
) {

  def renderAddToListCheckYourAnswers[T <: PageMode](
    checkYourAnswers: CheckYourAnswers[T],
    formTemplate: FormTemplate,
    specimenSource: Option[FormTemplate],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    addToListIteration: Bracket.AddToListIteration[Visibility],
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    validationResult: ValidationResult,
    cache: AuthCacheWithForm,
    envelope: EnvelopeWithMapping,
    addressRecordLookup: AddressRecordLookup,
    fastForward: List[FastForward]
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html = {

    val displayWidth = checkYourAnswers.displayWidth.getOrElse(LayoutDisplayWidth.M)
    val keyDisplayWidth = checkYourAnswers.keyDisplayWidth.getOrElse(KeyDisplayWidth.S)
    val listResult = validationResult.formFieldValidationResults
    val pageLevelErrorHtml = PageLevelErrorHtml.generatePageLevelErrorHtml(listResult, List.empty)
    val renderComeBackLater =
      cache.retrievals.renderSaveAndComeBackLater && !DraftRetrievalHelper.isNotPermitted(
        formTemplate,
        cache.retrievals
      )

    val envelopeUpd = envelope.byPurpose(SummaryPagePurpose.ForUser)

    val summaryListRecords: List[SummaryList] = SectionRenderingService.summaryList(
      formTemplate._id,
      checkYourAnswers,
      addToListIteration,
      formModelOptics.formModelVisibilityOptics,
      maybeAccessCode,
      cache,
      validationResult,
      envelopeUpd,
      addressRecordLookup,
      sectionNumber,
      fastForward
    )

    val infoFields = checkYourAnswers.fields
      .map { fields =>
        fields.toList
          .filter { field =>
            field.includeIf.fold(true) { includeIf =>
              formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None)
            }
          }
          .map {
            case info @ IsInformationMessage(InformationMessage(infoType, infoText, _)) =>
              htmlForInformationMessage(info, infoType, infoText)
            case fc @ IsTableComp(table) =>
              htmlForTableComp(fc, table, formModelOptics)
            case fc @ IsMiniSummaryList(miniSummaryList) =>
              val repeaterPage: Page[Basic] =
                Page(
                  title = checkYourAnswers.title,
                  id = checkYourAnswers.id,
                  noPIITitle = checkYourAnswers.noPIITitle,
                  description = None,
                  shortName = None,
                  caption = None,
                  includeIf = None,
                  fields = List(fc),
                  continueLabel = None,
                  continueIf = None,
                  instruction = None,
                  presentationHint = None,
                  dataRetrieve = None,
                  confirmation = None,
                  redirects = None,
                  hideSaveAndComeBackButton = None,
                  removeItemIf = None,
                  displayWidth = None,
                  notRequiredIf = None
                )

              val singleton = Singleton(repeaterPage).asInstanceOf[Singleton[DataExpanded]]

              val ei: ExtraInfo = ExtraInfo(
                singleton = singleton,
                maybeAccessCode = None,
                sectionNumber = formTemplate.sectionNumberZero,
                formModelOptics = formModelOptics,
                formTemplate = formTemplate,
                envelopeId = cache.form.envelopeId,
                envelope = EnvelopeWithMapping.empty,
                formMaxAttachmentSizeMB = 0,
                retrievals = cache.retrievals,
                formLevelHeading = false,
                specialAttributes = Map.empty[String, String],
                addressRecordLookup = AddressRecordLookup.from(cache.form.thirdPartyData)
              )
              htmlForMiniSummaryList(
                fc,
                formTemplate._id,
                miniSummaryList.rows,
                ei,
                validationResult,
                NotChecked,
                keyDisplayWidth
              )
            case unsupported =>
              throw new Exception("AddToList.CheckYourAnswers.fields contains a non-Info component: " + unsupported)
          }
      }
      .getOrElse(List(HtmlFormat.empty))

    val (title, noPIITitle) =
      SectionRenderingService.atlCyaTitles(cache, sectionNumber, checkYourAnswers, formModelOptics)

    val navigator = Navigator(sectionNumber, formModelOptics.formModelVisibilityOptics.formModel)
    val isDecSectionAvailable = navigator.nextSectionNumber.isAddToListDeclarationPage

    val ff = fastForward match {
      case Nil                       => Nil
      case FastForward.CYA(to) :: xs => FastForward.CYA(to) :: xs
      case FastForward.StopAt(sn) :: xs if !isDecSectionAvailable =>
        FastForward.StopAt(sn.increment(formModelOptics.formModelVisibilityOptics.formModel)) :: xs
      case otherwise if isDecSectionAvailable => FastForward.StopAt(sectionNumber) :: otherwise
      case _                                  => fastForward
    }

    html.form.addToListCheckYourAnswers(
      title,
      checkYourAnswers.expandedCaption.map(_.value()),
      noPIITitle,
      formTemplate,
      maybeAccessCode,
      sectionNumber,
      summaryListRecords,
      frontendAppConfig,
      SectionRenderingService.determineContinueLabelKey(
        cache.retrievals.continueLabelKey,
        DraftRetrievalHelper.isNotPermitted(formTemplate, cache.retrievals),
        checkYourAnswers.expandedContinueLabel,
        false
      ),
      renderComeBackLater,
      pageLevelErrorHtml,
      checkYourAnswers.expandedHeader.map(markDownParser),
      checkYourAnswers.expandedFooter.map(markDownParser),
      specimenNavigation(formTemplate, specimenSource, sectionNumber, formModelOptics.formModelRenderPageOptics),
      ff,
      infoFields,
      displayWidth,
      isMainContentFullWidth = checkYourAnswers.displayWidth.nonEmpty
    )

  }

  def renderATLDeclarationSection(
    maybeAccessCode: Option[AccessCode],
    declarationPage: Singleton[DataExpanded],
    cache: AuthCacheWithForm,
    formHandlerResult: FormHandlerResult,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    fastForward: List[FastForward],
    sectionNumber: SectionNumber
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html = {

    val listResult = formHandlerResult.validationResult.formFieldValidationResults
    val pageLevelErrorHtml = PageLevelErrorHtml.generatePageLevelErrorHtml(listResult, List.empty)

    val ei = ExtraInfo(
      declarationPage,
      maybeAccessCode,
      cache.formTemplate.sectionNumberZero,
      formModelOptics,
      cache.formTemplate,
      cache.form.envelopeId,
      EnvelopeWithMapping.empty,
      0,
      cache.retrievals,
      formLevelHeading = false,
      specialAttributes = Map.empty,
      AddressRecordLookup.from(cache.form.thirdPartyData)
    )

    val infoFields = declarationPage.page.fields
      .filter { field =>
        field.includeIf.fold(true) { includeIf =>
          formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None)
        }
      }
      .map {
        case info @ IsInformationMessage(InformationMessage(infoType, infoText, _)) =>
          htmlForInformationMessage(info, infoType, infoText)
        case fc @ IsTableComp(table) =>
          htmlForTableComp(fc, table, formModelOptics)
        case fc @ IsMiniSummaryList(miniSummaryList) =>
          htmlForMiniSummaryList(
            fc,
            cache.formTemplate._id,
            miniSummaryList.rows,
            ei,
            formHandlerResult.validationResult,
            NotChecked,
            KeyDisplayWidth.M
          )
        case unsupported =>
          throw new Exception("AddToList.DeclarationPage.fields contains a non-read only component: " + unsupported)
      }

    val continueLabel = declarationPage.page.continueLabel.map(_.value()).getOrElse(messages("button.continue"))

    val renderingInfo = SectionRenderingInformation(
      cache.formTemplate._id,
      maybeAccessCode,
      sectionNumber,
      declarationPage.page.sectionHeader(),
      declarationPage.noPIITitle.fold(
        declarationPage.title.valueWithoutInterpolations(
          formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_)
        )
      )(_.value()),
      infoFields,
      "",
      cache.form.envelopeId,
      uk.gov.hmrc.gform.gform.routes.FormController
        .updateFormData(cache.formTemplate._id, maybeAccessCode, sectionNumber, fastForward, SaveAndContinue),
      false,
      continueLabel,
      0,
      FileInfoConfig.allAllowedFileTypes,
      Nil,
      Map.empty,
      Map.empty,
      None,
      false
    )
    val mainForm = html.form.form_standard(
      renderingInfo,
      shouldDisplayContinue = true,
      ei.saveAndComeBackLaterButton,
      isFileUploadOnlyPage = false,
      None
    )

    val href = uk.gov.hmrc.gform.gform.routes.FormController
      .backAction(
        cache.formTemplate._id,
        maybeAccessCode,
        sectionNumber,
        fastForward
      )

    val backLink =
      new BackLink(href = href.path, content = content.Text(messages("linkText.back")))

    html.form.form(
      cache.formTemplate,
      pageLevelErrorHtml,
      renderingInfo,
      mainForm,
      backLink = Some(backLink),
      shouldDisplayHeading = true,
      frontendAppConfig,
      fastForward = fastForward,
      accessCode = maybeAccessCode
    )
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
    specimenSource: Option[FormTemplate],
    validationResult: ValidationResult,
    retrievals: MaterialisedRetrievals,
    fastForward: List[FastForward]
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html = {

    val listResult = validationResult.formFieldValidationResults
    val pageLevelErrorHtml = PageLevelErrorHtml.generatePageLevelErrorHtml(listResult, List.empty)
    val actionForm = uk.gov.hmrc.gform.gform.routes.FormController
      .updateFormData(
        formTemplate._id,
        maybeAccessCode,
        sectionNumber,
        if (
          fastForward.contains(FastForward.Yes) && fastForward
            .contains(FastForward.CYA(SectionOrSummary.FormSummary))
        ) {
          List(FastForward.CYA(SectionOrSummary.FormSummary))
        } else {
          fastForward
        },
        SaveAndContinue
      )

    val formComponent = repeater.addAnotherQuestion

    val descriptions: NonEmptyList[AtlDescription] = bracket.repeaters.map(_.expandedDescription)

    val descriptionTotals = bracket.repeaters.map(_.expandedDescriptionTotal)

    val choice = formComponent.`type`.cast[Choice].get

    val formFieldValidationResult = validationResult(formComponent)
    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage: Option[ErrorMessage] = errors.map(error =>
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            classes = getLegendClasses(false, formComponent.labelSize),
            content = content.Text(formComponent.label.value())
          )
        )
      )
    )

    val hiddenFieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            classes = getLegendClasses(false, formComponent.labelSize),
            content = content.Text(formComponent.label.value())
          )
        ),
        attributes = Map("style" -> "display:none")
      )
    )

    def isChecked(index: String): Boolean =
      formFieldValidationResult
        .getOptionalCurrentValue(HtmlFieldId.indexed(formComponent.id, index))
        .isDefined

    val items = choice.options.zipWithIndex.map { case (option, index) =>
      RadioItem(
        id = Some(formComponent.id.value + index),
        value = Some(option.getValue(index, formModelOptics.formModelVisibilityOptics)),
        content = content.Text(option.label.value()),
        checked = isChecked(option.getValue(index, formModelOptics.formModelVisibilityOptics)),
        attributes =
          dataLabelAttribute(option.label, formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_))
      )
    }

    val radios = Radios(
      idPrefix = Some(formComponent.id.value),
      fieldset = fieldset,
      errorMessage = errorMessage,
      name = formComponent.id.value,
      items = items.toList,
      hint = hintText(formComponent),
      classes = if (choice.orientation === Horizontal) "govuk-radios--inline" else ""
    )

    val addAnotherQuestion: Html =
      new components.GovukRadios(govukFieldset, govukHint, govukLabel, govukFormGroup, govukHintAndErrorMessage)(radios)

    val evalRepeatsUntil = repeater.repeatsUntil.exists(repeatsUntil =>
      formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(repeatsUntil, None)
    )

    val evalRepeatsWhile = repeater.repeatsWhile.exists(repeatsWhile =>
      formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(repeatsWhile, None)
    )

    val infoFields = repeater.fields
      .map { fields =>
        fields.toList
          .filter { field =>
            field.includeIf.fold(true) { includeIf =>
              formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None)
            }
          }
          .map {
            case info @ IsInformationMessage(InformationMessage(infoType, infoText, _)) =>
              htmlForInformationMessage(info, infoType, infoText)
            case fc @ IsTableComp(table) =>
              htmlForTableComp(fc, table, formModelOptics)
            case fc @ IsMiniSummaryList(miniSummaryList) =>
              val repeaterPage: Page[Basic] =
                Page(
                  title = repeater.title,
                  id = repeater.id,
                  noPIITitle = repeater.noPIITitle,
                  description = None,
                  shortName = None,
                  caption = None,
                  includeIf = None,
                  fields = List(fc),
                  continueLabel = None,
                  continueIf = None,
                  instruction = None,
                  presentationHint = None,
                  dataRetrieve = None,
                  confirmation = None,
                  redirects = None,
                  hideSaveAndComeBackButton = None,
                  removeItemIf = None,
                  displayWidth = None,
                  notRequiredIf = None
                )
              val singleton = Singleton(repeaterPage).asInstanceOf[Singleton[DataExpanded]]

              val ei: ExtraInfo = ExtraInfo(
                singleton = singleton,
                maybeAccessCode = None,
                sectionNumber = formTemplate.sectionNumberZero,
                formModelOptics = formModelOptics,
                formTemplate = formTemplate,
                envelopeId = form.envelopeId,
                envelope = EnvelopeWithMapping.empty,
                formMaxAttachmentSizeMB = 0,
                retrievals = retrievals,
                formLevelHeading = false,
                specialAttributes = Map.empty[String, String],
                addressRecordLookup = AddressRecordLookup.from(form.thirdPartyData)
              )
              htmlForMiniSummaryList(
                fc,
                formTemplate._id,
                miniSummaryList.rows,
                ei,
                validationResult,
                NotChecked,
                KeyDisplayWidth.S
              )
            case unsupported => throw new Exception("AddToList.fields contains a non-Info component: " + unsupported)
          }
      }
      .getOrElse(List(HtmlFormat.empty))

    val radiosWithYes =
      radios.copy(
        items =
          items.copy(head = items.head.copy(checked = true), tail = items.tail.map(_.copy(checked = false))).toList,
        fieldset = hiddenFieldset
      )

    val addAnotherQuestionWithYes: Html =
      new components.GovukRadios(
        govukFieldset = govukFieldset,
        govukHint = govukHint,
        govukLabel = govukLabel,
        govukFormGroup = govukFormGroup,
        govukHintAndErrorMessage = govukHintAndErrorMessage
      )(radiosWithYes)

    val radiosWithNo =
      radios.copy(
        items =
          items.copy(head = items.head.copy(checked = false), tail = items.tail.map(_.copy(checked = true))).toList,
        fieldset = hiddenFieldset
      )

    val addAnotherQuestionWithNo: Html =
      new components.GovukRadios(govukFieldset, govukHint, govukLabel, govukFormGroup, govukHintAndErrorMessage)(
        radiosWithNo
      )

    val addAnotherQuestionSnippets =
      if (evalRepeatsWhile) addAnotherQuestionWithYes
      else if (evalRepeatsUntil) addAnotherQuestionWithNo
      else addAnotherQuestion

    val snippets = HtmlFormat.fill(infoFields :+ addAnotherQuestionSnippets)

    val shouldDisplayBack: Boolean = {
      if (sectionNumber.isTaskList) true
      else
        Origin(DataOrigin.unSwapDataOrigin(formModelOptics).formModelVisibilityOptics.formModel)
          .filteredSectionNumbers(sectionNumber)
          .sorted
          .exists(_ < sectionNumber)
    }

    val recordTableDescriptions: NonEmptyList[AddToListSummaryRow] = descriptions.zipWithIndex.map {
      case (description, index) =>
        description match {
          case AtlDescription.SmartStringBased(ss) =>
            val html = markDownParser(ss)
            AddToListSummaryRow.ListWithActionsRow(index, html, Jsoup.parse(html.body).text())
          case AtlDescription.KeyValueBased(k, v) =>
            AddToListSummaryRow.SummaryListRow(index, markDownParser(k), markDownParser(v), actionButtons = true)
        }
    }

    val descTotalRow: Option[AddToListSummaryRow] = descriptionTotals.toList.flatten.lastOption match {
      case Some(AtlDescription.KeyValueBased(k, v)) =>
        Some(
          AddToListSummaryRow
            .SummaryListRow(recordTableDescriptions.size, markDownParser(k), markDownParser(v), actionButtons = false)
        )
      case None => None
    }

    val recordTable: NonEmptyList[AddToListSummaryRow] =
      descTotalRow.fold(recordTableDescriptions)(row => recordTableDescriptions.append(row))

    val summaryList = recordTable.collectFirst {
      case _: AddToListSummaryRow.ListWithActionsRow =>
        val listWithActionsItems = recordTable.collect {
          case AddToListSummaryRow.ListWithActionsRow(index, name, text) =>
            val changeAction = ListWithActionsAction(
              content = content.Text(messages("addToList.change")),
              href = uk.gov.hmrc.gform.gform.routes.FormController
                .addToListAction(
                  formTemplate._id,
                  maybeAccessCode,
                  sectionNumber,
                  FastForward.Yes :: FastForward.CYA(SectionOrSummary.Section(sectionNumber)) :: fastForward,
                  EditAddToList(index, AddToListId(bracket.source.id.formComponentId))
                )
                .url,
              visuallyHiddenText = Some(messages("addToList.change.visually.hidden", text)),
              attributes = Map("aria-label" -> messages("addToList.change.visually.hidden", text))
            )

            val removeAction = ListWithActionsAction(
              content = content.Text(messages("addToList.remove")),
              href = uk.gov.hmrc.gform.gform.routes.FormAddToListController
                .requestRemoval(
                  formTemplate._id,
                  maybeAccessCode,
                  sectionNumber,
                  index,
                  AddToListId(bracket.source.id.formComponentId),
                  fastForward
                )
                .url,
              visuallyHiddenText = Some(messages("addToList.change.visually.hidden", text)),
              attributes = Map("aria-label" -> messages("addToList.change.visually.hidden", text))
            )
            ListWithActionsItem(
              name = HtmlContent(name),
              actions = List(changeAction, removeAction)
            )

        }
        new HmrcListWithActions()(
          ListWithActions(
            items = listWithActionsItems,
            classes = "hmrc-add-to-a-list--wide"
          )
        )

      case _: AddToListSummaryRow.SummaryListRow =>
        val summaryRows = recordTable.collect {
          case AddToListSummaryRow.SummaryListRow(index, key, value, actionButtons) =>
            val changeAction = ActionItem(
              content = content.Text(messages("addToList.change")),
              href = uk.gov.hmrc.gform.gform.routes.FormController
                .addToListAction(
                  formTemplate._id,
                  maybeAccessCode,
                  sectionNumber,
                  FastForward.Yes :: FastForward.CYA(SectionOrSummary.Section(sectionNumber)) :: fastForward,
                  EditAddToList(index, AddToListId(bracket.source.id.formComponentId))
                )
                .url,
              visuallyHiddenText = Some(messages("addToList.change.visually.hidden", Jsoup.parse(key.body).text())),
              attributes =
                Map("aria-label" -> messages("addToList.change.visually.hidden", Jsoup.parse(key.body).text()))
            )

            val removeAction = ActionItem(
              content = content.Text(messages("addToList.remove")),
              href = uk.gov.hmrc.gform.gform.routes.FormAddToListController
                .requestRemoval(
                  formTemplate._id,
                  maybeAccessCode,
                  sectionNumber,
                  index,
                  AddToListId(bracket.source.id.formComponentId),
                  fastForward
                )
                .url,
              visuallyHiddenText = Some(messages("addToList.change.visually.hidden", Jsoup.parse(key.body).text())),
              attributes =
                Map("aria-label" -> messages("addToList.change.visually.hidden", Jsoup.parse(key.body).text()))
            )

            val summaryListRowClasses = "hmrc-add-to-a-list--wide"

            SummaryListRow(
              key = Key(HtmlContent(key), classes = summaryListRowClasses),
              value = Value(HtmlContent(value), classes = summaryListRowClasses),
              actions =
                if (actionButtons)
                  Some(Actions(items = List(changeAction, removeAction), classes = summaryListRowClasses))
                else None
            )

        }
        new GovukSummaryList()(
          SummaryList(rows = summaryRows)
        )
    }

    val renderComeBackLater =
      retrievals.renderSaveAndComeBackLater && !DraftRetrievalHelper.isNotPermitted(formTemplate, retrievals)

    html.form.addToList(
      repeater.title.value(),
      repeater.expandedCaption.map(_.value()),
      repeater.noPIITitle.fold(
        repeater.title.valueWithoutInterpolations(
          formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_)
        )
      )(_.value()),
      bracket,
      formTemplate,
      pageLevelErrorHtml,
      frontendAppConfig,
      actionForm,
      renderComeBackLater,
      SectionRenderingService.determineContinueLabelKey(
        retrievals.continueLabelKey,
        DraftRetrievalHelper.isNotPermitted(formTemplate, retrievals),
        bracket.source.repeaterContinueLabel,
        false
      ),
      shouldDisplayBack,
      snippets,
      specimenNavigation(formTemplate, specimenSource, sectionNumber, formModelOptics.formModelRenderPageOptics),
      maybeAccessCode,
      sectionNumber,
      fastForward,
      summaryList.getOrElse(HtmlFormat.empty),
      repeater.displayWidth.getOrElse(LayoutDisplayWidth.M)
    )
  }

  def renderSection(
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    formHandlerResult: FormHandlerResult,
    formTemplate: FormTemplate,
    specimenSource: Option[FormTemplate],
    envelopeId: EnvelopeId,
    singleton: Singleton[DataExpanded],
    formMaxAttachmentSizeMB: Int,
    allowedFileTypes: AllowedFileTypes,
    restrictedFileExtensions: List[FileExtension],
    retrievals: MaterialisedRetrievals,
    obligations: Obligations,
    fastForward: List[FastForward],
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    upscanInitiate: UpscanInitiate,
    addressRecordLookup: AddressRecordLookup
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
      specialAttributes = Map.empty,
      addressRecordLookup
    )
    val actionForm: Call = uk.gov.hmrc.gform.gform.routes.FormController
      .updateFormData(formTemplate._id, maybeAccessCode, sectionNumber, fastForward, SaveAndContinue)

    val page = singleton.page

    val listResult = validationResult.formFieldValidationResults(singleton)

    val javascript =
      JavascriptMaker.generateJs(sectionNumber, formModelOptics)

    val pageLevelErrorHtml = request.flash.get(GformFlashKeys.FileUploadError) match {
      case Some(message) =>
        PageLevelErrorHtml.noJSFileUploadError(message, request.flash.get(GformFlashKeys.FileUploadFileId))
      case None => PageLevelErrorHtml.generatePageLevelErrorHtml(listResult, List.empty)
    }

    val originSection = Origin(
      DataOrigin.unSwapDataOrigin(formModelOptics).formModelVisibilityOptics.formModel
    ).minSectionNumber
    val renderUnits: List[RenderUnit] = page.renderUnits

    val formModel = formModelOptics.formModelRenderPageOptics.formModel
    val formComponents = formModel(sectionNumber).allFormComponents

    val fileUploadComponents: List[FormComponent] = formComponents.collect {
      case fc @ IsFileUpload(_)      => fc
      case fc @ IsMultiFileUpload(_) => fc
    }

    val fileUploadMaxSize: Map[FormComponentId, Int] = formComponents.collect {
      case fc @ IsFileUpload(fu)      => fc.id -> fu.fileSizeLimit.getOrElse(ei.formMaxAttachmentSizeMB)
      case fc @ IsMultiFileUpload(fu) => fc.id -> fu.fileSizeLimit.getOrElse(ei.formMaxAttachmentSizeMB)
    }.toMap

    val upscanData: Map[FormComponentId, UpscanData] =
      fileUploadComponents.flatMap { fc =>
        val uploadRequest = upscanInitiate.get(fc.id).uploadRequest
        val snippetsForUpscan = List(htmlForUpscan(fc, ei, uploadRequest.fields))
        Some(
          fc.id -> UpscanData(uploadRequest.href, snippetsForUpscan, FormMetaData(fc.id, "gf-upscan-" + fc.id.value))
        )
      }.toMap

    val snippetsForFields = renderUnits
      .map(renderUnit =>
        htmlFor(renderUnit, formTemplate._id, ei, validationResult, obligations, upscanInitiate, upscanData)
      )
    val renderComeBackLater = retrievals.renderSaveAndComeBackLater && page.continueIf.fold(true)(
      !_.isTerminationPage(formModelOptics.formModelVisibilityOptics.booleanExprResolver)
    ) && !DraftRetrievalHelper.isNotPermitted(formTemplate, retrievals) && !singleton.page.isHideSaveAndComeBackButton

    val tableComps = formComponents.collect { case IsTableComp(tc) => tc }
    val miniSummaryListComps = formComponents.collect { case IsMiniSummaryList(tc) => tc }

    val maybeDisplayWidth =
      if (tableComps.nonEmpty || miniSummaryListComps.nonEmpty) Some(page.displayWidth.getOrElse(LayoutDisplayWidth.M))
      else None

    val hasGroupComponent: Boolean = formComponents.exists {
      case IsGroup(_) => true
      case _          => false
    }

    val renderingInfo = SectionRenderingInformation(
      formTemplate._id,
      maybeAccessCode,
      sectionNumber,
      page.sectionHeader(),
      page.noPIITitle.fold(page.title.value())(_.value()),
      snippetsForFields,
      javascript,
      envelopeId,
      ei.isFileUploadOnlyPage(validationResult).fold(actionForm) { case (formComponent, _) =>
        upscanData.get(formComponent.id) match {
          case None             => throw new IllegalArgumentException(s"Unable to find upscanData for ${formComponent.id}")
          case Some(upscanData) => Call("POST", upscanData.url)
        }
      },
      renderComeBackLater,
      SectionRenderingService.determineContinueLabelKey(
        retrievals.continueLabelKey,
        DraftRetrievalHelper.isNotPermitted(formTemplate, retrievals),
        page.continueLabel,
        ei.getButtonName(validationResult).isDefined
      ),
      formMaxAttachmentSizeMB,
      allowedFileTypes,
      restrictedFileExtensions,
      ei.isFileUploadOnlyPage(validationResult).fold(upscanData)(_ => Map.empty[FormComponentId, UpscanData]),
      fileUploadMaxSize,
      maybeDisplayWidth,
      hasGroupComponent
    )

    val mainForm: Html = html.form.form_standard(
      renderingInfo,
      shouldDisplayContinue = !page.isTerminationPage(formModelOptics.formModelVisibilityOptics.booleanExprResolver),
      ei.saveAndComeBackLaterButton,
      ei.isFileUploadOnlyPage(validationResult).isDefined,
      ei.getButtonName(validationResult)
    )

    html.form.form(
      formTemplate,
      pageLevelErrorHtml,
      renderingInfo,
      mainForm,
      backLink = mkBackLink(
        formTemplate,
        maybeAccessCode,
        sectionNumber,
        originSection,
        fastForward,
        listResult.exists(_.fieldErrors.nonEmpty)
      ),
      shouldDisplayHeading = !formLevelHeading,
      frontendAppConfig,
      specimenNavigation =
        specimenNavigation(formTemplate, specimenSource, sectionNumber, formModelOptics.formModelRenderPageOptics),
      fastForward,
      isMainContentFullWidth = maybeDisplayWidth.nonEmpty,
      accessCode = maybeAccessCode
    )
  }

  private def pageIncludeIf(page: Page[Basic], formComponentId: FormComponentId): Option[IncludeIf] = {
    val firstPageFormComponentId: Option[FormComponentId] =
      page.fields.headOption.map(_.id)

    firstPageFormComponentId.flatMap { firstPageFcId =>
      if (firstPageFcId === formComponentId) {
        page.includeIf
      } else {
        Option.empty[IncludeIf]
      }
    }
  }

  private def specimenNavigation(
    formTemplate: FormTemplate,
    specimenSource: Option[FormTemplate],
    sectionNumber: SectionNumber,
    formModelRenderPageOptics: FormModelRenderPageOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[_],
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html =
    if (formTemplate.isSpecimen) {
      sectionNumber.fold { classic =>
        val pages: NonEmptyList[(PageModel[DataExpanded], SectionNumber)] =
          formModelRenderPageOptics.formModel.pagesWithIndex

        val currentPageModel: Option[PageModel[DataExpanded]] =
          pages.collectFirst { case (pageModel, sn) if sn === classic => pageModel }

        val firstFormComponentId: Option[FormComponentId] = currentPageModel.flatMap(_.allFormComponentIds.headOption)

        val maybeIncludeIf: Option[IncludeIf] =
          (specimenSource, firstFormComponentId) match {
            case (Some(specimenSrc), Some(formComponentId)) =>
              specimenSrc.formKind.fold { classicKind =>
                val includeIfs: List[Option[IncludeIf]] = classicKind.sections.flatMap { section =>
                  section.fold { nonRepeatingSection =>
                    pageIncludeIf(nonRepeatingSection.page, formComponentId) :: Nil
                  } { repeatedSection =>
                    pageIncludeIf(repeatedSection.page, formComponentId) :: Nil
                  } { addToList =>
                    addToList.pages.toList.map(page => pageIncludeIf(page, formComponentId))
                  }
                }
                includeIfs.flatMap(_.toList).headOption
              }(taskListKind => throw new Exception("Task list not supported"))

            case _ => throw new Exception("Not a specimen")
          }

        val classicPages: List[(PageModel[DataExpanded], SectionNumber.Classic)] =
          pages.toList.collect { case (pageModel, c: SectionNumber.Classic) =>
            pageModel -> c
          }

        val specimenLinks: SpecimenLinks = SpecimenLinks.from(classicPages.map(_._2), classic)

        specimen.navigation(
          formTemplate,
          classic,
          classicPages,
          specimenLinks,
          maybeIncludeIf
        )
      } { taskList =>
        val coordinates: Coordinates = taskList.coordinates
        val classic: SectionNumber.Classic = taskList.sectionNumber

        val pages: NonEmptyList[(PageModel[DataExpanded], SectionNumber)] =
          formModelRenderPageOptics.formModel.pagesWithIndex

        val allSectionNumbers: List[SectionNumber] =
          pages.map(_._2).filter(_.maybeCoordinates.exists(_.taskSectionNumber === coordinates.taskSectionNumber))

        val distinctCoordinates: List[Coordinates] = allSectionNumbers.flatMap(_.maybeCoordinates).distinct

        val isFirstSectionNumber: Set[SectionNumber] = distinctCoordinates.flatMap { coordinates =>
          allSectionNumbers.find(_.maybeCoordinates.contains(coordinates))
        }.toSet

        val firstSectionNumbers: List[SectionNumber] = allSectionNumbers.filter(isFirstSectionNumber)

        val taskListPages: List[(PageModel[DataExpanded], SectionNumber.TaskList)] =
          pages.toList.collect {
            case (pageModel, c @ SectionNumber.TaskList(coordinates, _)) if coordinates === taskList.coordinates =>
              pageModel -> c
          }

        val tasks: List[uk.gov.hmrc.gform.sharedmodel.formtemplate.Task] =
          TaskListUtils.withTaskSection(formTemplate, taskList.coordinates.taskSectionNumber)(section =>
            section.tasks.toList
          )

        val tasksWithFirstSectionNumber = tasks.zip(firstSectionNumbers)

        val specimenLinks: SpecimenLinks = SpecimenLinks.from(taskListPages.map(_._2.sectionNumber), classic)

        specimen.navigation_tasklist(
          formTemplate,
          taskList,
          taskListPages,
          tasksWithFirstSectionNumber,
          specimenLinks
        )
      }
    } else HtmlFormat.empty

  def renderSummarySectionDeclaration(
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    maybeAccessCode: Option[AccessCode],
    summarySection: SummarySection,
    validationResult: ValidationResult
  )(implicit
    request: RequestHeader,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html = {
    val formTemplate = cache.formTemplate
    val formTemplateId = formTemplate._id
    val envelopeId = cache.form.envelopeId
    val retrievals = cache.retrievals

    val page = summarySection.toPage
    val ei = ExtraInfo(
      Singleton(page.asInstanceOf[Page[DataExpanded]]),
      maybeAccessCode,
      formTemplate.sectionNumberZero,
      formModelOptics,
      formTemplate,
      envelopeId,
      EnvelopeWithMapping.empty,
      0,
      retrievals,
      formLevelHeading = false,
      specialAttributes = Map.empty,
      AddressRecordLookup.from(cache.form.thirdPartyData)
    )

    val snippets = page.renderUnits.map(renderUnit =>
      htmlFor(
        renderUnit,
        formTemplateId,
        ei,
        validationResult,
        obligations = NotChecked,
        UpscanInitiate.empty,
        Map.empty[FormComponentId, UpscanData]
      )
    )
    HtmlFormat.fill(snippets)
  }

  def renderDeclarationSection(
    maybeAccessCode: Option[AccessCode],
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
      formTemplate.sectionNumberZero,
      formModelOptics,
      formTemplate,
      EnvelopeId(""),
      EnvelopeWithMapping.empty,
      0,
      retrievals,
      formLevelHeading = false,
      specialAttributes = Map.empty,
      AddressRecordLookup.from(ThirdPartyData.empty)
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
      htmlFor(
        renderUnit,
        formTemplate._id,
        ei,
        validationResult,
        obligations = NotChecked,
        UpscanInitiate.empty,
        Map.empty[FormComponentId, UpscanData]
      )
    )
    val pageLevelErrorHtml = PageLevelErrorHtml.generatePageLevelErrorHtml(listResult, List.empty)
    val renderingInfo = SectionRenderingInformation(
      formTemplate._id,
      maybeAccessCode,
      formTemplate.sectionNumberZero,
      declarationPage.sectionHeader(),
      declarationPage.noPIITitle.fold(
        declarationPage.title.valueWithoutInterpolations(
          formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_)
        )
      )(_.value()),
      snippets,
      "",
      EnvelopeId(""),
      uk.gov.hmrc.gform.gform.routes.DeclarationController
        .submitDeclaration(formTemplate._id, maybeAccessCode, uk.gov.hmrc.gform.controllers.Continue),
      false,
      continueLabel,
      0,
      FileInfoConfig.allAllowedFileTypes,
      Nil,
      Map.empty,
      Map.empty,
      None,
      false
    )
    val mainForm = html.form.form_standard(
      renderingInfo,
      shouldDisplayContinue = true,
      ei.saveAndComeBackLaterButton,
      isFileUploadOnlyPage = false,
      None
    )
    html.form.form(
      formTemplate,
      pageLevelErrorHtml,
      renderingInfo,
      mainForm,
      backLink = Some(mkBackLinkDeclaration(formTemplate, maybeAccessCode, None, None)),
      shouldDisplayHeading = true,
      frontendAppConfig,
      fastForward = List(FastForward.Yes),
      accessCode = maybeAccessCode
    )
  }

  def mkBackLinkDeclaration(
    formTemplate: FormTemplate,
    maybeAccessCode: Option[AccessCode],
    coordinates: Option[Coordinates],
    taskCompleted: Option[Boolean]
  )(implicit messages: Messages): BackLink = {
    val href =
      uk.gov.hmrc.gform.gform.routes.SummaryController
        .summaryById(formTemplate._id, maybeAccessCode, coordinates, taskCompleted)
        .url
    new BackLink(href = href, content = new content.Text(messages("linkText.back")))
  }

  def renderTaskDeclarationSection(
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate,
    singleton: Singleton[DataExpanded],
    retrievals: MaterialisedRetrievals,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    coordinates: Coordinates,
    taskCompleted: Option[Boolean]
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html = {

    val ei = ExtraInfo(
      singleton,
      maybeAccessCode,
      formTemplate.sectionNumberZero,
      formModelOptics,
      formTemplate,
      EnvelopeId(""),
      EnvelopeWithMapping.empty,
      0,
      retrievals,
      formLevelHeading = false,
      specialAttributes = Map.empty,
      AddressRecordLookup.from(ThirdPartyData.empty)
    )

    val declarationPage = singleton.page

    val continueLabel = declarationPage.continueLabel.fold(messages("button.continue"))(_.value())

    val snippets = declarationPage.renderUnits.map(renderUnit =>
      htmlFor(
        renderUnit,
        formTemplate._id,
        ei,
        ValidationResult.empty,
        obligations = NotChecked,
        UpscanInitiate.empty,
        Map.empty[FormComponentId, UpscanData]
      )
    )
    val renderingInfo = SectionRenderingInformation(
      formTemplate._id,
      maybeAccessCode,
      formTemplate.sectionNumberZero,
      declarationPage.sectionHeader(),
      declarationPage.noPIITitle.fold(
        declarationPage.title.valueWithoutInterpolations(
          formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_)
        )
      )(_.value()),
      snippets,
      "",
      EnvelopeId(""),
      uk.gov.hmrc.gform.tasklist.routes.TaskListController.landingPage(formTemplate._id, maybeAccessCode),
      false,
      continueLabel,
      0,
      FileInfoConfig.allAllowedFileTypes,
      Nil,
      Map.empty,
      Map.empty,
      None,
      false
    )
    val mainForm = html.form.form_standard(
      renderingInfo,
      shouldDisplayContinue = true,
      ei.saveAndComeBackLaterButton,
      isFileUploadOnlyPage = false,
      None
    )
    html.form.form(
      formTemplate,
      NoErrors,
      renderingInfo,
      mainForm,
      backLink = Some(
        mkBackLinkDeclaration(
          formTemplate,
          maybeAccessCode,
          Some(coordinates),
          taskCompleted
        )
      ),
      shouldDisplayHeading = true,
      frontendAppConfig,
      fastForward = List(FastForward.Yes),
      accessCode = maybeAccessCode
    )
  }

  def mkBackLink(
    formTemplate: FormTemplate,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    originSection: SectionNumber,
    fastForward: List[FastForward],
    pageHasError: Boolean
  )(implicit messages: Messages): Option[BackLink] = {

    val href = fastForward match {
      case FastForward.CYA(_) :: xs if !pageHasError =>
        uk.gov.hmrc.gform.gform.routes.FormController
          .backAction(formTemplate._id, maybeAccessCode, sectionNumber, fastForward)
      case _ =>
        uk.gov.hmrc.gform.gform.routes.FormController
          .backAction(
            formTemplate._id,
            maybeAccessCode,
            sectionNumber,
            FastForward.StopAt(sectionNumber) :: fastForward
          )
    }

    val backLink =
      new BackLink(href = href.path, content = content.Text(messages("linkText.back")))

    if (sectionNumber > originSection || sectionNumber.isTaskList) {
      Some(backLink)
    } else
      fastForward match {
        case FastForward.CYA(_) :: xs if !pageHasError => Some(backLink)
        case _                                         => None
      }
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
    cache: AuthCacheWithForm,
    destinationList: DestinationList,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html = {
    val formTemplate = cache.formTemplate
    val formTemplateId = cache.form.formTemplateId
    val retrievals = cache.retrievals
    val envelopeId = cache.form.envelopeId

    val ackSection = destinationList.acknowledgementSection.toSection
    val ei = ExtraInfo(
      Singleton(ackSection.page.asInstanceOf[Page[DataExpanded]]),
      maybeAccessCode,
      formTemplate.sectionNumberZero,
      formModelOptics,
      formTemplate,
      envelopeId,
      EnvelopeWithMapping.empty,
      0,
      retrievals,
      formLevelHeading = false,
      specialAttributes = Map.empty,
      AddressRecordLookup.from(cache.form.thirdPartyData)
    )

    val formCategory = formTemplate.formCategory
    val acknowledgementSection = destinationList.acknowledgementSection
    val panelTitle = acknowledgementSection.title.map(_.value())
    val showReference = acknowledgementSection.showReference
    val showBanner = acknowledgementSection.showBanner
    val heading =
      acknowledgementSection.noPIITitle.fold(
        acknowledgementSection.title.map(_.value()).getOrElse(acknowledgementHeading(formCategory))
      )(noPIITitle => noPIITitle.value())
    val acknowledgementPanel = renderAcknowledgementPanel(
      panelTitle,
      showReference,
      formCategory,
      envelopeId,
      heading
    )
    val snippets = destinationList.acknowledgementSection.toPage.renderUnits.map(renderUnit =>
      htmlFor(
        renderUnit,
        formTemplateId,
        ei,
        ValidationResult.empty,
        obligations = NotChecked,
        UpscanInitiate.empty,
        Map.empty[FormComponentId, UpscanData]
      )
    )

    uk.gov.hmrc.gform.views.html.hardcoded.pages.partials
      .acknowledgement(
        formTemplateId,
        snippets,
        acknowledgementPanel,
        heading,
        formTemplate,
        frontendAppConfig,
        maybeAccessCode,
        showBanner
      )
  }

  def renderAcknowledgementPanel(
    panelTitle: Option[String],
    showReference: Boolean,
    formCategory: FormCategory,
    envelopeId: EnvelopeId,
    heading: String
  )(implicit
    messages: Messages
  ): Html = {
    val govukPanel = new GovukPanel()
    val htmlContent: Content =
      if (showReference) {
        HtmlContent(
          uk.gov.hmrc.gform.views.html.hardcoded.pages.partials.submission_reference(SubmissionRef(envelopeId))
        )
      } else {
        Empty
      }
    govukPanel(
      Panel(
        title = content.Text(panelTitle.getOrElse(heading)),
        content = htmlContent
      )
    )
  }

  def acknowledgementHeading(formCategory: FormCategory)(implicit messages: Messages): String = {
    val formCat = messages(s"formCategory.${formCategory.getString}")
    messages("ack.title", formCat.capitalize)
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
      formTemplate.sectionNumberZero,
      formModelOptics,
      formTemplate,
      EnvelopeId(""),
      EnvelopeWithMapping.empty,
      0,
      emptyRetrievals,
      formLevelHeading = false,
      specialAttributes = Map.empty,
      AddressRecordLookup.from(ThirdPartyData.empty)
    )
    val page = singleton.page
    val listResult = validationResult.formFieldValidationResults(singleton)
    val snippets =
      page.renderUnits.map { renderUnit =>
        htmlFor(
          renderUnit,
          formTemplate._id,
          ei,
          validationResult,
          obligations = NotChecked,
          UpscanInitiate.empty,
          Map.empty[FormComponentId, UpscanData]
        )
      }
    val pageLevelErrorHtml = PageLevelErrorHtml.generatePageLevelErrorHtml(listResult, globalErrors)
    val renderingInfo = SectionRenderingInformation(
      formTemplate._id,
      maybeAccessCode,
      formTemplate.sectionNumberZero,
      page.sectionHeader(),
      page.noPIITitle.fold(page.title.value())(_.value()),
      snippets,
      "",
      EnvelopeId(""),
      uk.gov.hmrc.gform.gform.routes.EnrolmentController
        .submitEnrolment(formTemplate._id, uk.gov.hmrc.gform.controllers.Continue),
      false,
      SectionRenderingService.determineContinueLabelKey(
        retrievals.continueLabelKey,
        DraftRetrievalHelper.isNotPermitted(formTemplate, retrievals),
        page.continueLabel,
        false
      ),
      0,
      FileInfoConfig.allAllowedFileTypes,
      Nil,
      Map.empty,
      Map.empty,
      None,
      false
    )
    val mainForm = html.form.form_standard(
      renderingInfo,
      shouldDisplayContinue = true,
      ei.saveAndComeBackLaterButton,
      isFileUploadOnlyPage = false,
      None
    )
    html.form
      .form(
        formTemplate,
        pageLevelErrorHtml,
        renderingInfo,
        mainForm,
        None,
        true,
        frontendAppConfig,
        fastForward = List(FastForward.Yes),
        accessCode = maybeAccessCode
      )
  }

  private def isVisible(
    formComponent: FormComponent,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  ): Boolean = formComponent.includeIf.fold(true) { includeIf =>
    formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None)
  }

  private def htmlForUpscan(
    formComponent: FormComponent,
    ei: ExtraInfo,
    fields: Map[String, String]
  ): Html =
    if (!isVisible(formComponent, ei.formModelOptics) || formComponent.onlyShowOnSummary) HtmlFormat.empty
    else {
      val hiddenFields: List[Html] = fields.toList.map { case (name, value) =>
        html.form.snippets.hidden(name, value)
      }
      HtmlFormat.fill(hiddenFields)
    }

  def htmlFor(
    renderUnit: RenderUnit,
    formTemplateId: FormTemplateId,
    ei: ExtraInfo,
    validationResult: ValidationResult,
    obligations: Obligations,
    upscanInitiate: UpscanInitiate,
    upscanData: Map[FormComponentId, UpscanData]
  )(implicit
    request: RequestHeader,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html =
    renderUnit.fold { case RenderUnit.Pure(formComponent) =>
      if (!isVisible(formComponent, ei.formModelOptics) || formComponent.onlyShowOnSummary) {
        HtmlFormat.empty
      } else {

        formComponent.`type` match {
          case Group(_, _, _, _, _) =>
            throw new IllegalArgumentException(s"Group '${formComponent.id}' cannot be rendered as RenderUnit.Pure")
          case Date(_, offset, dateValue) =>
            htmlForDate(formComponent, offset, dateValue, validationResult, ei)
          case CalendarDate =>
            htmlForCalendarDate(formComponent, validationResult, ei)
          case p @ PostcodeLookup(_, _, _) =>
            htmlForPostcodeLookup(formComponent, validationResult, ei)
          case TaxPeriodDate =>
            htmlForTaxPeriodDate(formComponent, validationResult, ei)
          case t @ Time(_, _) =>
            renderTime(t, formComponent, validationResult, ei)
          case a @ Address(_, _, countyDisplayed, _) =>
            htmlForAddress(formComponent, a, validationResult, ei, countyDisplayed)
          case o @ OverseasAddress(_, _, _, _, _, _) =>
            htmlForOverseasAddress(formComponent, o, validationResult, ei)
          case t @ Text(Lookup(register, _), _, _, _, _, _, _) =>
            renderLookup(t, formComponent, register, validationResult, ei)
          case t @ Text(_, _, _, _, _, _, _) =>
            renderText(t, formComponent, validationResult, ei)
          case t @ TextArea(_, _, _, _, _, _) =>
            renderTextArea(t, formComponent, validationResult, ei)
          case Choice(
                choice,
                options,
                orientation,
                selections,
                hints,
                optionalHelpText,
                dividerPosition,
                dividerText,
                noneChoice,
                _,
                hideChoicesSelected
              ) =>
            htmlForChoice(
              formComponent,
              choice,
              options,
              orientation,
              selections,
              hints,
              optionalHelpText,
              validationResult,
              ei,
              dividerPosition,
              dividerText,
              noneChoice,
              hideChoicesSelected
            )
          case RevealingChoice(options, multiValue) =>
            htmlForRevealingChoice(
              formComponent,
              formTemplateId,
              multiValue,
              options,
              validationResult,
              ei,
              obligations,
              upscanInitiate,
              upscanData,
              ei.formModelOptics
            )
          case FileUpload(_, allowedFileTypes) =>
            val allowedContentTypes = allowedFileTypes
              .map(_.contentTypes)
              .getOrElse(ei.formTemplate.allowedFileTypes.contentTypes)
              .toList
              .map(_.value)
              .mkString(", ")
            val additionalAttributes = Map("accept" -> allowedContentTypes)

            ei.isFileUploadOnlyPage(validationResult) match {
              case None =>
                htmlForFileUploadStandard(
                  formComponent,
                  ei,
                  validationResult,
                  upscanData,
                  additionalAttributes
                )
              case Some(_) =>
                htmlForFileUploadSingle(formComponent, ei, upscanData, additionalAttributes)
            }

          case mfu @ MultiFileUpload(_, allowedFileTypes, _, _, _, _, _) =>
            val allowedContentTypes = allowedFileTypes
              .map(_.contentTypes)
              .getOrElse(ei.formTemplate.allowedFileTypes.contentTypes)
              .toList
              .map(_.value)
              .mkString(", ")
            val additionalAttributes = Map("accept" -> allowedContentTypes)

            htmlForMultiFileUploadStandard(
              formComponent,
              ei,
              validationResult,
              upscanData,
              additionalAttributes,
              mfu
            )

          case InformationMessage(infoType, infoText, _) =>
            htmlForInformationMessage(formComponent, infoType, infoText)
          case htp @ HmrcTaxPeriod(idType, idNumber, regimeType) =>
            htmlForHmrcTaxPeriod(
              formComponent,
              ei,
              validationResult,
              obligations,
              htp,
              ei.formModelOptics.formModelVisibilityOptics
            )
          case MiniSummaryList(rows, _, keyDisplayWidth) =>
            htmlForMiniSummaryList(
              formComponent,
              formTemplateId,
              rows,
              ei,
              validationResult,
              obligations,
              keyDisplayWidth.getOrElse(KeyDisplayWidth.S)
            )
          case t: TableComp => htmlForTableComp(formComponent, t, ei.formModelOptics)
          case b: Button    => htmlForButton(formComponent, b, ei)
        }
      }
    } { case r @ RenderUnit.Group(_, _) =>
      htmlForGroup(r, formTemplateId, ei, validationResult, obligations, upscanInitiate, upscanData)
    }

  private def htmlForFileUploadStandard(
    formComponent: FormComponent,
    ei: ExtraInfo,
    validationResult: ValidationResult,
    upscanData: Map[FormComponentId, UpscanData],
    additionalAttributes: Map[String, String]
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html = {
    val fileId: FileId = ei.getFileIdSingle(formComponent)

    val fileSize: Long =
      ei.envelope
        .findSingle(formComponent.modelComponentId)
        .map(_.length)
        .getOrElse(0)

    upscanData.get(formComponent.id) match {
      case None => throw new IllegalArgumentException(s"Unable to find upscanData for ${formComponent.id}")
      case Some(upscanData) =>
        val fileUploadName = "file"
        val attributes = Map("form" -> upscanData.formMetaData.htmlId) ++ additionalAttributes

        htmlForFileUpload(
          formComponent,
          ei.formTemplateId,
          ei,
          validationResult,
          fileId,
          fileSize,
          fileUploadName,
          upscanData.url,
          attributes
        )
    }
  }

  private def htmlForFileUploadSingle(
    formComponent: FormComponent,
    ei: ExtraInfo,
    upscanData: Map[FormComponentId, UpscanData],
    additionalAttributes: Map[String, String]
  )(implicit
    request: RequestHeader,
    sse: SmartStringEvaluator,
    m: Messages
  ): Html =
    upscanData.get(formComponent.id) match {
      case None => throw new IllegalArgumentException(s"Unable to find upscanData for ${formComponent.id}")
      case Some(upscanData) =>
        val fileUploadName = "file"
        htmlForFileUploadOnly(
          formComponent,
          fileUploadName,
          ei,
          upscanData.snippets,
          additionalAttributes
        )
    }

  private def htmlForMultiFileUploadStandard(
    formComponent: FormComponent,
    ei: ExtraInfo,
    validationResult: ValidationResult,
    upscanData: Map[FormComponentId, UpscanData],
    additionalAttributes: Map[String, String],
    multiFileUpload: MultiFileUpload
  )(implicit
    request: RequestHeader,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Html =
    upscanData.get(formComponent.id) match {
      case None => throw new IllegalArgumentException(s"Unable to find upscanData for ${formComponent.id}")
      case Some(upscanData) =>
        val fileUploadName = "file"
        val attributes = Map("form" -> upscanData.formMetaData.htmlId) ++ additionalAttributes

        htmlForMultiFileUpload(
          formComponent,
          ei.formTemplateId,
          ei,
          validationResult,
          fileUploadName,
          upscanData.url,
          attributes,
          multiFileUpload
        )
    }

  private def htmlForHmrcTaxPeriod[D <: DataOrigin](
    formComponent: FormComponent,
    ei: ExtraInfo,
    validationResult: ValidationResult,
    obligations: Obligations,
    hmrcTP: HmrcTaxPeriod,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
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

    val setValue =
      TaxPeriodHelper.formatTaxPeriodOutput(formFieldValidationResult, ei.envelope, formModelVisibilityOptics)
    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage = errors.map(error =>
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

    val label = formComponent.label.value()

    val isPageHeading = ei.formLevelHeading

    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = content.Text(label),
            isPageHeading = isPageHeading,
            classes = getLegendClasses(isPageHeading, formComponent.labelSize)
          )
        )
      )
    )

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
        hint = hintText(formComponent),
        errorMessage = errorMessage,
        name = formComponent.id.value,
        items = items.toList
      )

      new components.GovukRadios(govukFieldset, govukHint, govukLabel, govukFormGroup, govukHintAndErrorMessage)(radios)

    }

    val warningText = WarningText(content = content.Text(messages("taxPeriod.noResults.warning")))

    val labelContent =
      if (isPageHeading) {
        content.HtmlContent(s"""<h1 class="govuk-heading-l">$label</h1>""")
      } else {
        content.HtmlContent(s"""<p class="govuk-body">$label</p>""")
      }

    maybeTaxPeriodOptions.fold(html.form.snippets.no_open_tax_period(labelContent, warningText))(renderOptions)
  }

  def htmlForInformationMessage(
    formComponent: FormComponent,
    infoType: InfoType,
    infoText: SmartString
  )(implicit messages: Messages, sse: SmartStringEvaluator) =
    html.form.snippets.field_template_info(
      formComponent,
      infoType,
      markDownParser(infoText),
      getLabelClasses(false, formComponent.labelSize)
    )

  private def htmlForMiniSummaryList(
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    rows: List[MiniSummaryRow],
    ei: ExtraInfo,
    validationResult: ValidationResult,
    obligations: Obligations,
    keyDisplayWidth: KeyDisplayWidth
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator,
    l: LangADT,
    fcrd: FormComponentRenderDetails[SummaryRender]
  ): Html = {
    def renderRows(rows: List[MiniSummaryRow], keyDisplayWidth: KeyDisplayWidth) = {

      def summaryListRowByPageId(key: Option[SmartString], value: String, pageId: PageId) = {
        val formModel = ei.formModelOptics.formModelVisibilityOptics.formModel
        val sn: SectionNumber = formModel.pageIdSectionNumberMap.toList
          .sortBy(_._1.maybeIndex)(Ordering[Option[Int]].reverse)
          .find { case (modelPageId, _) =>
            modelPageId.baseId == pageId.modelPageId.baseId
          }
          .fold(throw new Exception(s"No section number found for pageId ${pageId.id}")) { case (_, sectionNumber) =>
            sectionNumber
          }

        val pageModel = formModel.pageModelLookup(sn)
        val pageHasEditableField = pageModel.allFormComponents.exists(_.editable)

        val sectionTitle4Ga = sectionTitle4GaFactory(pageModel, sn)
        List(
          SummaryListRowHelper.summaryListRow(
            key.map(sse(_, false)).getOrElse(fcrd.label(formComponent)),
            Html(value),
            Some(""),
            SummaryListRowHelper.getKeyDisplayWidthClass(keyDisplayWidth),
            "",
            "",
            List(
              (
                uk.gov.hmrc.gform.gform.routes.FormController
                  .form(
                    formTemplateId,
                    ei.maybeAccessCode,
                    sn,
                    sectionTitle4Ga,
                    SuppressErrors.Yes,
                    List(FastForward.CYA(SectionOrSummary.Section(ei.sectionNumber)))
                  ),
                messages(if (pageHasEditableField) "summary.change" else "summary.view"),
                ""
              )
            ),
            ""
          )
        )
      }

      def summaryListRowByTaskId(key: Option[SmartString], value: String, taskId: TaskId) = {

        val taskCoordinatesMap: Map[TaskId, (Task, Coordinates)] =
          ei.formTemplate.formKind.fold(_ => Map.empty[TaskId, (Task, Coordinates)]) { _ =>
            TaskListUtils.toTaskCoordinatesMap(ei.formTemplate).collect {
              case (task, coordinates) if task.id.isDefined => task.id.get -> (task, coordinates)
            }
          }

        val link = taskCoordinatesMap
          .get(taskId)
          .map { case (task, coordinates) =>
            if (task.summarySection.isDefined) {
              uk.gov.hmrc.gform.gform.routes.SummaryController
                .summaryById(formTemplateId, ei.maybeAccessCode, Some(coordinates), None)
            } else {
              uk.gov.hmrc.gform.tasklist.routes.TaskListController
                .newTask(
                  formTemplateId,
                  ei.maybeAccessCode,
                  coordinates.taskSectionNumber,
                  coordinates.taskNumber,
                  true
                )
            }
          }
          .getOrElse(
            throw new Exception(
              s"TaskId $taskId not found in taskIdCoordinateMap. Available taskIds: " + taskCoordinatesMap.keys + "."
                .mkString(", ")
            )
          )

        List(
          SummaryListRowHelper.summaryListRow(
            key.map(sse(_, false)).getOrElse(fcrd.label(formComponent)),
            Html(value),
            Some(""),
            SummaryListRowHelper.getKeyDisplayWidthClass(keyDisplayWidth),
            "",
            "",
            List(
              (
                link,
                messages("summary.change"),
                ""
              )
            ),
            ""
          )
        )
      }

      val formattedExprStr = (e: Expr) =>
        MiniSummaryListHelper.getFormattedExprStr(ei.formModelOptics.formModelVisibilityOptics, e)

      val slRows = rows.flatMap {
        case ValueRow(key, MiniSummaryListValue.AnyExpr(e), _, Some(pageId), _) =>
          summaryListRowByPageId(key, formattedExprStr(e), pageId)
        case ValueRow(key, MiniSummaryListValue.AnyExpr(e), _, _, Some(taskId)) =>
          summaryListRowByTaskId(key, formattedExprStr(e), taskId)
        case ValueRow(key, MiniSummaryListValue.AnyExpr(e), _, _, _) =>
          List(
            SummaryListRowHelper.summaryListRow(
              key.map(sse(_, false)).getOrElse(fcrd.label(formComponent)),
              Html(
                formattedExprStr(e) + MiniSummaryListHelper
                  .checkAndReturnSuffix(e, ei.formModelOptics.formModelVisibilityOptics.formModel)
              ),
              Some(""),
              SummaryListRowHelper.getKeyDisplayWidthClass(keyDisplayWidth),
              "",
              "",
              List(),
              "govuk-summary-list__row--no-actions"
            )
          )

        case ValueRow(key, MiniSummaryListValue.Reference(FormCtx(formComponentId)), _, _, _) =>
          val formModel = ei.formModelOptics.formModelVisibilityOptics.formModel

          formModel.sectionNumberLookup
            .get(formComponentId)
            .map { sn =>
              val sectionTitle4Ga = sectionTitle4GaFactory(formModel.pageModelLookup(sn), sn)
              val fc = formModel.fcLookup(formComponentId)
              val fcUpdated = key.map(k => fc.copy(shortName = Some(k))).getOrElse(fc)
              FormComponentSummaryRenderer
                .summaryListRows[DataOrigin.Mongo, SummaryRender](
                  fcUpdated,
                  ei.singleton.page.id.map(_.modelPageId),
                  formTemplateId,
                  ei.formModelOptics.formModelVisibilityOptics,
                  ei.maybeAccessCode,
                  sn,
                  sectionTitle4Ga,
                  obligations,
                  validationResult,
                  ei.envelope,
                  ei.addressRecordLookup,
                  None,
                  Some(List(FastForward.CYA(SectionOrSummary.Section(ei.sectionNumber)))),
                  keyDisplayWidth
                )
            }
            .toList
            .flatten
        case SmartStringRow(key, ss, _, Some(pageId), _) => summaryListRowByPageId(key, ss.value(), pageId)
        case SmartStringRow(key, ss, _, _, Some(taskId)) => summaryListRowByTaskId(key, ss.value(), taskId)
        case SmartStringRow(key, ss, _, _, _) =>
          List(
            SummaryListRowHelper.summaryListRow(
              key.map(sse(_, false)).getOrElse(fcrd.label(formComponent)),
              Html(ss.value()),
              Some(""),
              SummaryListRowHelper.getKeyDisplayWidthClass(keyDisplayWidth),
              "",
              "",
              List(),
              "govuk-summary-list__row--no-actions"
            )
          )

        case HeaderRow(header)         => throw new Exception("should not have HeaderRow  here")
        case ATLRow(atlId, _, atlRows) => throw new Exception("should not have ATLRow here")
      }
      new GovukSummaryList()(SummaryList(slRows))
    }

    def renderedATLRows(atlId: AddToListId, atlRows: List[MiniSummaryRow]) = {
      val addToListIteration = ei.formModelOptics.formModelVisibilityOptics.formModel.brackets
        .addToListBracket(atlId)
        .iterations
      addToListIteration.toList.map { iteration =>
        val index = iteration.repeater.repeater.index
        val baseIds: List[FormComponentId] = iteration.singletons.toList.flatMap { singletonWithNumber =>
          val page = singletonWithNumber.singleton.page
          page.fields.filterNot(_.hideOnSummary).map(fId => FormComponentId(fId.baseComponentId.value))
        }
        def updatedIncludeIf(mayBeIncludeIf: Option[IncludeIf]) =
          mayBeIncludeIf.map(iIf => IncludeIf(BooleanExprUpdater(iIf.booleanExpr, index, baseIds)))

        val updatedATLRows = atlRows.map {
          case ValueRow(key, MiniSummaryListValue.Reference(fCtx), includeIf, pageId, taskId) =>
            ValueRow(
              key.map(_.expand(index, baseIds)),
              MiniSummaryListValue.Reference(ExprUpdater.formCtx(fCtx, index, baseIds)),
              updatedIncludeIf(includeIf),
              pageId,
              taskId
            )

          case HeaderRow(h) => HeaderRow(h.expand(index, baseIds))
          case ValueRow(key, MiniSummaryListValue.AnyExpr(expr), includeIf, pageId, taskId) =>
            ValueRow(
              key.map(_.expand(index, baseIds)),
              MiniSummaryListValue.AnyExpr(ExprUpdater(expr, index, baseIds)),
              updatedIncludeIf(includeIf),
              pageId,
              taskId
            )
          case e => e
        }
        htmlForMiniSummaryList(
          formComponent,
          formTemplateId,
          updatedATLRows,
          ei,
          validationResult,
          obligations,
          keyDisplayWidth
        )
      }
    }
    val visibleRows: List[MiniSummaryRow] = rows
      .filter(r => isVisibleMiniSummaryListRow(r, ei.formModelOptics))
    val visibleRowsPartitioned: List[List[MiniSummaryRow]] = visibleRows
      .foldLeft(List.empty[List[MiniSummaryRow]])((acc, row) =>
        row match {
          case _: HeaderRow => List(row) :: acc
          case _: ATLRow    => List(row) :: acc
          case _: SmartStringRow | _: ValueRow =>
            acc match {
              case Nil          => List(List(row))
              case head :: tail => (row :: head) :: tail
            }
        }
      )
      .reverse
      .map(_.reverse)
    val htmls = visibleRowsPartitioned.map {
      case HeaderRow(h) :: xs => HtmlFormat.fill(List(header(Html(sse(h, false))), renderRows(xs, keyDisplayWidth)))
      case ATLRow(atlId, _, atlRows) :: xs =>
        HtmlFormat.fill(renderedATLRows(AddToListId(atlId), atlRows) ++ List(renderRows(xs, keyDisplayWidth)))
      case xs => renderRows(xs, keyDisplayWidth)

    }
    HtmlFormat.fill(htmls)
  }

  private def htmlForTableComp(
    formComponent: FormComponent,
    table: TableComp,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    sse: SmartStringEvaluator,
    l: LangADT
  ): Html = {
    def isVisibleValueRow(
      row: TableValueRow
    ): Boolean = row.includeIf.fold(true)(includeIf =>
      formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None)
    )
    val formModel = formModelOptics.formModelVisibilityOptics.formModel

    def isNumericFormComponentRef(expr: Expr): Boolean = expr match {
      case FormCtx(formComponentId) =>
        formModel.fcLookup.get(formComponentId).exists(_.isNumeric)
      case _ => false
    }

    def isNumeric(v: TableValue): Boolean = {
      val interpolationsAreNumeric =
        v.value.interpolations(formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_)) match {
          case List(Typed(_, ExplicitExprType.Sterling(_)))     => true
          case List(Typed(_, ExplicitExprType.Number(_, _)))    => true
          case exprs if exprs.forall(isNumericFormComponentRef) => true // !! interpolations contain both en and cy data
          case _                                                => false
        }
      interpolationsAreNumeric && v.value
        .valueWithoutInterpolations(formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_))
        .trim
        .isEmpty()
    }

    val normalisedTable = SectionRenderingService.normaliseTableComp(table, isVisibleValueRow)

    val filteredRows = normalisedTable.rows.collect {
      case valueRow: TableValueRow if isVisibleValueRow(valueRow) =>
        valueRow.values.map { v =>
          val classes = v.cssClass.toList.flatMap(_.split(" +")) :+ {
            if (isNumeric(v)) "govuk-table__cell--numeric" else ""
          }
          GovukTableRow(
            content = HtmlContent(sse(v.value, false)),
            colspan = v.colspan,
            rowspan = v.rowspan,
            classes = classes.mkString(" ")
          )
        }
    }

    val headerNumericClasses = table.rows
      .find(isVisibleValueRow)
      .map(_.values.map(v => if (isNumeric(v)) "govuk-table__header--numeric" else ""))
      .getOrElse(List())

    val headCells = table.header.zipWithIndex.map { case (header, index) =>
      HeadCell(
        content = HtmlContent(sse(header.label, false)),
        classes = header.classes.getOrElse(headerNumericClasses.lift(index).getOrElse(""))
      )
    }

    val caption: Option[String] = table.caption.orElse(Some(formComponent.label.value()))

    val captionClasses = {
      val c = table.captionClasses.trim
      val labelClass = formComponent.labelSize.fold("") {
        case ExtraLarge => "govuk-table__caption--xl"
        case Large      => "govuk-table__caption--l"
        case Medium     => "govuk-table__caption--m"
        case Small      => "govuk-table__caption--s"
        case ExtraSmall => "govuk-table__caption--s"
      }
      val captionClasses = if (c.isEmpty && formComponent.labelSize.isEmpty) "govuk-table__caption--m" else c
      captionClasses + " " + labelClass
    }
    new GovukTable()(
      Table(
        rows = filteredRows,
        head = Some(headCells),
        caption = caption,
        captionClasses = captionClasses,
        classes = table.classes,
        firstCellIsHeader = table.firstCellIsHeader
      )
    )
  }

  private def htmlForButton(
    formComponent: FormComponent,
    btn: Button,
    ei: ExtraInfo
  )(implicit
    request: RequestHeader,
    messages: Messages,
    sse: SmartStringEvaluator
  ): Html = {

    val label = content.Text(formComponent.label.value())

    val reference = ei.formModelOptics.formModelVisibilityOptics
      .evalAndApplyTypeInfoFirst(btn.reference)
      .stringRepresentation

    val amountInPence = ei.formModelOptics.formModelVisibilityOptics
      .evalAndApplyTypeInfoFirst(btn.amountInPence)
      .stringRepresentation

    val govukButton = GovukButton(
      id = Some(formComponent.id.value),
      inputType = Some("submit"),
      name = Some(formComponent.id.value),
      content = label,
      classes = btn.classes.getOrElse(""),
      isStartButton = btn.isStartButton,
      attributes = Map(
        "formaction" -> uk.gov.hmrc.gform.payment.routes.PaymentController
          .pay(ei.formTemplateId, ei.maybeAccessCode, PaymentReference(reference), amountInPence, request.uri)
          .url
      )
    )

    new components.GovukButton()(govukButton)

  }

  private def htmlForFileUpload(
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    ei: ExtraInfo,
    validationResult: ValidationResult,
    fileId: FileId,
    fileSize: Long,
    fileUploadName: String,
    formAction: String,
    attributes: Map[String, String]
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ) = {

    val formFieldValidationResult = validationResult(formComponent)

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage = errors.map(error =>
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

    val currentValue = formFieldValidationResult.getCurrentValue.filterNot(_ === "").map(HtmlFormat.escape(_).body)

    val labelContent = content.Text(formComponent.label.value())

    val isPageHeading = ei.formLevelHeading

    val singleton = ei.singleton
    val page = singleton.page
    val formLevelHeading = shouldDisplayHeading(singleton)

    val label = Label(
      isPageHeading = isPageHeading,
      classes = getLabelClasses(isPageHeading, formComponent.labelSize),
      content = labelContent
    )

    val fileUpload: fileupload.FileUpload = fileupload.FileUpload(
      id = formComponent.id.value,
      name = fileUploadName,
      label = label,
      hint = hintText(formComponent),
      errorMessage = errorMessage,
      attributes = attributes
    )

    val deleteUrl =
      uk.gov.hmrc.gform.objectStore.routes.ObjectStoreController.requestRemoval(
        formTemplateId,
        ei.maybeAccessCode,
        ei.sectionNumber,
        FileComponentId.Single(formComponent.id)
      )

    val fileInput: Html =
      new components.GovukFileUpload(govukLabel, govukFormGroup, govukHintAndErrorMessage)(fileUpload)

    val submitButton: GovukButton = GovukButton(
      name = Some(s"${formComponent.id}-uploadButton"),
      content = content.Text(messages("file.upload")),
      inputType = Some("submit"),
      classes = "govuk-button--secondary",
      attributes = Map(
        "formaction"  -> formAction,
        "formenctype" -> "multipart/form-data"
      ) ++ attributes,
      preventDoubleClick = Some(true)
    )

    val submitButtonHtml: Html = new components.GovukButton()(submitButton)

    val uploadedFiles: Html =
      html.form.snippets
        .uploaded_files(
          formComponent.id,
          page.sectionHeader(),
          formLevelHeading,
          fileId,
          currentValue,
          deleteUrl,
          FileUploadUtils.formatSize(fileSize)
        )

    currentValue match {
      case Some(v) =>
        val fileName = v.replace(fileId.value + "_", "")
        val hiddenInput = html.form.snippets.hidden(formComponent.id.value, fileName)
        HtmlFormat.fill(List(hiddenInput, uploadedFiles))
      case None => HtmlFormat.fill(List(fileInput, uploadedFiles, submitButtonHtml))
    }
  }

  private def htmlForMultiFileUpload(
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    ei: ExtraInfo,
    validationResult: ValidationResult,
    fileUploadName: String,
    formAction: String,
    attributes: Map[String, String],
    multiFileUpload: MultiFileUpload
  )(implicit
    request: RequestHeader,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ) = {
    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)

    val flashError: Option[String] = request.flash.get(GformFlashKeys.FileUploadError)
    val errors: Option[String] =
      if (flashError.isDefined)
        flashError
      else
        ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage: Option[ErrorMessage] = errors.map(error =>
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

    val currentValues: List[(FileId, String, Call)] = formFieldValidationResult match {
      case ComponentField(_, data) =>
        data.toList
          .sortBy { case (htmlFieldId, _) =>
            htmlFieldId.fold(_ =>
              throw new Exception(s"Invalid fileComponentId representation. Index is needed $htmlFieldId")
            )(indexed =>
              indexed.index.toIntOption
                .getOrElse(throw new Exception(s"Failed to convert fileComponentId index to a number $indexed"))
            )
          }
          .map { case (htmlFieldId, validationResult) =>
            val fileComponentId = FileComponentId.fromHtmlFieldId(htmlFieldId)
            val fileId = ei.envelope.fileIdFor(fileComponentId)
            val deleteUrl =
              uk.gov.hmrc.gform.objectStore.routes.ObjectStoreController.requestRemoval(
                formTemplateId,
                ei.maybeAccessCode,
                ei.sectionNumber,
                fileComponentId
              )
            (
              fileId,
              validationResult.getCurrentValue.getOrElse(""),
              deleteUrl
            )
          }
      case _ => List.empty[(FileId, String, Call)]
    }

    val isMinimumFilesUploaded: Boolean = ei.getButtonName(validationResult).isEmpty

    val labelContent: content.Text = if (currentValues.nonEmpty) {
      content.Text(multiFileUpload.uploadAnotherLabel.getOrElse(formComponent.label).value())
    } else {
      content.Text(formComponent.label.value())
    }

    val isPageHeading: Boolean = ei.formLevelHeading

    val label: Label = Label(
      isPageHeading = isPageHeading,
      classes = getLabelClasses(isPageHeading, formComponent.labelSize),
      content = labelContent
    )

    val fileUpload: fileupload.FileUpload = fileupload.FileUpload(
      id = formComponent.id.value,
      name = fileUploadName,
      label = label,
      hint = hintText(formComponent),
      errorMessage = errorMessage,
      attributes = attributes
    )

    val fileInput: Html =
      new components.GovukFileUpload(govukLabel, govukFormGroup, govukHintAndErrorMessage)(fileUpload)

    val submitButtonClasses: String = if (isMinimumFilesUploaded) {
      "govuk-button--secondary"
    } else {
      "govuk-button--secondary js-hidden"
    }

    val submitButton: GovukButton = GovukButton(
      name = Some(s"${formComponent.id}-uploadButton"),
      content = content.Text(messages("file.upload")),
      inputType = Some("submit"),
      classes = submitButtonClasses,
      attributes = Map(
        "formaction"  -> formAction,
        "formenctype" -> "multipart/form-data"
      ) ++ attributes,
      preventDoubleClick = Some(true)
    )

    val submitButtonHtml: Html = new components.GovukButton()(submitButton)

    val hint: Html =
      multiFileUpload.hint.map(hint => html.form.snippets.multi_file_hint(hint.value())).getOrElse(HtmlFormat.empty)

    val uploadedFiles: Html =
      html.form.snippets
        .uploaded_files_multi(
          formComponent.id,
          currentValues
        )

    val continueText: Html =
      multiFileUpload.continueText
        .map(continue => html.form.snippets.markdown_wrapper(HtmlFormat.raw(continue.value())))
        .getOrElse(HtmlFormat.empty)

    val maxFilesRequired: Int = ei.maxFilesRequired(multiFileUpload.maxFiles)
    val isMaximumFilesUploaded: Boolean = currentValues.size >= maxFilesRequired

    (isMinimumFilesUploaded, isMaximumFilesUploaded) match {
      case (true, true)  => HtmlFormat.fill(List(hint, uploadedFiles, continueText))
      case (true, false) => HtmlFormat.fill(List(hint, uploadedFiles, fileInput, submitButtonHtml, continueText))
      case (false, _)    => HtmlFormat.fill(List(hint, uploadedFiles, fileInput, submitButtonHtml))
    }
  }

  private def hasNotBeenSelectedYet(
    hideChoicesSelected: Boolean,
    optionData: OptionData,
    modelComponentId: ModelComponentId,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    m: Messages
  ): Boolean = if (!hideChoicesSelected) true
  else {
    val formModelVisibilityOptics = formModelOptics.formModelVisibilityOptics
    val allValues: Iterable[(ModelComponentId, VariadicValue)] =
      formModelVisibilityOptics.recData.variadicFormData.forBaseComponentId(modelComponentId.baseComponentId).filter {
        case (mcId, _) =>
          mcId =!= modelComponentId // Ignore itself, so user can edit it
      }
    val selectedValues: Set[String] = allValues.flatMap { case (_, vv) => vv.toSeq }.toSet
    optionData match {
      case OptionData.ValueBased(_, _, _, _, _, _) =>
        val value = optionData.getValue(-1, formModelVisibilityOptics)
        !selectedValues(value)
      case OptionData.IndexBased(_, _, _, _, _) =>
        true // Do not hide index based options
    }
  }

  private def isVisibleOption(
    optionData: OptionData,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  ): Boolean =
    optionData match {
      case OptionData.ValueBased(_, _, includeIf, _, value, _) =>
        includeIf.fold(true)(includeIf => formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None))
      case OptionData.IndexBased(_, _, includeIf, _, _) =>
        includeIf.fold(true)(includeIf => formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None))
    }

  private def optionHasContent(
    optionData: OptionData
  )(implicit sse: SmartStringEvaluator): Boolean =
    optionData.label.value().trim().nonEmpty

  private def isVisibleMiniSummaryListRow(
    row: MiniSummaryRow,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  ): Boolean = row match {
    case v: ValueRow =>
      v.includeIf.fold(true)(includeIf => formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None))
    case s: SmartStringRow =>
      s.includeIf.fold(true)(includeIf => formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None))
    case HeaderRow(header) => true
    case v: ATLRow =>
      v.includeIf.fold(true)(includeIf => formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None))
  }

  private def htmlForChoice(
    formComponent: FormComponent,
    choice: ChoiceType,
    options: NonEmptyList[OptionData],
    orientation: Orientation,
    selections: List[Int],
    hints: Option[NonEmptyList[SmartString]],
    optionalHelpText: Option[NonEmptyList[SmartString]],
    validationResult: ValidationResult,
    ei: ExtraInfo,
    dividerPosition: Option[DividerPosition],
    dividerText: LocalisedString,
    maybeNoneChoice: Option[NoneChoice],
    hideChoicesSelected: Boolean
  )(implicit
    l: LangADT,
    m: Messages,
    sse: SmartStringEvaluator
  ) = {
    val prepopValues =
      if (ei.formModelOptics.pageOpticsData.contains(formComponent.modelComponentId))
        Set.empty[String] // Don't prepop something we already submitted
      else selections.map(_.toString).toSet

    val visibleOptionsWithIndex: NonEmptyList[(OptionData, Int)] = options.zipWithIndex
      .filter { case (o, _) =>
        hasNotBeenSelectedYet(hideChoicesSelected, o, formComponent.modelComponentId, ei.formModelOptics) &&
          isVisibleOption(o, ei.formModelOptics) &&
          optionHasContent(o)
      }
      .toNel
      .getOrElse(throw new IllegalArgumentException("All options of the choice component are invisible"))

    val optionsWithHelpText: NonEmptyList[(OptionData, Option[Html])] =
      optionalHelpText
        .map(
          _.zipWith(visibleOptionsWithIndex.map(_._1))((helpText, option) =>
            (
              option,
              if (helpText.isEmpty(ei.formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_))) None
              else Some(markDownParser(helpText))
            )
          )
        )
        .getOrElse(visibleOptionsWithIndex.map(_._1).map(option => (option, None)))

    val optionsWithHintAndHelpText: NonEmptyList[(OptionData, Option[Hint], Option[Html])] =
      hints
        .flatMap(
          _.toList.zipWithIndex.filter(h => visibleOptionsWithIndex.map(_._2).toList.contains(h._2)).map(_._1).toNel
        )
        .map(_.zipWith(optionsWithHelpText) { case (hint, (option, helpText)) =>
          (
            option,
            if (hint.isEmpty(ei.formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_)))
              toHint(option.hint)
            else toHint(Some(hint)),
            helpText
          )
        })
        .getOrElse(optionsWithHelpText.map { case (option, helpText) => (option, toHint(option.hint), helpText) })

    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage = errors.map(error =>
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

    val isPageHeading = ei.formLevelHeading
    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = content.Text(formComponent.label.value()),
            isPageHeading = isPageHeading,
            classes = getLegendClasses(isPageHeading, formComponent.labelSize)
          )
        )
      )
    )

    def isChecked(index: String): Boolean =
      formFieldValidationResult
        .getOptionalCurrentValue(HtmlFieldId.indexed(formComponent.id, index))
        .orElse(prepopValues.find(_ === index))
        .isDefined

    def helpTextHtml(maybeHelpText: Option[Html]): Option[Html] =
      maybeHelpText.map(helpText => html.form.snippets.markdown_wrapper(helpText))

    choice match {
      case Radio | YesNo =>
        val itemsWithNoDivider = optionsWithHintAndHelpText.zipWithIndex.map {
          case ((option, maybeHint, maybeHelpText), index) =>
            RadioItem(
              id = Some(formComponent.id.value + index),
              value = Some(option.getValue(index, ei.formModelOptics.formModelVisibilityOptics)),
              content = content.Text(option.label.value()),
              checked = isChecked(option.getValue(index, ei.formModelOptics.formModelVisibilityOptics)),
              conditionalHtml = helpTextHtml(maybeHelpText),
              attributes = dataLabelAttribute(
                option.label,
                ei.formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_)
              ),
              hint = maybeHint
            )
        }
        val items = dividerPosition.foldLeft(itemsWithNoDivider.toList) { case (ls, pos) =>
          val (before, after) = pos match {
            case DividerPosition.Value(value)   => ls.span(_.value =!= Some(value))
            case DividerPosition.Number(intPos) => ls.splitAt(intPos)
          }
          before ++ List(RadioItem(divider = Some(dividerText.value))) ++ after
        }

        val radios = Radios(
          idPrefix = Some(formComponent.id.value),
          fieldset = fieldset,
          hint = hintText(formComponent),
          errorMessage = errorMessage,
          name = formComponent.id.value,
          items = items,
          classes = if (orientation === Horizontal) "govuk-radios--inline" else ""
        )

        new components.GovukRadios(govukFieldset, govukHint, govukLabel, govukFormGroup, govukHintAndErrorMessage)(
          radios
        )

      case Checkbox =>
        val itemsWithNoDivider = optionsWithHintAndHelpText.zipWithIndex.map {
          case ((option, maybeHint, maybeHelpText), index) =>
            val item = CheckboxItem(
              id = Some(formComponent.id.value + index),
              value = option.getValue(index, ei.formModelOptics.formModelVisibilityOptics),
              content = content.Text(option.label.value()),
              checked = isChecked(option.getValue(index, ei.formModelOptics.formModelVisibilityOptics)),
              conditionalHtml = helpTextHtml(maybeHelpText),
              attributes = dataLabelAttribute(
                option.label,
                ei.formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_)
              ),
              hint = maybeHint
            )
            if (
              maybeNoneChoice.exists(noneChoice =>
                noneChoice.selection === option.getValue(index, ei.formModelOptics.formModelVisibilityOptics)
              )
            ) {
              item.copy(behaviour = Some(ExclusiveCheckbox))
            } else {
              item
            }
        }
        val items = dividerPosition.foldLeft(itemsWithNoDivider.toList) { (ls, pos) =>
          val (before, after): (List[CheckboxItem], List[CheckboxItem]) = pos match {
            case DividerPosition.Value(value)   => ls.span(_.value =!= value)
            case DividerPosition.Number(intPos) => ls.splitAt(intPos)
          }
          before ++ List(CheckboxItem(divider = Some(dividerText.value))) ++ after
        }

        val checkboxes: Checkboxes = Checkboxes(
          idPrefix = Some(formComponent.id.value),
          fieldset = fieldset,
          hint = hintText(formComponent),
          errorMessage = errorMessage,
          name = formComponent.id.value,
          items = items,
          classes = if (orientation === Horizontal && optionalHelpText.isEmpty) "gform-checkbox--inline" else ""
        )

        new components.GovukCheckboxes(govukFieldset, govukHint, govukLabel, govukFormGroup, govukHintAndErrorMessage)(
          checkboxes
        )
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

  private def hintText(fc: FormComponent)(implicit messages: Messages, sse: SmartStringEvaluator) = {
    val maybeHelpText: Option[String] =
      fc.helpText.fold(fc.message.map(m => Messages(s"$m.default.helpText")))(h => Some(h.value()))
    maybeHelpText.map(h => Hint(content = content.HtmlContent(markDownParser(h))))
  }

  private def hintTextForAutoSuggest(fc: FormComponent)(implicit messages: Messages, sse: SmartStringEvaluator) = {
    val maybeHelpText: Option[String] =
      fc.helpText.fold(fc.message.map(m => Messages(s"$m.default.helpText")))(h => Some(h.value()))
    maybeHelpText.map(h =>
      Hint(id = Some(s"${fc.id.value}__assistiveHint"), content = content.HtmlContent(markDownParser(h)))
    )
  }
  private def htmlForRevealingChoice(
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    multiValue: Boolean,
    options: List[RevealingChoiceElement],
    validationResult: ValidationResult,
    extraInfo: ExtraInfo,
    obligations: Obligations,
    upscanInitiate: UpscanInitiate,
    upscanData: Map[FormComponentId, UpscanData],
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
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

    val visibleOptions =
      options.filter(o => isVisibleOption(o.choice, extraInfo.formModelOptics) && optionHasContent(o.choice))
    if (visibleOptions.length === 0)
      throw new IllegalArgumentException(
        s"All options of the revealing choice component are invisible for${formComponent.id}"
      )

    val revealingChoicesList
      : List[(OptionData, Option[Hint], String => Boolean, FormComponentId => Int => Option[NonEmptyList[Html]])] =
      visibleOptions.map { o =>
        val isSelected: String => Boolean =
          index =>
            extraInfo.formModelOptics.pageOpticsData
              .get(formComponent.modelComponentId)
              .fold(o.selected)(_.contains(index))

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
                  obligations = obligations,
                  upscanInitiate,
                  upscanData
                )
              )

        val maybeRevealingFieldsHtml: FormComponentId => Int => Option[NonEmptyList[Html]] = controlledBy =>
          index =>
            revealingFieldsHtml(controlledBy)(index) match {
              case x :: xs => Some(NonEmptyList(x, xs))
              case Nil     => None
            }

        (o.choice, toHint(o.hint.orElse(o.choice.hint)), isSelected, maybeRevealingFieldsHtml)
      }

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage = errors.map(error =>
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

    val isPageHeading = extraInfo.formLevelHeading
    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = content.Text(formComponent.label.value()),
            isPageHeading = isPageHeading,
            classes = getLegendClasses(isPageHeading, formComponent.labelSize)
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
            value = option.getValue(index, extraInfo.formModelOptics.formModelVisibilityOptics),
            content = content.Text(option.label.value()),
            checked = isChecked(option.getValue(index, extraInfo.formModelOptics.formModelVisibilityOptics)),
            conditionalHtml = revealingFieldsHtml(maybeRevealingFieldsHtml(formComponent.id)(index)),
            attributes = dataLabelAttribute(
              option.label,
              formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_)
            ),
            hint = maybeHint
          )
      }

      val checkboxes = Checkboxes(
        idPrefix = Some(formComponent.id.value),
        fieldset = fieldset,
        hint = hintText(formComponent),
        errorMessage = errorMessage,
        name = formComponent.id.value,
        items = items
      )

      new components.GovukCheckboxes(govukFieldset, govukHint, govukLabel, govukFormGroup, govukHintAndErrorMessage)(
        checkboxes
      )
    } else {

      val items = revealingChoicesList.zipWithIndex.map {
        case ((option, maybeHint, isChecked, maybeRevealingFieldsHtml), index) =>
          RadioItem(
            id = Some(formComponent.id.value + index),
            value = Some(option.getValue(index, extraInfo.formModelOptics.formModelVisibilityOptics)),
            content = content.Text(option.label.value()),
            checked = isChecked(option.getValue(index, extraInfo.formModelOptics.formModelVisibilityOptics)),
            conditionalHtml = revealingFieldsHtml(maybeRevealingFieldsHtml(formComponent.id)(index)),
            attributes = dataLabelAttribute(
              option.label,
              formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_)
            ),
            hint = maybeHint
          )
      }

      val radios = Radios(
        idPrefix = Some(formComponent.id.value),
        fieldset = fieldset,
        hint = hintText(formComponent),
        errorMessage = errorMessage,
        name = formComponent.id.value,
        items = items
      )

      new components.GovukRadios(govukFieldset, govukHint, govukLabel, govukFormGroup, govukHintAndErrorMessage)(radios)
    }
  }

  private def renderLookup(
    text: Text,
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

    val labelString = formComponent.label.value()
    val isPageHeading = ei.formLevelHeading

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage = errors.map(error =>
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

    val label = Label(
      forAttr = Some(formComponent.id.value),
      isPageHeading = isPageHeading,
      classes = getLabelClasses(isPageHeading, formComponent.labelSize),
      content = content.Text(labelString)
    )

    lookupRegistry.get(register) match {
      case None => Html("") // Ups
      case Some(AjaxLookup(options, _, showAll)) =>
        html.form.snippets.lookup_autosuggest(
          label,
          formComponent.id,
          formComponent.editable,
          showAll,
          register,
          ei.formTemplate._id,
          ei.maybeAccessCode,
          prepopValue,
          formFieldValidationResult,
          hintTextForAutoSuggest(formComponent),
          getSelectItemsForLookup(formComponent, register, ei, options, prepopValue),
          errorMessage,
          text.displayWidth
        )
      case Some(RadioLookup(options)) =>
        val isPageHeading = ei.formLevelHeading
        val fieldset = Some(
          Fieldset(
            legend = Some(
              Legend(
                content = content.Text(formComponent.label.value()),
                isPageHeading = isPageHeading,
                classes = getLegendClasses(isPageHeading, formComponent.labelSize)
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
          hint = hintText(formComponent),
          errorMessage = errorMessage,
          name = formComponent.id.value,
          items = items
        )

        new components.GovukRadios(govukFieldset, govukHint, govukLabel, govukFormGroup, govukHintAndErrorMessage)(
          radios
        )

    }
  }

  private def renderTextArea(
    text: TextArea,
    formComponent: FormComponent,
    validationResult: ValidationResult,
    ei: ExtraInfo
  )(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ) = {
    val prepopValue = ei.formModelOptics.pageOpticsData.one(formComponent.modelComponentId)
    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)

    val labelContent = content.Text(formComponent.label.value())

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage: Option[ErrorMessage] = errors.map(error =>
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

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
      case DisplayWidth.DEFAULT => "govuk-input--width-40"
      case _                    => "govuk-input--width-40"
    }

    val isPageHeading = ei.formLevelHeading

    val label = Label(
      isPageHeading = isPageHeading,
      classes = getLabelClasses(isPageHeading, formComponent.labelSize),
      content = labelContent
    )

    val govukTextarea = new components.GovukTextarea(govukLabel, govukFormGroup, govukHintAndErrorMessage)

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
          hint = hintText(formComponent),
          value = maybeCurrentValue,
          maxLength = Some(maxLength),
          errorMessage = errorMessage,
          classes = sizeClasses,
          attributes = attributes,
          threshold = text.dataThreshold
        )

        new HmrcCharacterCount(new GovukCharacterCount(govukTextarea, govukHint))(characterCount)

      case _ =>
        val textArea = Textarea(
          id = formComponent.id.value,
          name = formComponent.id.value,
          rows = text.rows,
          label = label,
          hint = hintText(formComponent),
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
  )(implicit l: LangADT, messages: Messages, sse: SmartStringEvaluator) = {
    def prepopValue: String =
      ei.formModelOptics.formModelVisibilityOptics
        .evalAndApplyTypeInfoExplicit(text.value, formComponent.id)
        .stringRepresentation

    val formFieldValidationResult = validationResult(formComponent)

    val maybeUnit = TextFormatter.appendUnit(text.constraint)
    val labelContent = content.Text(formComponent.label.value())

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage: Option[ErrorMessage] = errors.map(error =>
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

    val maybeCurrentValue: Option[String] =
      formFieldValidationResult.getCurrentValue
        .orElse(Some(prepopValue))
        .map { cv =>
          formFieldValidationResult match {
            case FieldOk(_, _) | FieldGlobalOk(_, _) =>
              TextFormatter
                .componentTextForRendering(cv, text.constraint, formComponent.presentationHint, formComponent.editable)
            case _ => cv
          }
        }

    formComponent.presentationHint match {
      case Some(xs) if xs.contains(TotalValue) =>
        val totalText =
          new TotalText(
            formComponent,
            labelContent,
            maybeUnit,
            hintText(formComponent),
            errorMessage,
            maybeCurrentValue
          )

        html.form.snippets.field_template_text_total(totalText, getLabelClasses(false, formComponent.labelSize))

      case _ if formComponent.derived => HtmlFormat.empty
      case _ =>
        val sizeClasses = TextConstraint.getSizeClass(text.constraint, text.displayWidth)
        val inputClasses = formComponent.extraLetterSpacing match {
          case Some(true)                                        => s"$sizeClasses govuk-input--extra-letter-spacing"
          case None if text.constraint.defaultExtraLetterSpacing => s"$sizeClasses govuk-input--extra-letter-spacing"
          case _                                                 => sizeClasses
        }

        val isPageHeading = ei.formLevelHeading
        val label = Label(
          isPageHeading = isPageHeading,
          classes = getLabelClasses(isPageHeading, formComponent.labelSize),
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
          val currencyInput = Input(
            id = formComponent.id.value,
            name = formComponent.id.value,
            label = label,
            hint = hintText(formComponent),
            value = maybeCurrentValue,
            errorMessage = errorMessage,
            classes = sizeClasses,
            prefix = Some(
              PrefixOrSuffix(
                content = content.Text("£")
              )
            ),
            attributes = ei.specialAttributes ++ attributes
          )

          new components.GovukInput(govukLabel, govukFormGroup, govukHintAndErrorMessage)(currencyInput)

        } else {
          val inputType = formComponent match {
            case IsTelephone() => "tel"
            case IsEmail()     => "email"
            case _             => "text"
          }

          val autocomplete = formComponent match {
            case IsTelephone() => Some("tel")
            case IsEmail()     => Some("email")
            case _             => Option.empty[String]
          }
          val spellcheck = formComponent match {
            case IsEmail() => Some(false)
            case _         => None
          }
          val input = Input(
            id = formComponent.id.value,
            inputType = inputType,
            name = formComponent.id.value,
            label = label,
            hint = hintText(formComponent),
            value = maybeCurrentValue,
            errorMessage = errorMessage,
            classes = inputClasses,
            spellcheck = spellcheck,
            attributes = ei.specialAttributes ++ attributes,
            autocomplete = autocomplete,
            prefix = text.prefix.map(s => PrefixOrSuffix(content = content.Text(s.value()))),
            suffix = maybeSuffix.map(s => PrefixOrSuffix(content = content.Text(s.value())))
          )

          new components.GovukInput(govukLabel, govukFormGroup, govukHintAndErrorMessage)(input)
        }
    }
  }

  private def htmlForAddress(
    formComponent: FormComponent,
    address: Address,
    validationResult: ValidationResult,
    ei: ExtraInfo,
    countyDisplayed: Boolean
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ) = {
    val formFieldValidationResult = validationResult(formComponent)
    val isPageHeading = ei.formLevelHeading
    html.form.snippets
      .field_template_address(
        address,
        formComponent,
        formFieldValidationResult,
        isPageHeading,
        getLegendClasses(isPageHeading, formComponent.labelSize),
        countyDisplayed
      )
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
    val isPageHeading = ei.formLevelHeading

    def fetchValue(key: HtmlFieldId, atom: Atom): String =
      formFieldValidationResult.getOptionalCurrentValue(key).getOrElse("")

    val lookupOptions = lookupRegistry.get(Register.Country).fold(LocalisedLookupOptions(Map.empty)) {
      case AjaxLookup(options, autocomplete, showAll) => options
      case RadioLookup(options)                       => options
    }

    val showAll = lookupRegistry
      .get(Register.Country)
      .map {
        case AjaxLookup(_, _, showAll) => showAll
        case _                         => ShowAll.Disabled
      }
      .getOrElse(ShowAll.Disabled)

    val countryHtmlFieldId: HtmlFieldId = HtmlFieldId.Pure(formComponent.atomicFormComponentId(OverseasAddress.country))
    val formFieldValidationResultCountry = formFieldValidationResult.forHtmlFieldId(countryHtmlFieldId)

    html.form.snippets
      .field_template_overseas_address(
        ei.formTemplate._id,
        ei.maybeAccessCode,
        overseasAddress,
        formComponent,
        getSelectItemsForLookup(
          formComponent,
          Register.Country,
          ei,
          lookupOptions,
          Some(fetchValue(countryHtmlFieldId, OverseasAddress.country)).filter(_.nonEmpty)
        ),
        formFieldValidationResult,
        formFieldValidationResultCountry,
        isPageHeading,
        getLegendClasses(isPageHeading, formComponent.labelSize),
        fetchValue,
        showAll
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
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

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
          val inputClasses = getInputClasses(formFieldValidationResult, atom)

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
          content = content.Text(formComponent.label.value()),
          classes = getLegendClasses(isPageHeading, formComponent.labelSize),
          isPageHeading = isPageHeading
        )
      )
    )

    val dateInput = DateInput(
      id = formComponent.id.value,
      items = items.toList,
      hint = hintText(formComponent),
      errorMessage = errorMessage,
      fieldset = Some(fieldset)
    )

    new components.GovukDateInput(govukFieldset, govukInput, govukFormGroup, govukHintAndErrorMessage)(dateInput)
  }

  private def htmlForPostcodeLookup(
    formComponent: FormComponent,
    validationResult: ValidationResult,
    ei: ExtraInfo
  )(implicit
    messages: Messages
  ) = {

    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage: Option[ErrorMessage] = errors.map(error =>
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

    val filterHint: Hint = Hint(
      content = content.Text(messages("postcodeLookup.Filter.hint"))
    )

    val attributes =
      if (formComponent.editable)
        Map.empty[String, String]
      else
        Map("readonly" -> "")

    val isPageHeading = false

    val hasErrors = formFieldValidationResult.isNotOk
    val inputClasses = if (hasErrors) "govuk-input--error" else ""

    val items: NonEmptyList[Input] =
      PostcodeLookup
        .fields(formComponent.modelComponentId.indexedComponentId)
        .map { modelComponentId =>
          val prepop = ei.formModelOptics.pageOpticsData.one(modelComponentId)
          val atom = modelComponentId.atom

          val labelContent = content.Text(messages("postcodeLookup." + atom.value.capitalize))

          val label = Label(
            isPageHeading = isPageHeading,
            classes = s"${getLabelClasses(isPageHeading, formComponent.labelSize)} govuk-!-font-weight-bold",
            content = labelContent
          )
          Input(
            id = modelComponentId.toMongoIdentifier,
            inputType = "text",
            name = modelComponentId.toMongoIdentifier,
            label = label,
            hint = if (modelComponentId.atom === PostcodeLookup.filter) Some(filterHint) else None,
            value = formFieldValidationResult
              .getOptionalCurrentValue(HtmlFieldId.pure(modelComponentId))
              .orElse(prepop),
            classes =
              if (modelComponentId.atom === PostcodeLookup.postcode) s"$inputClasses govuk-input--width-10"
              else "govuk-input--width-20",
            attributes = attributes,
            autocomplete =
              if (modelComponentId.atom === PostcodeLookup.filter) Some("address-line1") else Some("postal-code"),
            errorMessage = if (modelComponentId.atom === PostcodeLookup.postcode) errorMessage else None
          )
        }

    val maker = new components.GovukInput(govukLabel, govukFormGroup, govukHintAndErrorMessage)

    val enterAddressHref = uk.gov.hmrc.gform.addresslookup.routes.AddressLookupController
      .enterAddress(
        ei.formTemplate._id,
        ei.maybeAccessCode,
        formComponent.id,
        ei.sectionNumber,
        SuppressErrors.Yes,
        List(FastForward.Yes)
      )

    (items.map(maker(_)) :+ html.form.snippets.manual_address(enterAddressHref)).combineAll
  }

  private def getInputClasses(formFieldValidationResult: FormFieldValidationResult, atom: Atom): String = {
    val errorsByAtom = formFieldValidationResult.fieldErrorsWithSuffix(atom)
    if (errorsByAtom.nonEmpty) "govuk-input--error" else ""
  }

  private def htmlForTaxPeriodDate(
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
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

    val attributes =
      if (formComponent.editable)
        Map.empty[String, String]
      else
        Map("readonly" -> "")

    val items =
      TaxPeriodDate
        .fields(formComponent.modelComponentId.indexedComponentId)
        .map { modelComponentId =>
          val prepop = ei.formModelOptics.pageOpticsData.one(modelComponentId)
          val atom = modelComponentId.atom
          val inputWidth = if (atom === TaxPeriodDate.year) "4" else "2"
          val inputClasses = getInputClasses(formFieldValidationResult, atom)

          InputItem(
            id = modelComponentId.toMongoIdentifier,
            name = modelComponentId.toMongoIdentifier,
            value = formFieldValidationResult
              .getOptionalCurrentValue(HtmlFieldId.pure(modelComponentId))
              .orElse(prepop),
            label = Some(messages("date." + atom.value.capitalize)),
            classes = s"$inputClasses govuk-input--width-$inputWidth",
            attributes = attributes
          )
        }

    val isPageHeading = ei.formLevelHeading

    val fieldset = Fieldset(
      legend = Some(
        Legend(
          content = content.Text(formComponent.label.value()),
          classes = getLegendClasses(isPageHeading, formComponent.labelSize),
          isPageHeading = isPageHeading
        )
      )
    )

    val dateInput = DateInput(
      id = formComponent.id.value,
      items = items.toList,
      hint = hintText(formComponent),
      errorMessage = errorMessage,
      fieldset = Some(fieldset)
    )

    new components.GovukDateInput(govukFieldset, govukInput, govukFormGroup, govukHintAndErrorMessage)(dateInput)

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
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

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
          val inputClasses = getInputClasses(formFieldValidationResult, atom)

          InputItem(
            id = modelComponentId.toMongoIdentifier,
            name = modelComponentId.toMongoIdentifier,
            label = Some(messages("date." + atom.value.capitalize)),
            value = formFieldValidationResult
              .getOptionalCurrentValue(HtmlFieldId.pure(modelComponentId))
              .orElse(prepopValues.map(_.valueForAtom(atom)))
              .orElse(prepop),
            classes = s"$inputClasses ${sizeForAtom(atom)}",
            attributes = attributes,
            inputmode = Some("numeric")
          )
        }

    val isPageHeading = ei.formLevelHeading

    val fieldset = Fieldset(
      legend = Some(
        Legend(
          content = content.Text(formComponent.label.value()),
          classes = getLegendClasses(isPageHeading, formComponent.labelSize),
          isPageHeading = isPageHeading
        )
      )
    )

    val dateInput = DateInput(
      id = formComponent.id.value,
      items = items.toList,
      hint = hintText(formComponent),
      errorMessage = errorMessage,
      fieldset = Some(fieldset)
    )

    new components.GovukDateInput(govukFieldset, govukInput, govukFormGroup, govukHintAndErrorMessage)(dateInput)
  }

  private def renderTime(
    time: Time,
    formComponent: FormComponent,
    validationResult: ValidationResult,
    ei: ExtraInfo
  )(implicit sse: SmartStringEvaluator, m: Messages) = {
    val prepopValue = ei.formModelOptics.pageOpticsData.one(formComponent.modelComponentId)
    val formFieldValidationResult = validationResult(formComponent)

    val labelContent = content.Text(formComponent.label.value())

    val errors: Option[String] = ValidationUtil.renderErrors(formFieldValidationResult).headOption

    val errorMessage: Option[ErrorMessage] = errors.map(error =>
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

    val isPageHeading = ei.formLevelHeading
    val label = Label(
      isPageHeading = isPageHeading,
      classes = getLabelClasses(isPageHeading, formComponent.labelSize),
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
      hint = hintText(formComponent),
      errorMessage = errorMessage,
      attributes = attributes
    )

    new components.GovukSelect(govukLabel, govukFormGroup, govukHintAndErrorMessage)(select)
  }

  private def htmlForFileUploadOnly(
    formComponent: FormComponent,
    fileUploadName: String,
    ei: ExtraInfo,
    snippets: List[Html],
    additionalAttributes: Map[String, String]
  )(implicit
    request: RequestHeader,
    sse: SmartStringEvaluator,
    m: Messages
  ): Html = {

    val errors: Option[String] = request.flash.get(GformFlashKeys.FileUploadError)

    val errorMessage = errors.map(error =>
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = content.Text(error)
      )
    )

    val labelContent = content.Text(formComponent.label.value())

    val isPageHeading = ei.formLevelHeading

    val label = Label(
      isPageHeading = isPageHeading,
      classes = getLabelClasses(isPageHeading, formComponent.labelSize),
      content = labelContent
    )
    val fileUpload: fileupload.FileUpload = fileupload.FileUpload(
      id = formComponent.id.value,
      name = fileUploadName,
      label = label,
      hint = hintText(formComponent),
      errorMessage = errorMessage,
      attributes = additionalAttributes
    )

    val fileInput: Html =
      new components.GovukFileUpload(govukLabel, govukFormGroup, govukHintAndErrorMessage)(fileUpload)

    val uploadedFiles: Html = html.form.snippets.uploaded_files_wrapper(formComponent.id)(HtmlFormat.empty)

    HtmlFormat.fill(snippets ++ List(fileInput, uploadedFiles))
  }

  private def htmlForGroup(
    renderUnitGroup: RenderUnit.Group,
    formTemplateId: FormTemplateId,
    ei: ExtraInfo,
    validationResult: ValidationResult,
    obligations: Obligations,
    upscanInitiate: UpscanInitiate,
    upscanData: Map[FormComponentId, UpscanData]
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
        getGroupForRendering(
          formComponent,
          formTemplateId,
          group,
          validationResult,
          ei,
          obligations,
          upscanInitiate,
          upscanData
        )
      }

    html.form.snippets.group(
      formComponent,
      maybeHint,
      renderUnitGroup.group,
      lhtml,
      canAddAnother,
      formTemplateId,
      ei.maybeAccessCode,
      ei.sectionNumber,
      getLabelClasses(false, formComponent.labelSize)
    )
  }

  private def getGroupForRendering(
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    group: Group,
    validationResult: ValidationResult,
    ei: ExtraInfo,
    obligations: Obligations,
    upscanInitiate: UpscanInitiate,
    upscanData: Map[FormComponentId, UpscanData]
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
        htmlFor(
          RenderUnit.pure(formComponent),
          formTemplateId,
          ei,
          validationResult,
          obligations = obligations,
          upscanInitiate,
          upscanData
        )
      )

      val removeButton: Option[ModelComponentId] =
        if (
          group.repeatsMax.getOrElse(0) === group.repeatsMin.getOrElse(0) ||
          (index === 1 && isLast)
        ) None
        else Some(formComponent.modelComponentId)

      val label = group.repeatLabel.map(_.value()).getOrElse("")

      val removeButtonHtml =
        html.form.snippets.delete_group_link(formTemplateId, label, removeButton, ei.maybeAccessCode, ei.sectionNumber)

      val dividerHtml = html.form.snippets.divider()

      val fieldSet = Fieldset(
        legend = Some(
          Legend(
            content = content.Text(label),
            classes = "govuk-fieldset__legend--m"
          )
        ),
        html = HtmlFormat.fill(lhtml ++ List(removeButtonHtml, dividerHtml))
      )

      govukFieldset(fieldSet) :: Nil

    } else {
      val htmls =
        group.fields.map(formComponent =>
          htmlFor(
            RenderUnit.pure(formComponent),
            formTemplateId,
            ei,
            validationResult,
            obligations = obligations,
            upscanInitiate,
            upscanData
          )
        )
      htmls
    }

  private def emptyRetrievals = AuthenticatedRetrievals(
    governmentGatewayId = GovernmentGatewayId(""),
    enrolments = Enrolments(Set.empty),
    affinityGroup = Individual,
    groupIdentifier = "",
    maybeNino = None,
    otherRetrievals = OtherRetrievals.empty,
    ConfidenceLevel.L50,
    credentialRole = None
  )

  def shouldDisplayHeading(
    singleton: Singleton[DataExpanded]
  ): Boolean =
    singleton.page.allFields.dropWhile(_.onlyShowOnSummary).headOption.exists(_.isPageHeading)

  private def dataLabelAttribute(label: SmartString, resolver: BooleanExpr => Boolean): Map[String, String] =
    dataLabelAttribute(label.localised(resolver).value(LangADT.En))
  private def dataLabelAttribute(label: String): Map[String, String] =
    Map("aria-label" -> label.replaceAll("''", "'")) // Unescape single-quote

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

    val selectionCriteria: Option[List[SimplifiedSelectionCriteria]] = (formComponent match {
      case IsText(Text(Lookup(_, sc), _, _, _, _, _, _))         => sc
      case IsOverseasAddress(OverseasAddress(_, _, _, _, _, sc)) => sc
      case _                                                     => None
    }).map {
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

  private def getLabelClasses(isPageHeading: Boolean, labelSize: Option[LabelSize]): String =
    (isPageHeading, labelSize) match {
      case (true, _)             => "govuk-label--l"
      case (_, Some(ExtraLarge)) => "govuk-label--xl"
      case (_, Some(Large))      => "govuk-label--l"
      case (_, Some(Medium))     => "govuk-label--m"
      case (_, Some(Small))      => "govuk-label--s"
      case (_, Some(ExtraSmall)) => "govuk-label--xs"
      case _                     => ""
    }

  private def getLegendClasses(isPageHeading: Boolean, labelSize: Option[LabelSize]): String =
    (isPageHeading, labelSize) match {
      case (true, _)             => "govuk-fieldset__legend--l"
      case (_, Some(ExtraLarge)) => "govuk-fieldset__legend--xl"
      case (_, Some(Large))      => "govuk-fieldset__legend--l"
      case (_, Some(Medium))     => "govuk-fieldset__legend--m"
      case (_, Some(Small))      => "govuk-fieldset__legend--s"
      case (_, Some(ExtraSmall)) => "govuk-fieldset__legend--xs"
      case _                     => ""
    }

  private val govukErrorMessage: components.GovukErrorMessage = new components.GovukErrorMessage()
  private val govukFieldset: components.GovukFieldset = new components.GovukFieldset()
  private val govukHint: components.GovukHint = new components.GovukHint()
  private val govukLabel: components.GovukLabel = new components.GovukLabel()
  private val govukFormGroup: GovukFormGroup = new GovukFormGroup
  private val govukHintAndErrorMessage: GovukHintAndErrorMessage =
    new GovukHintAndErrorMessage(govukHint, govukErrorMessage)
  private val govukInput = new components.GovukInput(govukLabel, govukFormGroup, govukHintAndErrorMessage)

}

object SectionRenderingService {

  def atlCyaTitles[T <: PageMode](
    cache: AuthCacheWithForm,
    sectionNumber: SectionNumber,
    checkYourAnswers: CheckYourAnswers[T],
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): (String, String) = {

    val isFirstVisit = !cache.form.visitsIndex.contains(sectionNumber)
    val title =
      if (isFirstVisit)
        checkYourAnswers.expandedTitle.fold(messages("summary.checkYourAnswers"))(_.value())
      else checkYourAnswers.expandedUpdateTitle.value()

    val noPIITitle =
      if (isFirstVisit)
        checkYourAnswers.expandedNoPIITitle.fold(messages("summary.checkYourAnswers"))(_.value())
      else
        checkYourAnswers.expandedNoPIIUpdateTitle.fold(checkYourAnswers.expandedNoPIITitle match {
          case Some(value) =>
            value.valueWithoutInterpolations(formModelOptics.formModelVisibilityOptics.booleanExprResolver.resolve(_))
          case None => messages("summary.checkYourAnswers")
        })(_.value())

    (title, noPIITitle)

  }

  def summaryList[T <: PageMode](
    formTemplateId: FormTemplateId,
    checkYourAnswers: CheckYourAnswers[T],
    addToListIteration: Bracket.AddToListIteration[Visibility],
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    validationResult: ValidationResult,
    envelope: EnvelopeWithMapping,
    addressRecordLookup: AddressRecordLookup,
    sectionNumber: SectionNumber,
    fastForward: List[FastForward]
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): List[SummaryList] = {
    val hidePageTitleByCya = checkYourAnswers.presentationHint.filter(_ === InvisiblePageTitle).fold(false)(_ => true)
    val headingSummaryListRows = addToListIteration.singletons.toList
      .map { singletonWithNumber =>
        val sectionTitle4Ga = sectionTitle4GaFactory(
          formModelVisibilityOptics.formModel(singletonWithNumber.sectionNumber),
          singletonWithNumber.sectionNumber
        )
        val page = singletonWithNumber.singleton.page

        val hidePageTitle = page.presentationHint.filter(_ === InvisiblePageTitle).fold(hidePageTitleByCya)(_ => true)
        val keyDisplayWidth = checkYourAnswers.keyDisplayWidth.getOrElse(KeyDisplayWidth.S)

        val rows = page.fields
          .filterNot(_.hideOnSummary)
          .flatMap { fc =>
            FormComponentSummaryRenderer
              .summaryListRows[DataOrigin.Mongo, AddToListCYARender](
                fc,
                page.id.map(_.modelPageId),
                formTemplateId,
                formModelVisibilityOptics,
                maybeAccessCode,
                singletonWithNumber.sectionNumber,
                sectionTitle4Ga,
                cache.form.thirdPartyData.obligations,
                validationResult,
                envelope,
                addressRecordLookup,
                None,
                Some(FastForward.CYA(SectionOrSummary.Section(sectionNumber)) :: fastForward),
                keyDisplayWidth
              )
          }

        val heading = if (hidePageTitle) "" else page.shortName.getOrElse(page.title).value()
        heading -> rows
      }
      .filterNot(_._2.isEmpty)

    def summarizeGroupedRows(
      summaryTitleRows: List[(String, List[SummaryListRow])]
    ): List[(String, List[SummaryListRow])] =
      summaryTitleRows.foldLeft(
        (
          Option.empty[String],
          List.empty[SummaryListRow],
          List.empty[(String, List[SummaryListRow])]
        )
      ) { case ((currentHeading, accumulatedRows, resultBuffer), (heading, rows)) =>
        if (heading.nonEmpty || accumulatedRows.isEmpty) {
          val newResultBuffer = currentHeading.map(ct => resultBuffer :+ (ct, accumulatedRows)).getOrElse(resultBuffer)
          (Some(heading), rows, newResultBuffer)
        } else {
          (currentHeading, accumulatedRows ++ rows, resultBuffer)
        }
      } match {
        case (currentHeading, accumulatedRows, resultBuffer) =>
          currentHeading.map(ct => resultBuffer :+ (ct, accumulatedRows)).getOrElse(resultBuffer)
      }

    summarizeGroupedRows(headingSummaryListRows).map { case (heading, rows) =>
      SummaryList(
        rows = rows,
        classes = "govuk-!-margin-bottom-0",
        attributes =
          if (heading.isEmpty) Map.empty[String, String]
          else Map("title" -> heading)
      )
    }
  }

  def determineContinueLabelKey(
    continueLabelKey: String,
    isNotPermitted: Boolean,
    continueLabel: Option[SmartString],
    isFileUploadOnlyPage: Boolean
  )(implicit messages: Messages, lise: SmartStringEvaluator): String =
    (continueLabel, isNotPermitted, isFileUploadOnlyPage) match {
      case (Some(cl), _, _)     => cl.value()
      case (None, true, _)      => messages("button.continue")
      case (None, false, false) => messages(continueLabelKey)
      case (None, false, true)  => messages("file.upload")
    }
  def normaliseTableComp(table: TableComp, isVisibleValueRow: TableValueRow => Boolean): TableComp = {

    def rowSpansIndexes(row: TableValueRow): List[(Int, TableValue)] = row.values.zipWithIndex.flatMap {
      case (tableValue, index) =>
        tableValue.rowspan match {
          case Some(s) if s > 1 => List(index -> tableValue)
          case _                => List.empty[(Int, TableValue)]
        }
    }

    val indexes: List[Int] = List.range(1, table.rows.size)

    /*
     *  If the table row is hidden and it starts a rowspan, we need to copy that starting
     *  cell to next row (and decrement the copied rowspan)
     */
    val updatedRows: List[TableValueRow] = indexes.foldLeft(table.rows) { case (rows, currentSplit) =>
      val (begin, end): (List[TableValueRow], List[TableValueRow]) = rows.splitAt(currentSplit)

      val currentRow = begin.last

      val rowIsVisible = isVisibleValueRow(currentRow)

      if (rowIsVisible) {
        rows // Nothing to do
      } else {
        val endUpdated = end match {
          case nextRow :: restOfRows =>
            val indexes: List[(Int, TableValue)] = rowSpansIndexes(currentRow)
            val updatedTableValues: List[TableValue] = indexes.foldLeft(nextRow.values) {
              case (rowCells, (currentIndex, tableValue)) =>
                val (beginCells, endCells) = rowCells.splitAt(currentIndex)
                beginCells ++ List(tableValue.decrementRowSpan) ++ endCells
            }
            val nextRowUpdated = nextRow.copy(values = updatedTableValues)
            nextRowUpdated :: restOfRows
          case Nil => Nil
        }

        begin ++ endUpdated
      }
    }

    val rowsWithVisibility: List[(TableValueRow, Boolean)] = updatedRows.map(row => row -> isVisibleValueRow(row))

    val rowsWithVisibilityTails: List[List[(TableValueRow, Boolean)]] = rowsWithVisibility.tails.toList

    /*
     *  Reduce rowspan if some of overlapping rows are hidden
     */
    val finalRows: List[TableValueRow] = rowsWithVisibilityTails.flatMap {
      case (tableValueRow, isVisible) :: tails =>
        if (!isVisible) {
          List(tableValueRow) // No need to touch invisible row
        } else {
          val newValues = tableValueRow.values.map { tableValue =>
            tableValue.rowspan match {
              case None => tableValue
              case Some(rowspan) =>
                val rowsToInspect: List[(TableValueRow, Boolean)] = tails.take(rowspan - 1)
                val hiddenInRowspan: Int = rowsToInspect.map(_._2).count(_ === false)
                val reducedRowSpan: Int = rowspan - hiddenInRowspan
                val updatedRowSpan = if (reducedRowSpan > 1) Some(reducedRowSpan) else None
                tableValue.copy(rowspan = updatedRowSpan)
            }
          }
          List(tableValueRow.copy(values = newValues))
        }
      case otherwise => Nil
    }

    table.copy(rows = finalRows)
  }
}

object IsNilOrInfoOnly {
  @tailrec
  def unapply(xs: List[FormComponent]): Boolean =
    xs match {
      case Nil                                                      => true
      case IsInformationMessage(_) :: tail                          => unapply(tail)
      case head :: tail if head.onlyShowOnSummary || !head.editable => unapply(tail)
      case _                                                        => false
    }
}
