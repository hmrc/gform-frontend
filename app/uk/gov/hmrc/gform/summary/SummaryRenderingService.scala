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

package uk.gov.hmrc.gform.summary

import cats.data.NonEmptyList
import cats.syntax.eq._
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.{ Call, Request }
import play.twirl.api.{ Html, HtmlFormat, XmlFormat }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.commons.MarkDownUtil.markDownParser
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.objectStore.{ EnvelopeWithMapping, ObjectStoreAlgebra }
import uk.gov.hmrc.gform.gform.{ HtmlSanitiser, NoErrors, PageLevelErrorHtml, SectionRenderingService, SummaryPagePurpose, routes }
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.PrintSection
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.PrintSection.PdfNotification
import uk.gov.hmrc.gform.validation.{ ValidationResult, ValidationService }
import uk.gov.hmrc.gform.views.html.summary.snippets._
import uk.gov.hmrc.gform.views.html.summary.summary
import uk.gov.hmrc.gform.views.summary.SummaryListRowHelper._
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{ Card, CardTitle, SummaryList, SummaryListRow }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.govukfrontend.views.html.components.GovukSummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.gform.views.xml.summary.pdf._
import uk.gov.hmrc.gform.pdf.model.PDFModel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.KeyDisplayWidth.KeyDisplayWidth
import uk.gov.hmrc.gform.views.summary.SummaryListRowHelper
import uk.gov.hmrc.gform.views.summary.pdf.PdfHelper

import java.time.format.DateTimeFormatter
import scala.concurrent.{ ExecutionContext, Future }

class SummaryRenderingService(
  renderer: SectionRenderingService,
  i18nSupport: I18nSupport,
  objectStoreAlgebra: ObjectStoreAlgebra[Future],
  validationService: ValidationService,
  frontendAppConfig: FrontendAppConfig
) {

  def createHtmlForPrintPdf(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    summaryPagePurpose: SummaryPagePurpose,
    pdf: PrintSection.Pdf,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    maybeCoordinates: Option[Coordinates]
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[PdfContent] =
    for {
      summaryHtml <-
        getSummaryHTML(maybeAccessCode, cache, summaryPagePurpose, formModelOptics, maybeCoordinates, None, None)
    } yield {
      val (headerStr, footerStr) = addDataToPrintPdfHTML(pdf.header, pdf.footer)
      PdfContent(
        HtmlSanitiser
          .sanitiseHtmlForPDF(
            summaryHtml,
            document => {
              document.title(s"${messages("summary.acknowledgement.pdf")} - ${cache.formTemplate.formName.value}")
              HtmlSanitiser.printSectionPdf(document, headerStr, footerStr)
            }
          )
      )
    }

  def createHtmlForNotificationPdf(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    summaryPagePurpose: SummaryPagePurpose,
    pdfNotification: PdfNotification,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[PdfContent] = {
    val pdfFieldIds = pdfNotification.fieldIds
    val pdfHeader = pdfNotification.header
    val pdfFooter = pdfNotification.footer

    for {
      pdfHtml <- getNotificationPdfHTML(
                   maybeAccessCode,
                   cache,
                   summaryPagePurpose,
                   pdfFieldIds,
                   formModelOptics
                 )
    } yield {
      val (headerStr, footerStr) = addDataToPrintPdfHTML(pdfHeader, pdfFooter)
      PdfContent(
        HtmlSanitiser
          .sanitiseHtmlForPDF(
            pdfHtml,
            document => {
              document.title(s"${messages("summary.acknowledgement.pdf")} - ${cache.formTemplate.formName.value}")
              HtmlSanitiser.printSectionPdf(document, headerStr, footerStr)
            }
          )
      )
    }
  }

  private def addDataToPrintPdfHTML(
    pdfHeader: SmartString,
    pdfFooter: SmartString
  )(implicit
    lise: SmartStringEvaluator
  ): (String, String) = {

    val headerHtml = markDownParser(pdfHeader).toString
    val footerHtml = markDownParser(pdfFooter).toString

    (headerHtml, footerHtml)

  }

  def getSummaryHTML(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    summaryPagePurpose: SummaryPagePurpose,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    maybeCoordinates: Option[Coordinates],
    maybeSummarySection: Option[SummarySection],
    taskCompleted: Option[Boolean]
  )(implicit
    request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[Html] = {
    val envelopeF = objectStoreAlgebra.getEnvelope(cache.form.envelopeId).map(EnvelopeWithMapping(_, cache.form))

    import i18nSupport._

    for {
      envelope <- envelopeF
      validationResult <-
        validationService
          .validateFormModel(cache.toCacheData, envelope, formModelOptics.formModelVisibilityOptics, maybeCoordinates)
    } yield {
      val summaryDeclaration: Html =
        renderer.renderSummarySectionDeclaration(cache, formModelOptics, maybeAccessCode, maybeSummarySection)
      val summarySection = maybeSummarySection.getOrElse(cache.formTemplate.summarySection)

      SummaryRenderingService.renderSummary(
        cache.formTemplate,
        validationResult,
        formModelOptics,
        maybeAccessCode,
        envelope,
        cache.retrievals,
        frontendAppConfig,
        cache.form.thirdPartyData.obligations,
        summaryPagePurpose,
        summaryDeclaration,
        cache.form.formData.fingerprint,
        AddressRecordLookup.from(cache.form.thirdPartyData),
        maybeCoordinates,
        summarySection,
        taskCompleted
      )
    }

  }

  def getNotificationPdfHTML(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    summaryPagePurpose: SummaryPagePurpose,
    pdfFieldIds: List[FormComponentId],
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[Html] = {

    val envelopeF = objectStoreAlgebra.getEnvelope(cache.form.envelopeId).map(EnvelopeWithMapping(_, cache.form))

    import i18nSupport._

    for {
      envelope <- envelopeF
      validationResult <-
        validationService
          .validateFormModel(cache.toCacheData, envelope, formModelOptics.formModelVisibilityOptics, None)
    } yield SummaryRenderingService.renderNotificationPdfSummary(
      cache.formTemplate,
      validationResult,
      formModelOptics.formModelVisibilityOptics,
      maybeAccessCode,
      envelope,
      cache.retrievals,
      frontendAppConfig,
      cache.form.thirdPartyData.obligations,
      summaryPagePurpose,
      pdfFieldIds,
      cache.form.formData.fingerprint,
      AddressRecordLookup.from(cache.form.thirdPartyData)
    )
  }
}

object SummaryRenderingService {

  private val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
  private val dateFormat = DateTimeFormatter.ofPattern("d MMM yyyy")

  def renderSummary[D <: DataOrigin](
    formTemplate: FormTemplate,
    validationResult: ValidationResult,
    formModelOptics: FormModelOptics[D],
    maybeAccessCode: Option[AccessCode],
    envelope: EnvelopeWithMapping,
    retrievals: MaterialisedRetrievals,
    frontendAppConfig: FrontendAppConfig,
    obligations: Obligations,
    summaryPagePurpose: SummaryPagePurpose,
    summaryDeclaration: Html,
    formDataFingerprint: String,
    addressRecordLookup: AddressRecordLookup,
    maybeCoordinates: Option[Coordinates],
    summarySection: SummarySection,
    taskCompleted: Option[Boolean]
  )(implicit request: Request[_], messages: Messages, l: LangADT, lise: SmartStringEvaluator): Html = {
    val headerHtml = markDownParser(summarySection.header)
    val footerHtml = markDownParser(summarySection.footer)
    val caption = summarySection.caption.map(_.value())
    val title = summarySection.title.value()

    val envelopeUpd = envelope.byPurpose(summaryPagePurpose)

    val renderComeBackLater =
      retrievals.renderSaveAndComeBackLater && !formTemplate.draftRetrievalMethod.isNotPermitted

    val lastSectionNumber = maybeCoordinates
      .map { c =>
        formModelOptics.formModelVisibilityOptics.formModel.availableSectionNumbers.filter(_.toCoordinatesUnsafe === c)
      }
      .getOrElse(formModelOptics.formModelVisibilityOptics.formModel.availableSectionNumbers)
      .reverse
      .head

    val pageLevelErrorHtml =
      PageLevelErrorHtml.generateSummaryLevelErrorHtml(validationResult.formFieldValidationResults, List.empty)

    val sfr =
      summaryRowsForRender(
        validationResult,
        formModelOptics,
        maybeAccessCode,
        formTemplate,
        envelopeUpd,
        obligations,
        addressRecordLookup,
        maybeCoordinates,
        summarySection.keyDisplayWidth
      )
    summary(
      ExtraInfoSummary(
        formTemplate,
        sfr,
        maybeAccessCode,
        lastSectionNumber,
        renderComeBackLater,
        determineContinueLabelKey(
          retrievals.continueLabelKey,
          formTemplate.draftRetrievalMethod.isNotPermitted,
          summarySection.continueLabel
        ),
        frontendAppConfig,
        summaryPagePurpose,
        title,
        caption,
        headerHtml,
        summaryDeclaration,
        footerHtml,
        formDataFingerprint,
        summarySection.displayWidth,
        pageLevelErrorHtml,
        maybeCoordinates,
        taskCompleted
      )
    )
  }

  def renderNotificationPdfSummary(
    formTemplate: FormTemplate,
    validationResult: ValidationResult,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    maybeAccessCode: Option[AccessCode],
    envelope: EnvelopeWithMapping,
    retrievals: MaterialisedRetrievals,
    frontendAppConfig: FrontendAppConfig,
    obligations: Obligations,
    summaryPagePurpose: SummaryPagePurpose,
    pdfFieldIds: List[FormComponentId],
    formDataFingerprint: String,
    addressRecordLookup: AddressRecordLookup
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator
  ): Html = {
    val headerHtml = markDownParser(formTemplate.summarySection.header)
    val footerHtml = markDownParser(formTemplate.summarySection.footer)
    val title = formTemplate.summarySection.title.value()
    val caption = formTemplate.summarySection.caption.map(_.value())
    val renderComeBackLater =
      retrievals.renderSaveAndComeBackLater && !formTemplate.draftRetrievalMethod.isNotPermitted
    val sfr: List[Html] =
      summaryForNotificationPdf(
        validationResult,
        formModelVisibilityOptics,
        maybeAccessCode,
        formTemplate,
        envelope,
        obligations,
        pdfFieldIds,
        addressRecordLookup
      )
    summary(
      ExtraInfoSummary(
        formTemplate,
        sfr,
        maybeAccessCode,
        formTemplate.sectionNumberZero,
        renderComeBackLater,
        determineContinueLabelKey(retrievals.continueLabelKey, formTemplate.draftRetrievalMethod.isNotPermitted, None),
        frontendAppConfig,
        summaryPagePurpose,
        title,
        caption,
        headerHtml,
        HtmlFormat.empty,
        footerHtml,
        formDataFingerprint,
        formTemplate.summarySection.displayWidth,
        NoErrors,
        None,
        None
      )
    )
  }

  def summaryRowsForRender[D <: DataOrigin](
    validationResult: ValidationResult,
    formModelOptics: FormModelOptics[D],
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate,
    envelope: EnvelopeWithMapping,
    obligations: Obligations,
    addressRecordLookup: AddressRecordLookup,
    maybeCoordinates: Option[Coordinates],
    keyDisplayWidth: KeyDisplayWidth
  )(implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator
  ): List[Html] = {

    val formModel = formModelOptics.formModelVisibilityOptics.formModel

    def middleSummaryListRows(
      singleton: Singleton[Visibility],
      sectionNumber: SectionNumber,
      iterationTitle: Option[String] = None
    )(implicit
      l: LangADT
    ): List[SummaryListRow] = {
      val page = singleton.page
      val sectionTitle4Ga = sectionTitle4GaFactory(singleton, sectionNumber)

      val ff = if (maybeCoordinates.isEmpty) {
        List(FastForward.CYA(SectionOrSummary.FormSummary))
      } else {
        List(FastForward.CYA(SectionOrSummary.TaskSummary))
      }

      val fieldsIncConf: List[FormComponent] = page.fields ++ page.confirmation.map(_.question)

      fieldsIncConf
        .filterNot(_.hideOnSummary)
        .flatMap(formComponent =>
          FormComponentSummaryRenderer.summaryListRows[D, SummaryRender](
            formComponent,
            page.id.map(_.modelPageId),
            formTemplate._id,
            formModelOptics.formModelVisibilityOptics,
            maybeAccessCode,
            sectionNumber,
            sectionTitle4Ga,
            obligations,
            validationResult,
            envelope,
            addressRecordLookup,
            iterationTitle,
            Some(ff),
            keyDisplayWidth
          )
        )
    }

    def summaryList(
      heading: HtmlFormat.Appendable,
      rows: List[SummaryListRow],
      card: Option[Card]
    ): List[HtmlFormat.Appendable] =
      if (rows.isEmpty) {
        Nil
      } else {
        val govukSummaryList = new GovukSummaryList()(
          SummaryList(
            rows = rows,
            card = card,
            classes = "govuk-!-margin-bottom-0"
          )
        )
        List(heading, govukSummaryList)
      }

    def addToListSummary(bracket: Bracket.AddToList[Visibility]): Html =
      begin_section(bracket.source.summaryName)

    def summarizeGroupedRows(
      summaryTitleRows: List[(HtmlFormat.Appendable, List[SummaryListRow])]
    ): List[(HtmlFormat.Appendable, List[SummaryListRow])] =
      summaryTitleRows.foldLeft(
        (
          Option.empty[HtmlFormat.Appendable],
          List.empty[SummaryListRow],
          List.empty[(HtmlFormat.Appendable, List[SummaryListRow])]
        )
      ) { case ((currentHeading, accumulatedRows, resultBuffer), (heading, rows)) =>
        if (heading != HtmlFormat.empty || accumulatedRows.isEmpty) {
          val newResultBuffer =
            currentHeading.map(ct => resultBuffer :+ (ct, accumulatedRows)).getOrElse(resultBuffer)
          (Some(heading), rows, newResultBuffer)
        } else {
          (currentHeading, accumulatedRows ++ rows, resultBuffer)
        }
      } match {
        case (currentHeading, accumulatedRows, resultBuffer) =>
          currentHeading.map(ct => resultBuffer :+ (ct, accumulatedRows)).getOrElse(resultBuffer)
      }

    def addToListRenderBracket(bracket: Bracket.AddToList[Visibility]): List[Html] = {
      val repeaters: NonEmptyList[RepeaterWithNumber[Visibility]] = bracket.iterations.map(_.repeater)

      val hidePageTitleByComponent = bracket.source.fold(_ => false)(_ => false)(addToList =>
        addToList.presentationHint.fold(false)(_ == InvisiblePageTitle)
      )

      val htmls: List[Html] = bracket.iterations.toList.flatMap { iteration =>
        val singletons = iteration.singletons.toList

        val hideAllPageTitle =
          singletons.forall(_.singleton.page.presentationHint.filter(_ === InvisiblePageTitle).fold(false)(_ => true))

        if (hidePageTitleByComponent || hideAllPageTitle) {
          val middleRows = singletons.flatMap { singletonWithNumber =>
            middleSummaryListRows(
              singletonWithNumber.singleton,
              singletonWithNumber.sectionNumber,
              Some(iteration.repeater.repeater.expandedShortName.value())
            )
          }

          summaryList(
            HtmlFormat.empty,
            middleRows,
            Some(
              Card(
                title = Some(
                  CardTitle(
                    content = content.Text(iteration.repeater.repeater.expandedShortName.value()),
                    classes = "govuk-!-font-size-24"
                  )
                )
              )
            )
          )
        } else {
          val headingSummaryListRows = singletons.map { singletonWithNumber =>
            val heading = getPageTitle(bracket.source, singletonWithNumber.singleton.page)
            val middleRows = middleSummaryListRows(
              singletonWithNumber.singleton,
              singletonWithNumber.sectionNumber,
              Some(iteration.repeater.repeater.expandedShortName.value())
            )
            heading -> middleRows
          }

          begin_section(iteration.repeater.repeater.expandedShortName) ::
            summarizeGroupedRows(headingSummaryListRows).flatMap { case (heading, middleRows) =>
              summaryList(heading, middleRows, None)
            }
        }
      }

      val addToListItemSummaries: NonEmptyList[SmartString] = repeaters.map(_.repeater.expandedSummaryDescription)

      val lastRepeaterWithNumber = repeaters.last

      val repeater = lastRepeaterWithNumber.repeater
      val sectionNumber = lastRepeaterWithNumber.sectionNumber

      val sectionTitle4Ga: SectionTitle4Ga = sectionTitle4GaFactory(repeater, sectionNumber)

      val ff = if (maybeCoordinates.isEmpty) {
        List(FastForward.CYA(SectionOrSummary.FormSummary))
      } else {
        List(FastForward.CYA(SectionOrSummary.TaskSummary))
      }
      val url: Call = routes.FormController
        .form(
          formTemplate._id,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          SuppressErrors.Yes,
          ff
        )

      val addToListSummaryItems: List[Html] = addToListItemSummaries.map(ss => markDownParser(ss)).toList

      val addToListSummary = ordered_list(addToListSummaryItems)

      val label = repeater.title.value()

      val slr: SummaryListRow = summaryListRow(
        label, // This is weird to use, as it can have $n, but this list in shown only once. Should we have other property here?
        addToListSummary,
        Some(label),
        SummaryListRowHelper.getKeyDisplayWidthClass(keyDisplayWidth),
        "",
        "",
        (
          url,
          messages("addToList.change"),
          messages("addToList.change") + " " + repeater.title
            .value()
        ) :: Nil,
        ""
      )

      val fcrd = implicitly[FormComponentRenderDetails[SummaryRender]]
      val slrTables: List[SummaryListRow] = bracket.iterations.last.repeater.repeater.fields
        .map(
          _.filterNot(_.hideOnSummary)
            .collect { case fc @ IsTableComp(table) =>
              val label = fcrd.label(fc)
              summaryListRow(
                label,
                markDownParser(table.summaryValue),
                Some(label),
                SummaryListRowHelper.getKeyDisplayWidthClass(keyDisplayWidth),
                "",
                "",
                (
                  url,
                  messages("addToList.change"),
                  messages("addToList.change") + " " + repeater.title
                    .value()
                ) :: Nil,
                ""
              )
            }
        )
        .getOrElse(List.empty[SummaryListRow])
      new GovukSummaryList()(SummaryList(rows = slr :: slrTables, classes = "govuk-!-margin-bottom-0")) :: htmls
    }

    def brackets: List[Bracket[Visibility]] = formModel.brackets.fold(_.brackets.toList)(taskListBrackets =>
      maybeCoordinates.fold(taskListBrackets.allBrackets.toList)(coordinates =>
        taskListBrackets.bracketsFor(coordinates).toBracketsList
      )
    )

    def getPageTitle(source: Section, page: Page[Visibility]) = {
      val pageTitle = page.shortName.getOrElse(page.title)
      source.fold { _ =>
        page.presentationHint
          .filter(_ == InvisiblePageTitle)
          .fold(begin_section(pageTitle))(_ => HtmlFormat.empty)
      } { _ =>
        page.presentationHint
          .filter(_ == InvisiblePageTitle)
          .fold(begin_section(pageTitle))(_ => HtmlFormat.empty)
      } { addToList =>
        val hidePageTitle =
          addToList.presentationHint
            .fold(page.presentationHint.fold(false)(_ == InvisiblePageTitle))(_ == InvisiblePageTitle)
        if (hidePageTitle)
          HtmlFormat.empty
        else
          begin_addToList_section(pageTitle)
      }
    }

    val (accumulatedRows, summaryLists) =
      brackets.foldLeft((Map.empty[HtmlFormat.Appendable, List[SummaryListRow]], List.empty[HtmlFormat.Appendable])) {
        case ((accumulatedRows, accList), bracket @ Bracket.AddToList(_, _, _)) =>
          accumulatedRows.headOption match {
            case Some((heading, rows)) =>
              val updatedList =
                accList ++ summaryList(heading, rows, None) ++ List(
                  addToListSummary(bracket)
                ) ++ addToListRenderBracket(
                  bracket
                )
              (Map.empty, updatedList)
            case None =>
              (accumulatedRows, accList ++ List(addToListSummary(bracket)) ++ addToListRenderBracket(bracket))
          }

        case ((accumulatedRows, accList), Bracket.RepeatingPage(singletons, source)) =>
          singletons.toList.foldLeft((accumulatedRows, accList)) { case ((accRows, acc), singletonWithNumber) =>
            val middleRows = middleSummaryListRows(singletonWithNumber.singleton, singletonWithNumber.sectionNumber)
            val pageTitle = getPageTitle(source, singletonWithNumber.singleton.page)

            accRows.headOption match {
              case Some((heading, rows)) if pageTitle.body === heading.body || pageTitle.body.isEmpty =>
                (Map(heading -> (rows ++ middleRows)), acc)
              case Some((heading, rows)) =>
                (Map(pageTitle -> middleRows), acc ++ summaryList(heading, rows, None))
              case None =>
                (Map(pageTitle -> middleRows), acc)
            }
          }

        case (
              (accumulatedRows, accList),
              Bracket.NonRepeatingPage(SingletonWithNumber(singleton, sectionNumber), source)
            ) =>
          val middleRows = middleSummaryListRows(singleton, sectionNumber)
          val pageTitle = getPageTitle(source, singleton.page)

          accumulatedRows.headOption match {
            case Some((heading, rows)) if pageTitle.body === heading.body || pageTitle.body.isEmpty =>
              (Map(heading -> (rows ++ middleRows)), accList)
            case Some((heading, rows)) =>
              (Map(pageTitle -> middleRows), accList ++ summaryList(heading, rows, None))
            case None =>
              (Map(pageTitle -> middleRows), accList)
          }
      }

    accumulatedRows.headOption match {
      case Some((heading, rows)) => summaryLists ++ summaryList(heading, rows, None)
      case None                  => summaryLists
    }
  }

  def summaryForNotificationPdf(
    validationResult: ValidationResult,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate,
    envelope: EnvelopeWithMapping,
    obligations: Obligations,
    pdfFieldIds: List[FormComponentId],
    addressRecordLookup: AddressRecordLookup
  )(implicit messages: Messages, l: LangADT, lise: SmartStringEvaluator): List[Html] = {

    def renderHtmls(fields: List[FormComponent])(implicit l: LangADT): List[Html] = {
      val rows = fields
        .flatMap(formComponent =>
          FormComponentSummaryRenderer.summaryListRows[DataOrigin.Mongo, SummaryRender](
            formComponent,
            None,
            formTemplate._id,
            formModelVisibilityOptics,
            maybeAccessCode,
            formTemplate.sectionNumberZero,
            SectionTitle4Ga(""),
            obligations,
            validationResult,
            envelope,
            addressRecordLookup,
            None,
            None,
            KeyDisplayWidth.S
          )
        )

      List(new GovukSummaryList()(SummaryList(rows)))
    }

    val baseComponentIds: List[BaseComponentId] = pdfFieldIds.map(fcId => fcId.baseComponentId)
    val baseComponentIdsZipped: List[(BaseComponentId, Int)] = baseComponentIds.zipWithIndex
    val baseComponentIdsSet: Set[BaseComponentId] = baseComponentIds.toSet

    val filteredFormComponents: List[FormComponent] = formModelVisibilityOptics.allFormComponents.filter { fc =>
      baseComponentIdsSet(fc.baseComponentId)
    }

    val sortedFcs =
      filteredFormComponents.sortBy(fc => baseComponentIdsZipped.find(_._1 === fc.baseComponentId).fold(0)(_._2))

    renderHtmls(sortedFcs)
  }

  def submissionDetailsAsHTML(maybeSubmissionDetails: Option[SubmissionDetails])(implicit
    messages: Messages
  ): Html =
    maybeSubmissionDetails
      .map { sd =>
        val formattedTime =
          s"${sd.submission.submittedDate.format(dateFormat)} ${sd.submission.submittedDate.format(timeFormat)}"
        val rows = List(
          cya_row(messages("submission.date"), formattedTime),
          cya_row(messages("submission.reference"), sd.submission.submissionRef.toString),
          cya_row(messages("submission.mark"), sd.hashedValue)
        )

        cya_section(messages("submission.details"), HtmlFormat.fill(rows))
      }
      .getOrElse(Html(""))

  def submissionDetailsAsTabularHTML(maybeSubmissionDetails: Option[SubmissionDetails])(implicit
    messages: Messages
  ): Html =
    maybeSubmissionDetails
      .map { sd =>
        val formattedTime =
          s"${sd.submission.submittedDate.format(dateFormat)} ${sd.submission.submittedDate.format(timeFormat)}"
        val rows = Map(
          (messages("submission.date"), formattedTime),
          (messages("submission.reference"), sd.submission.submissionRef.toString),
          (messages("submission.mark"), sd.hashedValue)
        )

        cya_tabular_section(messages("submission.details"), rows)
      }
      .getOrElse(Html(""))

  private def determineContinueLabelKey(
    continueLabelKey: String,
    isNotPermitted: Boolean,
    continueLabel: Option[SmartString]
  )(implicit messages: Messages, lise: SmartStringEvaluator): String =
    (continueLabel, isNotPermitted) match {
      case (Some(cl), _) => cl.value()
      case (None, true)  => messages("button.continue")
      case (None, false) => messages(continueLabelKey)
    }

  def renderSubmissionDetails(maybeSubmissionDetails: Option[SubmissionDetails])(implicit
    messages: Messages
  ) =
    maybeSubmissionDetails
      .map { sd =>
        val formattedTime =
          s"${sd.submission.submittedDate.format(dateFormat)} ${sd.submission.submittedDate.format(timeFormat)}"
        val rows = Map(
          (messages("submission.date"), formattedTime),
          (messages("submission.reference"), sd.submission.submissionRef.toString),
          (messages("submission.mark"), sd.hashedValue)
        )

        submisssionDetails(messages("submission.details"), rows)
      }
      .getOrElse(XmlFormat.empty)

  def renderSummaryData(
    summaryDataList: List[SummaryData]
  )(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) =
    summaryDataList.map {
      case pageData: PageData => summaryPage(pageData)
      case AddToListData(title, summary, pageGroups, id) =>
        val summaryValues = summary.values.zipWithIndex.map { case (v, idx) => (idx + 1) -> v }
        val addToListSummaryPage = addToListSummary(summaryValues)
        addToList(id, title, Some(summary.title), addToListSummaryPage, pageGroups)
    }

  def renderPageField(field: PageField): XmlFormat.Appendable = field match {
    case SimpleField(label, values) =>
      listItem(label, simpleField(values))
    case ChoiceField(label, values) =>
      listItem(label, choiceField(values))
    case rc: RevealingChoiceField =>
      val renderedElements = if (rc.isSeparate) {
        listItem(rc.label, revealingChoiceField(rc.choiceElements.map(ce => Html(ce.label)))) ::
          rc.choiceElements.flatMap { choiceElement =>
            choiceElement.fields.map(renderPageField)
          }
      } else {
        rc.choiceElements.flatMap { choiceElement =>
          listItem(rc.label, XmlFormat.raw(choiceElement.label)) ::
            choiceElement.fields.map(renderPageField)
        }
      }

      XmlFormat.fill(renderedElements)
    case GroupField(label, groupFields) =>
      listItemForGroup(label, listBlockForGroup(XmlFormat.fill(groupFields.map(renderPageField))))
    case _ =>
      XmlFormat.empty
  }

  def renderAddToListSummaryItemBody(content: String) = {

    val (boldText, remainingText) = if (content.contains("<strong>")) {
      // the regex pattern to find text between <strong> </strong>
      val patternStrong = "<strong>(.*?)</strong>".r

      patternStrong.findFirstMatchIn(content) match {
        case Some(m) =>
          val bold = m.group(1)
          val rest = content.substring(m.end).trim
          (bold, rest)
        case None =>
          ("", content)
      }

    } else if (content.contains("#")) {
      val newLine = "\n\n"
      val lines = content.split(newLine)
      (lines.headOption.map(_.replace("#", "")).getOrElse(""), lines.tail.mkString(newLine))
    } else {
      // the regex pattern to find text between **
      val patternBold = "\\*\\*(.*?)\\*\\*".r

      patternBold.findFirstMatchIn(content) match {
        case Some(m) =>
          val bold = m.group(1)
          val rest = content.substring(m.end).trim
          (bold, rest)
        case None =>
          ("", content)
      }
    }

    addToListSummaryItemBody(boldText, PdfHelper.renderHtml(remainingText))
  }
}
