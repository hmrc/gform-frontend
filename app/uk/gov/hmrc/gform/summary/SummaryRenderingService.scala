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
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.commons.MarkDownUtil.markDownParser
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.fileupload.{ EnvelopeWithMapping, FileUploadAlgebra }
import uk.gov.hmrc.gform.gform.{ HtmlSanitiser, SectionRenderingService, SummaryPagePurpose, routes }
import uk.gov.hmrc.gform.gformbackend.GformConnector
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
import uk.gov.hmrc.govukfrontend.views.viewmodels.notificationbanner.NotificationBanner
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{ Card, CardTitle, SummaryList, SummaryListRow }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.govukfrontend.views.html.components.GovukSummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.content

import java.time.format.DateTimeFormatter
import scala.concurrent.{ ExecutionContext, Future }

class SummaryRenderingService(
  renderer: SectionRenderingService,
  i18nSupport: I18nSupport,
  fileUploadAlgebra: FileUploadAlgebra[Future],
  validationService: ValidationService,
  frontendAppConfig: FrontendAppConfig,
  gformConnector: GformConnector
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
  ): Future[PdfHtml] =
    for {
      summaryHtml <-
        getSummaryHTML(maybeAccessCode, cache, summaryPagePurpose, formModelOptics, maybeCoordinates, None, false)
    } yield {
      val (headerStr, footerStr) = addDataToPrintPdfHTML(pdf.header, pdf.footer)
      PdfHtml(
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
  ): Future[PdfHtml] = {
    val pdfFieldIds = pdfNotification.fieldIds
    val pdfHeader = pdfNotification.header
    val pdfFooter = pdfNotification.footer

    for {
      pdfHtml <- getNotificationPdfHTML(
                   cache.form.formTemplateId,
                   maybeAccessCode,
                   cache,
                   summaryPagePurpose,
                   pdfFieldIds,
                   formModelOptics
                 )
    } yield {
      val (headerStr, footerStr) = addDataToPrintPdfHTML(pdfHeader, pdfFooter)
      PdfHtml(
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
    taskComplete: Boolean
  )(implicit
    request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[Html] = {
    val envelopeF = fileUploadAlgebra
      .getEnvelope(cache.form.envelopeId)(cache.formTemplate.isObjectStore)
      .map(EnvelopeWithMapping(_, cache.form))

    import i18nSupport._

    for {
      envelope <- envelopeF
      validationResult <-
        validationService
          .validateFormModel(cache.toCacheData, envelope, formModelOptics.formModelVisibilityOptics, maybeCoordinates)
      notificatioBanner <- gformConnector.notificationBanner
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
        taskComplete,
        notificatioBanner.map(_.toViewNotificationBanner)
      )
    }

  }

  def getNotificationPdfHTML(
    formTemplateId: FormTemplateId,
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

    val envelopeF = fileUploadAlgebra
      .getEnvelope(cache.form.envelopeId)(cache.formTemplate.isObjectStore)
      .map(EnvelopeWithMapping(_, cache.form))

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
    taskComplete: Boolean,
    notificationBanner: Option[NotificationBanner]
  )(implicit request: Request[_], messages: Messages, l: LangADT, lise: SmartStringEvaluator): Html = {
    val headerHtml = markDownParser(summarySection.header)
    val footerHtml = markDownParser(summarySection.footer)
    val caption = summarySection.caption.map(_.value)
    val title = summarySection.title.value

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

    val sfr =
      summaryRowsForRender(
        validationResult,
        formModelOptics,
        maybeAccessCode,
        formTemplate,
        envelopeUpd,
        obligations,
        addressRecordLookup,
        maybeCoordinates
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
        None,
        title,
        caption,
        headerHtml,
        summaryDeclaration,
        footerHtml,
        formDataFingerprint,
        summarySection.displayWidth,
        maybeCoordinates,
        taskComplete,
        notificationBanner
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
    val title = formTemplate.summarySection.title.value
    val caption = formTemplate.summarySection.caption.map(_.value)
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
        None,
        title,
        caption,
        headerHtml,
        HtmlFormat.empty,
        footerHtml,
        formDataFingerprint,
        formTemplate.summarySection.displayWidth,
        None,
        false,
        Option.empty[NotificationBanner]
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
    maybeCoordinates: Option[Coordinates]
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
      page.fields
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
            Some(ff)
          )
        )
    }

    def summaryList(begin: HtmlFormat.Appendable, rows: List[SummaryListRow], card: Option[Card]) =
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
        List(begin, govukSummaryList)
      }

    def addToListSummary(bracket: Bracket.AddToList[Visibility]): Html =
      begin_section(bracket.source.summaryName)

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
          begin_section(iteration.repeater.repeater.expandedShortName) ::
            singletons.flatMap { singletonWithNumber =>
              val begin = getPageTitle(bracket.source, singletonWithNumber.singleton.page)
              val middleRows = middleSummaryListRows(
                singletonWithNumber.singleton,
                singletonWithNumber.sectionNumber,
                Some(iteration.repeater.repeater.expandedShortName.value())
              )
              summaryList(begin, middleRows, None)
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

      val label = repeater.title.value

      val slr: SummaryListRow = summaryListRow(
        label, // This is weird to use, as it can have $n, but this list in shown only once. Should we have other property here?
        addToListSummary,
        Some(label),
        "",
        "",
        "",
        (
          url,
          messages("addToList.addOrRemove"),
          messages("addToList.addOrRemove") + " " + repeater.title
            .value()
        ) :: Nil,
        ""
      )

      val fcrd = implicitly[FormComponentRenderDetails[SummaryRender]]
      val slrTables: List[SummaryListRow] = bracket.iterations.last.repeater.repeater.fields
        .map(
          _.filterNot(_.hideOnSummary).toList
            .collect { case fc @ IsTableComp(table) =>
              val label = fcrd.label(fc)
              summaryListRow(
                label,
                markDownParser(table.summaryValue),
                Some(label),
                "",
                "",
                "",
                (
                  url,
                  messages("addToList.addOrRemove"),
                  messages("addToList.addOrRemove") + " " + repeater.title
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

    brackets.flatMap {
      case bracket @ Bracket.AddToList(_, _) => List(addToListSummary(bracket)) ++ addToListRenderBracket(bracket)
      case Bracket.RepeatingPage(singletons, source) =>
        singletons.toList.flatMap { singletonWithNumber =>
          val middleRows = middleSummaryListRows(singletonWithNumber.singleton, singletonWithNumber.sectionNumber)
          val begin = getPageTitle(source, singletonWithNumber.singleton.page)
          summaryList(begin, middleRows, None)
        }
      case Bracket.NonRepeatingPage(singleton, sectionNumber, source) =>
        val middleRows = middleSummaryListRows(singleton, sectionNumber)
        val begin = getPageTitle(source, singleton.page)
        summaryList(begin, middleRows, None)
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
            None
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
      case (Some(cl), _) => cl.value
      case (None, true)  => messages("button.continue")
      case (None, false) => messages(continueLabelKey)
    }

}
