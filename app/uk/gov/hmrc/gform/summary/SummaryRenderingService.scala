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

package uk.gov.hmrc.gform.summary

import cats.data.NonEmptyList
import java.time.format.DateTimeFormatter

import cats.syntax.eq._
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.{ Call, Request }
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.commons.MarkDownUtil.markDownParser
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadAlgebra }
import uk.gov.hmrc.gform.gform.{ HtmlSanitiser, SummaryPagePurpose }
import uk.gov.hmrc.gform.gform.routes
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.models.{ SectionSelectorType, Visibility }
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.models.{ Bracket, FastForward, RepeaterWithNumber, SectionSelector, Singleton }
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
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{ SummaryList, SummaryListRow }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.govukfrontend.views.html.components.govukSummaryList

import scala.concurrent.{ ExecutionContext, Future }

class SummaryRenderingService(
  i18nSupport: I18nSupport,
  fileUploadAlgebra: FileUploadAlgebra[Future],
  recalculation: Recalculation[Future, Throwable],
  validationService: ValidationService,
  frontendAppConfig: FrontendAppConfig) {

  def createHtmlForPdf[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    submissionDetails: Option[SubmissionDetails],
    summaryPagePurpose: SummaryPagePurpose,
    formModelOptics: FormModelOptics[D]
  )(
    implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[PdfHtml] =
    for {
      summaryHtml <- getSummaryHTML(maybeAccessCode, cache, summaryPagePurpose, formModelOptics)
    } yield {
      val submissionDetailsString = SummaryRenderingService.addSubmissionDetailsToDocument(submissionDetails, cache)
      PdfHtml(
        HtmlSanitiser
          .sanitiseHtmlForPDF(
            summaryHtml,
            document => {
              document.title(s"${messages("summary.acknowledgement.pdf")} - ${cache.formTemplate.formName.value}")
              HtmlSanitiser.acknowledgementPdf(document, submissionDetailsString, cache.formTemplate)
            }
          ))
    }

  def createHtmlForPrintPdf(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    summaryPagePurpose: SummaryPagePurpose,
    pdf: PrintSection.Pdf,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(
    implicit request: Request[_],
    messages: Messages,
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[PdfHtml] =
    for {
      summaryHtml <- getSummaryHTML(maybeAccessCode, cache, summaryPagePurpose, formModelOptics)
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
          ))
    }

  def createHtmlForNotificationPdf(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    summaryPagePurpose: SummaryPagePurpose,
    pdfNotification: PdfNotification,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(
    implicit request: Request[_],
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
                  formModelOptics)
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
          ))
    }
  }

  private def addDataToPrintPdfHTML(
    pdfHeader: SmartString,
    pdfFooter: SmartString
  )(
    implicit
    lise: SmartStringEvaluator
  ): (String, String) = {

    val headerHtml = markDownParser(pdfHeader).toString
    val footerHtml = markDownParser(pdfFooter).toString

    (headerHtml, footerHtml)

  }

  def getSummaryHTML[D <: DataOrigin](
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    summaryPagePurpose: SummaryPagePurpose,
    formModelOptics: FormModelOptics[D]
  )(
    implicit
    request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator): Future[Html] = {
    val envelopeF = fileUploadAlgebra.getEnvelope(cache.form.envelopeId)

    import i18nSupport._

    for {
      envelope <- envelopeF
      validationResult <- validationService
                           .validateFormModel(cache.toCacheData, envelope, formModelOptics.formModelVisibilityOptics)
    } yield
      SummaryRenderingService.renderSummary(
        cache.formTemplate,
        validationResult,
        formModelOptics,
        maybeAccessCode,
        envelope,
        cache.retrievals,
        frontendAppConfig,
        cache.form.thirdPartyData.obligations,
        summaryPagePurpose
      )

  }

  def getNotificationPdfHTML(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    summaryPagePurpose: SummaryPagePurpose,
    pdfFieldIds: List[FormComponentId],
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(
    implicit
    request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[Html] = {

    val envelopeF = fileUploadAlgebra.getEnvelope(cache.form.envelopeId)

    import i18nSupport._

    for {
      envelope <- envelopeF
      validationResult <- validationService
                           .validateFormModel(cache.toCacheData, envelope, formModelOptics.formModelVisibilityOptics)
    } yield
      SummaryRenderingService.renderNotificationPdfSummary(
        cache.formTemplate,
        validationResult,
        formModelOptics.formModelVisibilityOptics,
        maybeAccessCode,
        envelope,
        cache.retrievals,
        frontendAppConfig,
        cache.form.thirdPartyData.obligations,
        summaryPagePurpose,
        pdfFieldIds
      )
  }
}

object SummaryRenderingService {

  def renderSummary[D <: DataOrigin](
    formTemplate: FormTemplate,
    validationResult: ValidationResult,
    formModelOptics: FormModelOptics[D],
    maybeAccessCode: Option[AccessCode],
    envelope: Envelope,
    retrievals: MaterialisedRetrievals,
    frontendAppConfig: FrontendAppConfig,
    obligations: Obligations,
    summaryPagePurpose: SummaryPagePurpose
  )(
    implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): Html = {
    val headerHtml = markDownParser(formTemplate.summarySection.header)
    val footerHtml = markDownParser(formTemplate.summarySection.footer)

    val envelopeUpd =
      summaryPagePurpose match {
        case SummaryPagePurpose.ForUser => envelope.withUserFileNames
        case SummaryPagePurpose.ForDms  => envelope
      }
    val sfr =
      summaryRowsForRender(
        validationResult,
        formModelOptics,
        maybeAccessCode,
        formTemplate,
        envelopeUpd,
        obligations
      )
    summary(
      formTemplate,
      sfr,
      maybeAccessCode,
      formTemplate.formCategory,
      retrievals.renderSaveAndComeBackLater,
      retrievals.continueLabelKey,
      frontendAppConfig,
      summaryPagePurpose,
      None,
      headerHtml,
      footerHtml
    )
  }

  def renderNotificationPdfSummary(
    formTemplate: FormTemplate,
    validationResult: ValidationResult,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    maybeAccessCode: Option[AccessCode],
    envelope: Envelope,
    retrievals: MaterialisedRetrievals,
    frontendAppConfig: FrontendAppConfig,
    obligations: Obligations,
    summaryPagePurpose: SummaryPagePurpose,
    pdfFieldIds: List[FormComponentId]
  )(
    implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator
  ): Html = {
    val headerHtml = markDownParser(formTemplate.summarySection.header)
    val footerHtml = markDownParser(formTemplate.summarySection.footer)
    val sfr =
      summaryForNotificationPdf(
        validationResult,
        formModelVisibilityOptics,
        maybeAccessCode,
        formTemplate,
        envelope,
        obligations,
        pdfFieldIds
      )
    summary(
      formTemplate,
      sfr,
      maybeAccessCode,
      formTemplate.formCategory,
      retrievals.renderSaveAndComeBackLater,
      retrievals.continueLabelKey,
      frontendAppConfig,
      summaryPagePurpose,
      None,
      headerHtml,
      footerHtml
    )
  }

  def summaryRowsForRender[D <: DataOrigin](
    validationResult: ValidationResult,
    formModelOptics: FormModelOptics[D],
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate,
    envelope: Envelope,
    obligations: Obligations,
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator
  ): List[Html] = {

    val formModel = formModelOptics.formModelVisibilityOptics.formModel

    def renderHtmls(singleton: Singleton[Visibility], sectionNumber: SectionNumber, source: Section)(
      implicit l: LangADT): List[Html] = {
      val page = singleton.page
      val sectionTitle4Ga = sectionTitle4GaFactory(page.title, sectionNumber)
      val pageTitle = page.shortName.getOrElse(page.title)

      val begin = source.fold { _ =>
        begin_section(pageTitle)
      } { _ =>
        begin_section(pageTitle)
      } { addToList =>
        addToList.presentationHint
          .filter(_ == InvisiblePageTitleInSummary)
          .fold(begin_section(pageTitle))(_ => HtmlFormat.empty)
      }

      val middleRows: List[SummaryListRow] = page.fields
        .filterNot(_.hideOnSummary)
        .flatMap(
          formComponent =>
            FormComponentSummaryRenderer.summaryListRows[D, SummaryRender](
              formComponent,
              formTemplate._id,
              formModelOptics.formModelVisibilityOptics,
              maybeAccessCode,
              sectionNumber,
              sectionTitle4Ga,
              obligations,
              validationResult,
              envelope
          )
        )

      if (middleRows.isEmpty) {
        Nil
      } else {
        val middleRowsHtml = new govukSummaryList()(SummaryList(middleRows, "govuk-!-margin-bottom-5"))
        List(begin, middleRowsHtml)
      }
    }

    def addToListSummary(bracket: Bracket.AddToList[Visibility]): Html =
      begin_section(bracket.source.summaryName)

    def addToListRenderBracket(bracket: Bracket.AddToList[Visibility]): List[Html] = {
      val repeaters: NonEmptyList[RepeaterWithNumber[Visibility]] = bracket.iterations.map(_.repeater)

      val htmls: List[Html] = bracket.iterations.toList.flatMap { iteration =>
        begin_section(iteration.repeater.repeater.expandedShortName) :: {
          iteration.singletons.toList.flatMap { singletonWithNumber =>
            renderHtmls(singletonWithNumber.singleton, singletonWithNumber.sectionNumber, bracket.source)
          }
        }
      }

      val addToListItemSummaries: NonEmptyList[SmartString] = repeaters.map(_.repeater.expandedDescription)

      val lastRepeaterWithNumber = repeaters.last

      val repeater = lastRepeaterWithNumber.repeater
      val sectionNumber = lastRepeaterWithNumber.sectionNumber

      val sectionTitle4Ga: SectionTitle4Ga = sectionTitle4GaFactory(repeater.expandedTitle, sectionNumber)

      val url: Call = routes.FormController
        .form(formTemplate._id, maybeAccessCode, sectionNumber, sectionTitle4Ga, SuppressErrors.Yes, FastForward.Yes)

      val addToListSummary = addToListItemSummaries.map(ss => markDownParser(ss).toString).toList.mkString("")

      val slr: SummaryListRow = summaryListRow(
        repeater.title
          .value(), // This is weird to use, as it can have $n, but this list in shown only once. Should we have other property here?
        addToListSummary,
        None,
        "",
        "",
        "",
        (url, messages("addToList.addOrRemove")) :: Nil
      )

      new govukSummaryList()(SummaryList(slr :: Nil, "govuk-!-margin-bottom-5")) :: htmls
    }

    val brackets: NonEmptyList[Bracket[Visibility]] = formModel.brackets.brackets

    brackets.toList.flatMap {
      case bracket @ Bracket.AddToList(_, _) => List(addToListSummary(bracket)) ++ addToListRenderBracket(bracket)
      case Bracket.RepeatingPage(singletons, source) =>
        singletons.toList.flatMap { singletonWithNumber =>
          renderHtmls(singletonWithNumber.singleton, singletonWithNumber.sectionNumber, source)
        }
      case Bracket.NonRepeatingPage(singleton, sectionNumber, source) =>
        renderHtmls(singleton, sectionNumber, source)
    }
  }

  def summaryForNotificationPdf(
    validationResult: ValidationResult,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate,
    envelope: Envelope,
    obligations: Obligations,
    pdfFieldIds: List[FormComponentId]
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): List[Html] = {

    def renderHtmls(fields: List[FormComponent])(implicit l: LangADT): List[Html] = {
      val rows = fields
        .flatMap(
          formComponent =>
            FormComponentSummaryRenderer.summaryListRows[DataOrigin.Mongo, SummaryRender](
              formComponent,
              formTemplate._id,
              formModelVisibilityOptics,
              maybeAccessCode,
              SectionNumber(0),
              SectionTitle4Ga(""),
              obligations,
              validationResult,
              envelope
          ))

      List(new govukSummaryList()(SummaryList(rows)))
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

  def addSubmissionDetailsToDocument[U <: SectionSelectorType: SectionSelector](
    submissionDetails: Option[SubmissionDetails],
    cache: AuthCacheWithForm
  )(
    implicit
    messages: Messages
  ): String = {
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val formattedTime = submissionDetails.map(sd =>
      s"""${sd.submission.submittedDate.format(dateFormat)} ${sd.submission.submittedDate.format(timeFormat)}""")

    val rows = List(
      formattedTime.map(ft => cya_row(messages("submission.date"), ft)),
      submissionDetails.map(sd => cya_row(messages("submission.reference"), sd.submission.submissionRef.toString)),
      submissionDetails.map(sd => cya_row(messages("submission.mark"), sd.hashedValue))
    ).flatten

    cya_section(messages("submission.details"), HtmlFormat.fill(rows)).toString()

  }

}
