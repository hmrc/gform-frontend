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

package uk.gov.hmrc.gform.instructions

import org.jsoup.nodes.Document
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.Request
import play.twirl.api.Html
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.commons.MarkDownUtil.markDownParser
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadAlgebra }
import uk.gov.hmrc.gform.gform.{ HtmlSanitiser, SummaryPagePurpose }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.summary.SummaryRenderingService.getSummaryListRows
import uk.gov.hmrc.gform.validation.{ ValidationResult, ValidationService }
import uk.gov.hmrc.gform.views.html.summary.snippets.begin_section
import uk.gov.hmrc.gform.views.html.summary.summary
import uk.gov.hmrc.govukfrontend.views.html.components.govukSummaryList
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{ SummaryList, SummaryListRow }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Section.AddToList
import uk.gov.hmrc.gform.summary.{ SubmissionDetails, SummaryRenderingService }
import uk.gov.hmrc.gform.views.summary.SummaryListRowHelper.summaryListRow

import scala.concurrent.{ ExecutionContext, Future }

class InstructionsRenderingService(
  i18nSupport: I18nSupport,
  fileUploadAlgebra: FileUploadAlgebra[Future],
  validationService: ValidationService,
  frontendAppConfig: FrontendAppConfig
) {

  def createHtmlForInstructionsPdf[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    submissionDetails: Option[SubmissionDetails],
    summaryPagePurpose: SummaryPagePurpose,
    formModelOptics: FormModelOptics[D])(
    implicit
    request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[PdfHtml] = {
    import i18nSupport._

    for {
      instructionsHtml <- getInstructionsHTML(maybeAccessCode, cache, summaryPagePurpose, formModelOptics)
    } yield {
      PdfHtml(
        HtmlSanitiser
          .sanitiseHtmlForPDF(
            instructionsHtml,
            document => {
              val submissionDetailsString =
                SummaryRenderingService.addSubmissionDetailsToDocument(submissionDetails, cache)
              addHeaderFooterSubmissionDetails(cache.formTemplate, submissionDetailsString, document)
            }
          ))
    }
  }

  private def getInstructionsHTML[D <: DataOrigin](
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
    import i18nSupport._

    for {
      envelope <- fileUploadAlgebra.getEnvelope(cache.form.envelopeId)
      validationResult <- validationService
                           .validateFormModel(cache.toCacheData, envelope, formModelOptics.formModelVisibilityOptics)
    } yield
      renderInstructionsHtml(
        cache.formTemplate,
        validationResult,
        formModelOptics,
        maybeAccessCode,
        envelope,
        cache.retrievals,
        frontendAppConfig,
        cache.form.thirdPartyData.obligations,
        cache.form.thirdPartyData.reviewComments,
        summaryPagePurpose
      )
  }

  private def renderInstructionsHtml[D <: DataOrigin](
    formTemplate: FormTemplate,
    validationResult: ValidationResult,
    formModelOptics: FormModelOptics[D],
    maybeAccessCode: Option[AccessCode],
    envelope: Envelope,
    retrievals: MaterialisedRetrievals,
    frontendAppConfig: FrontendAppConfig,
    obligations: Obligations,
    reviewerComments: Option[String],
    summaryPagePurpose: SummaryPagePurpose
  )(
    implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): Html = {

    val envelopeUpd =
      summaryPagePurpose match {
        case SummaryPagePurpose.ForUser => envelope.withUserFileNames
        case SummaryPagePurpose.ForDms  => envelope
      }

    summary(
      formTemplate,
      htmlRows(
        validationResult,
        formModelOptics,
        maybeAccessCode,
        formTemplate,
        envelopeUpd,
        obligations
      ),
      maybeAccessCode,
      formTemplate.formCategory,
      retrievals.renderSaveAndComeBackLater,
      retrievals.continueLabelKey,
      frontendAppConfig,
      summaryPagePurpose,
      reviewerComments,
      Html(""),
      Html("")
    )
  }

  private def htmlRows[D <: DataOrigin](
    validationResult: ValidationResult,
    formModelOptics: FormModelOptics[D],
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate,
    envelope: Envelope,
    obligations: Obligations
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator
  ): List[Html] = {

    val formModel = formModelOptics.formModelVisibilityOptics.formModel

    def renderHtmls(singleton: Singleton[Visibility], sectionNumber: SectionNumber)(implicit l: LangADT): List[Html] =
      (for {
        pageInstruction <- singleton.page.instruction
        begin = begin_section(pageInstruction.name)
        sectionTitle4Ga = sectionTitle4GaFactory(pageInstruction.name, sectionNumber)
        fields = singleton.page.fields
          .filterNot(_.hideOnSummary)
          .filter(_.instruction.isDefined)
          .sortBy(_.instruction.flatMap(_.order).getOrElse(Integer.MAX_VALUE))
        middleRows: List[SummaryListRow] = fields
          .flatMap(
            formComponent =>
              getSummaryListRows(
                formComponent,
                formTemplate._id,
                formModelOptics.formModelVisibilityOptics,
                maybeAccessCode,
                sectionNumber,
                sectionTitle4Ga,
                obligations,
                validationResult,
                envelope,
                getInstructionLabel
            ))
      } yield {
        if (middleRows.isEmpty) {
          Nil
        } else {
          val middleRowsHtml = new govukSummaryList()(SummaryList(middleRows, "govuk-!-margin-bottom-9"))
          List(begin, middleRowsHtml)
        }
      }).getOrElse(List.empty)

    def addToListRender(addToList: Section.AddToList): Html = {
      val repeaters: List[Repeater[Visibility]] = formModel.repeaters(addToList.id)
      val recordTable: List[SmartString] = repeaters.map(_.expandedDescription)
      val value = recordTable.map(_.value()).mkString("</br>")

      val slr: SummaryListRow = summaryListRow(
        addToList.title.value(),
        value,
        None,
        "",
        "",
        "",
        Nil
      )

      new govukSummaryList()(SummaryList(slr :: Nil, "govuk-!-margin-bottom-5"))
    }

    def instructionOrder(section: Section) =
      section
        .fold(_.page.instruction.flatMap(_.order))(_.page.instruction.flatMap(_.order))(_.instruction.flatMap(_.order))
        .getOrElse(Integer.MAX_VALUE)

    formModel.pagesWithIndex
      .groupBy {
        case (pageModel, _) => pageModel.source
      }
      .toList
      .sortBy {
        case (section, _) => instructionOrder(section)
      }
      .flatMap {
        case (a: AddToList, pagesWithSectionNumber) =>
          List(addToListRender(a)) ++ pagesWithSectionNumber
            .collect {
              case (singleton: Singleton[Visibility], sectionNumber) =>
                (formModel.repeaterForSingleton(singleton, sectionNumber).get, (singleton, sectionNumber))
            }
            .groupBy {
              case (repeater, _) => repeater
            }
            .mapValues(_.map(_._2))
            .toList
            .sortBy {
              case (repeater, _) => repeater.index
            }
            .flatMap {
              case (repeater: Repeater[Visibility], pages: List[(Singleton[Visibility], SectionNumber)]) =>
                List(begin_section(repeater.expandedShortName)) ++ pages
                  .flatMap {
                    case (pageModel, sectionNumber) => renderHtmls(pageModel, sectionNumber)
                  }
              case _ => List.empty
            }
        case (_, pagesWithSectionNumber) =>
          pagesWithSectionNumber
            .flatMap {
              case (pModel, _) => pModel.fold(renderHtmls(_, formModel.pagesMap(pModel)))(_ => Nil)
            }
      }
  }

  private def addHeaderFooterSubmissionDetails(
    formTemplate: FormTemplate,
    submissionDetails: String,
    document: Document)(implicit lise: SmartStringEvaluator): Unit = {
    val mayBeInstructionPdf = formTemplate.destinations match {
      case DestinationList(_, acknowledgementSection, _) =>
        acknowledgementSection.instructionPdf
      case _ => None
    }

    val form = document.getElementsByTag("form").first()
    mayBeInstructionPdf.flatMap(_.header).map(ss => h1(markDownParser(ss).toString)).foreach(form.prepend)
    form.append(submissionDetails)
    mayBeInstructionPdf.flatMap(_.footer).map(ss => h1(markDownParser(ss).toString)).foreach(form.append)
  }

  private def getInstructionLabel(formComponent: FormComponent)(implicit lise: SmartStringEvaluator): String =
    formComponent.instruction.map(_.name.value()).getOrElse(SmartString.empty.value())

  private def h1(content: String): String = s"""<h1 class="govuk-heading-l">$content</h1>"""
}
