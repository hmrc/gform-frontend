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

package uk.gov.hmrc.gform.pdf

import play.api.i18n.Messages
import play.api.mvc.Request
import play.twirl.api.Html
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.objectStore.{ EnvelopeWithMapping, ObjectStoreAlgebra }
import uk.gov.hmrc.gform.gform.SummaryPagePurpose
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.pdf.model.PDFModel.HeaderFooter
import uk.gov.hmrc.gform.pdf.model.{ PDFCustomRender, PDFLayout, PDFModel, PDFPageModelBuilder, PDFType }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, PdfContent }
import uk.gov.hmrc.gform.summary.SubmissionDetails
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.gform.views.html.summary.{ summaryPdf, summaryTabularPdf }
import uk.gov.hmrc.gform.views.xml.summary.pdf.summary
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class PDFRenderService(
  objectStoreAlgebra: ObjectStoreAlgebra[Future],
  validationService: ValidationService
) {

  def createPDFContent[D <: DataOrigin, U <: SectionSelectorType: SectionSelector, T <: PDFType](
    title: String,
    maybePageTitle: Option[String],
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[D],
    maybeHeaderFooter: Option[HeaderFooter],
    maybeSubmissionDetails: Option[SubmissionDetails],
    purpose: SummaryPagePurpose,
    summaryDeclaration: Option[Html],
    maybeDraftText: Option[String] = None,
    maybePdfOptions: Option[PDFModel.Options] = None,
    maybeFormName: Option[String] = None
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator,
    pdfFunctions: PDFCustomRender[T]
  ): Future[PdfContent] =
    for {
      envelopeWithMapping <- objectStoreAlgebra
                               .getEnvelope(cache.form.envelopeId)
                               .map(EnvelopeWithMapping(_, cache.form))
      validationResult <-
        validationService
          .validateFormModel(cache.toCacheData, envelopeWithMapping, formModelOptics.formModelVisibilityOptics, None)
    } yield {
      val envelopeByPurpose = envelopeWithMapping.byPurpose(purpose)
      val pdfModel = PDFPageModelBuilder.makeModel(formModelOptics, cache, envelopeByPurpose, validationResult)

      val layout =
        if (maybePdfOptions.flatMap(_.tabularFormat).getOrElse(true)) PDFLayout.Tabular else pdfFunctions.layout

      val includeSignatureBox = maybePdfOptions.flatMap(_.includeSignatureBox).getOrElse(false)

      val content = if (cache.formTemplate.accessiblePdf) {
        summary(
          title,
          maybeFormName,
          pdfModel,
          maybeHeaderFooter.flatMap(_.header.map(_.rawDefaultValue).filter(_.isEmpty)),
          maybeHeaderFooter.flatMap(_.footer.map(_.rawDefaultValue).filter(_.isEmpty)),
          maybeSubmissionDetails,
          summaryDeclaration,
          maybeDraftText,
          includeSignatureBox
        ).body
      } else {
        layout match {
          case PDFLayout.Default =>
            summaryPdf(
              title,
              maybePageTitle,
              pdfModel,
              maybeHeaderFooter,
              maybeSubmissionDetails,
              cache.formTemplate,
              summaryDeclaration,
              maybeDraftText,
              includeSignatureBox
            ).toString
          case PDFLayout.Tabular =>
            summaryTabularPdf(
              title,
              pdfModel,
              maybeHeaderFooter,
              cache.formTemplate,
              maybeFormName,
              maybeDraftText,
              maybeSubmissionDetails,
              summaryDeclaration,
              includeSignatureBox
            ).toString
        }
      }
      PdfContent(content)
    }
}
