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

package uk.gov.hmrc.gform.pdf

import play.api.i18n.Messages
import play.api.mvc.Request
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.{ EnvelopeWithMapping, FileUploadAlgebra }
import uk.gov.hmrc.gform.gform.SummaryPagePurpose
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.pdf.model.{ PDFCustomRender, PDFLayout, PDFPageModelBuilder, PDFType }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, PdfHtml }
import uk.gov.hmrc.gform.summary.SubmissionDetails
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.gform.views.html.summary.{ summaryPdf, summaryTabularPdf }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.pdf.model.PDFModel.HeaderFooter

import scala.concurrent.{ ExecutionContext, Future }

class PDFRenderService(fileUploadAlgebra: FileUploadAlgebra[Future], validationService: ValidationService) {

  def createPDFHtml[D <: DataOrigin, U <: SectionSelectorType: SectionSelector, T <: PDFType](
    title: String,
    maybePageTitle: Option[String],
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[D],
    maybeHeaderFooter: Option[HeaderFooter],
    maybeSubmissionDetails: Option[SubmissionDetails],
    purpose: SummaryPagePurpose
  )(implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator,
    pdfFunctions: PDFCustomRender[T]
  ): Future[PdfHtml] =
    for {
      envelopeWithMapping <- fileUploadAlgebra
                               .getEnvelope(cache.form.envelopeId)
                               .map(EnvelopeWithMapping(_, cache.form))
      validationResult <-
        validationService
          .validateFormModel(cache.toCacheData, envelopeWithMapping, formModelOptics.formModelVisibilityOptics)
    } yield {
      val envelopeByPurpose = envelopeWithMapping.byPurpose(purpose)
      val pdfModel = PDFPageModelBuilder.makeModel(formModelOptics, cache, envelopeByPurpose, validationResult)
      val html = pdfFunctions.layout match {
        case PDFLayout.Default =>
          summaryPdf(
            title,
            maybePageTitle,
            pdfModel,
            maybeHeaderFooter,
            maybeSubmissionDetails,
            cache.formTemplate
          ).toString
        case PDFLayout.Tabular =>
          summaryTabularPdf(
            title,
            pdfModel,
            maybeHeaderFooter,
            cache.formTemplate
          ).toString
      }
      PdfHtml(html)
    }
}
