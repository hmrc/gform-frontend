/*
 * Copyright 2025 HM Revenue & Customs
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

import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.Request
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.CustomerIdRecalculation
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.pdf.PDFRenderService
import uk.gov.hmrc.gform.pdf.model.{ PDFModel, PDFType }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormIdData, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, PdfContent }
import uk.gov.hmrc.gform.summary.SubmissionDetails
import uk.gov.hmrc.gform.summarypdf.{ FopService, PdfGeneratorService }
import uk.gov.hmrc.gform.validation.ValidationResult
import uk.gov.hmrc.http.NotFoundException
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendHeaderCarrierProvider

import scala.concurrent.{ ExecutionContext, Future }

class AcknowledgementPdfService(
  i18nSupport: I18nSupport,
  nonRepudiationHelpers: NonRepudiationHelpers,
  renderer: SectionRenderingService,
  auditService: AuditService,
  gformConnector: GformConnector,
  pdfRenderService: PDFRenderService,
  pdfGeneratorService: PdfGeneratorService,
  fopService: FopService
)(implicit ec: ExecutionContext)
    extends FrontendHeaderCarrierProvider {

  def getByteArrayFop(pdfContent: PdfContent): Future[Array[Byte]] =
    fopService.render(pdfContent.content)

  def getByteArrayPdf(pdfContent: PdfContent): Future[Array[Byte]] =
    for {
      pdfF <- pdfGeneratorService.generateByteArrayPDF(pdfContent)
    } yield pdfF.toByteArray

  def getRenderedPdfSize(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[_],
    l: LangADT,
    ss: SmartStringEvaluator
  ): Future[Int] =
    for {
      pdfContent <- createPDFContent(cache, maybeAccessCode, formModelOptics, sendAuditEvent = false)
      pdfSize <- if (cache.formTemplate.accessiblePdf) {
                   for {
                     pdfSource <- getByteArrayFop(pdfContent)
                   } yield pdfSource.length
                 } else {
                   for {
                     pdfSource <- getByteArrayPdf(pdfContent)
                   } yield pdfSource.length
                 }
    } yield pdfSize

  def createPDFContent(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    sendAuditEvent: Boolean
  )(implicit
    request: Request[_],
    l: LangADT,
    ss: SmartStringEvaluator
  ): Future[PdfContent] = {
    import i18nSupport._
    val messages: Messages = request2Messages(request)
    val formString = nonRepudiationHelpers.formDataToJson(cache.form)
    val hashedValue = nonRepudiationHelpers.computeHash(formString)

    val maybePDFHeaderFooter = cache.formTemplate.destinations match {
      case d: DestinationList => d.acknowledgementSection.pdf.map(p => (p.header, p.footer))
      case _                  => None
    }

    val maybePdfOptions = cache.formTemplate.destinations match {
      case d: DestinationList =>
        d.acknowledgementSection.pdf.map(p => PDFModel.Options(p.tabularFormat, p.includeSignatureBox))
      case _ => None
    }

    val summarySectionDeclaration = renderer.renderSummarySectionDeclaration(
      cache,
      formModelOptics,
      maybeAccessCode,
      cache.formTemplate.summarySection.excludeFieldsFromPDF,
      ValidationResult.empty
    )

    for {
      _ <- if (sendAuditEvent) {
             val customerId = CustomerIdRecalculation
               .evaluateCustomerId[DataOrigin.Mongo, SectionSelectorType.WithAcknowledgement](
                 cache,
                 formModelOptics.formModelVisibilityOptics
               )
             val eventId = auditService
               .calculateSubmissionEvent(
                 cache.form,
                 formModelOptics.formModelVisibilityOptics,
                 cache.retrievals,
                 customerId
               )
               .eventId

             nonRepudiationHelpers.sendAuditEvent(hashedValue, formString, eventId)
             Future.unit
           } else Future.unit
      submission <- gformConnector
                      .maybeOneOfSubmissionDetails(
                        FormIdData(cache.retrievals, cache.formTemplate._id, maybeAccessCode),
                        FormIdData(cache.retrievals, cache.form.formTemplateId, maybeAccessCode),
                        cache.form.envelopeId
                      )
                      .map(
                        _.getOrElse(
                          throw new NotFoundException(s"Submission for envelope id ${cache.form.envelopeId} not found.")
                        )
                      )
      pdfContent <-
        pdfRenderService
          .createPDFContent[DataOrigin.Mongo, SectionSelectorType.WithAcknowledgement, PDFType.Summary](
            s"${messages("summary.acknowledgement.pdf")} - ${cache.formTemplate.formName.value}",
            None,
            cache,
            formModelOptics,
            maybePDFHeaderFooter.map { case (maybeHeader, maybeFooter) =>
              PDFModel.HeaderFooter(maybeHeader, maybeFooter)
            },
            Some(SubmissionDetails(submission, hashedValue)),
            SummaryPagePurpose.ForUser,
            Some(summarySectionDeclaration),
            None,
            maybePdfOptions,
            Some(cache.formTemplate.formName.value)
          )
    } yield pdfContent
  }

}
