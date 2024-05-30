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

import org.slf4j.LoggerFactory
import play.api.http.HttpEntity
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents, Request, ResponseHeader, Result }
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.auth.models.{ CompositeAuthDetails, OperationWithForm }
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActionsAlgebra }
import uk.gov.hmrc.gform.controllers.GformSessionKeys.COMPOSITE_AUTH_DETAILS_SESSION_KEY
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, PdfHtml }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.graph.CustomerIdRecalculation
import uk.gov.hmrc.gform.pdf.PDFRenderService
import uk.gov.hmrc.gform.pdf.model.{ PDFModel, PDFType }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.gform.summary.SubmissionDetails
import uk.gov.hmrc.http.BadRequestException
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class AcknowledgementController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  pdfService: PdfGeneratorService,
  pdfRenderService: PDFRenderService,
  renderer: SectionRenderingService,
  gformConnector: GformConnector,
  nonRepudiationHelpers: NonRepudiationHelpers,
  messagesControllerComponents: MessagesControllerComponents,
  auditService: AuditService
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  private val logger = LoggerFactory.getLogger(getClass)

  def showAcknowledgement(
    maybeAccessCode: Option[AccessCode],
    formTemplateId: FormTemplateId
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.ViewAcknowledgement
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      import i18nSupport._
      val formTemplateContext = request.attrs(FormTemplateKey)
      val formTemplate = formTemplateContext.formTemplate
      val compositeAuthDetails: CompositeAuthDetails =
        jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)

      formTemplate.authConfig match {
        case Composite(_) =>
          val authConfigName =
            AuthConfig.authConfigNameInLogs(compositeAuthDetails.get(formTemplateContext).getOrElse(""))
          logger.info(
            s"For a template, ${cache.formTemplateId.value} with composite config user has selected " +
              s"$authConfigName config " +
              s"and submitted a form with envelopeId ${cache.form.envelopeId}"
          )
        case config =>
          logger.info(
            s"For a template, ${cache.formTemplateId.value} with ${AuthConfig.authConfigNameInLogs(config.authConfigName)} config " +
              s"user has submitted a form with envelopeId ${cache.form.envelopeId}"
          )
      }
      cache.formTemplate.destinations match {
        case destinationList: DestinationList =>
          Future.successful(
            Ok(
              renderer
                .renderAcknowledgementSection(
                  maybeAccessCode,
                  cache,
                  destinationList,
                  formModelOptics
                )
            )
          )
        case _ =>
          Future.failed(new BadRequestException(s"Acknowledgement is not defined for ${cache.formTemplateId}"))
      }
    }

  def createPDFHtml(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    sendAuditEvent: Boolean
  )(implicit
    request: Request[_],
    l: LangADT,
    ss: SmartStringEvaluator
  ): Future[PdfHtml] = {
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
      None
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
           } else Future.unit
      submission <- gformConnector.submissionDetails(
                      FormIdData(cache.retrievals, cache.formTemplate._id, maybeAccessCode),
                      cache.form.envelopeId
                    )
      pdfHtml <-
        pdfRenderService
          .createPDFHtml[DataOrigin.Mongo, SectionSelectorType.WithAcknowledgement, PDFType.Summary](
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
    } yield pdfHtml
  }

  def downloadPDF(maybeAccessCode: Option[AccessCode], formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.WithAcknowledgement](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.ViewAcknowledgement
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      for {
        pdfHtml   <- createPDFHtml(cache, maybeAccessCode, formModelOptics, sendAuditEvent = true)
        pdfSource <- pdfService.generatePDF(pdfHtml)
      } yield Result(
        header = ResponseHeader(200, Map.empty),
        body = HttpEntity.Streamed(pdfSource, None, Some("application/pdf"))
      )
    }

  def exitSurvey(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    Action.async { request =>
      Future.successful(Redirect(s"/feedback/${formTemplateId.value}").withNewSession)
    }

  def changeStateAndRedirectToCYA(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.ForceUpdateFormStatus
    ) { implicit request => _ => cache => _ => _ =>
      gformConnector
        .updateUserData(
          FormIdData(cache.retrievals, formTemplateId, maybeAccessCode),
          UserData(
            cache.form.formData,
            InProgress,
            cache.form.visitsIndex,
            cache.form.thirdPartyData,
            cache.form.componentIdToFileId
          )
        )
        .flatMap { _ =>
          Future.successful(
            Redirect(routes.SummaryController.summaryById(formTemplateId, maybeAccessCode, None, None, true, None))
          )
        }
    }

}
