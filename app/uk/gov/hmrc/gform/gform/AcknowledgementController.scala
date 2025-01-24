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

import org.apache.xmlgraphics.util.MimeConstants
import org.slf4j.LoggerFactory
import play.api.http.HttpEntity
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.auth.models.{ CompositeAuthDetails, OperationWithForm }
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.controllers.GformSessionKeys.COMPOSITE_AUTH_DETAILS_SESSION_KEY
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.http.BadRequestException
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class AcknowledgementController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  acknowledgementPdfService: AcknowledgementPdfService,
  renderer: SectionRenderingService,
  messagesControllerComponents: MessagesControllerComponents
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

  def downloadPDF(maybeAccessCode: Option[AccessCode], formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.WithAcknowledgement](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.ViewAcknowledgement
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      val pdfContentF =
        acknowledgementPdfService.createPDFContent(cache, maybeAccessCode, formModelOptics, sendAuditEvent = true)

      if (cache.formTemplate.accessiblePdf) {
        for {
          pdfContent <- pdfContentF
          pdfSource  <- acknowledgementPdfService.getByteArrayFop(pdfContent)
        } yield Ok(pdfSource).as(MimeConstants.MIME_PDF)
      } else {
        for {
          pdfContent <- pdfContentF
          pdfSource  <- acknowledgementPdfService.generatePDF(pdfContent)
        } yield Result(
          header = ResponseHeader(200, Map.empty),
          body = HttpEntity.Streamed(pdfSource, None, Some(MimeConstants.MIME_PDF))
        )
      }
    }

  def exitSurvey(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    Action.async { request =>
      Future.successful(Redirect(s"/feedback/${formTemplateId.value}").withNewSession)
    }

}
