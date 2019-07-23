/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.testonly

import akka.stream.scaladsl.Source
import akka.util.ByteString
import cats.MonadError
import cats.data.EitherT
import cats.instances.future._
import play.api.Play
import play.api.libs.json.Json
import play.api.mvc._
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.InjectionDodge
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, MaterialisedRetrievals }
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.ProxyActions
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.gform.FrontEndSubmissionVariablesBuilder
import uk.gov.hmrc.gform.gform.{ CustomerId, GformSubmission, StructuredFormDataBuilder }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.CustomerIdRecalculation
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SubmissionData }
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParametersRecalculated, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.frontend.controller.FrontendController

class TestOnlyController(
  proxy: ProxyActions,
  gformConnector: GformConnector,
  lookupRegistry: LookupRegistry,
  auth: AuthenticatedRequestActions,
  customerIdRecalculation: CustomerIdRecalculation[Future]
) extends FrontendController with ServicesConfig {
  override protected def mode = InjectionDodge.mode
  override protected def runModeConfiguration = InjectionDodge.configuration

  def proxyToGform(path: String): Action[Source[ByteString, _]] = proxy(gformBaseUrl)(path)

  def proxyToFileUpload(path: String): Action[Source[ByteString, _]] = proxy(fileUploadBaseUrl)(path)

  def proxyToSave4Later(path: String): Action[Source[ByteString, _]] = proxy(save4Later)(path)

  def whatsInSession(): Action[AnyContent] = Action { implicit request =>
    Ok(Json.toJson(request.session.data))
  }

  def clearSession(): Action[AnyContent] = Action { implicit request =>
    Ok("session cleared").withSession()
  }

  def getEnvelopeId(formId: FormId) = Action.async { implicit request =>
    gformConnector.getForm(formId).map(form => Ok(form.envelopeId.value))
  }

  private lazy val gformBaseUrl = baseUrl("gform")
  private lazy val fileUploadBaseUrl = baseUrl("file-upload")
  private lazy val save4Later = baseUrl("save4later")

  private def recov[A](f: Future[A])(errorMsg: String)(implicit ec: ExecutionContext): FOpt[A] =
    fromFutureOptA(f.map(Right.apply).recover { case e => Left(UnexpectedState(errorMsg + "\n" + e.getMessage)) })

  def handlebarPayloadByFormId(
    formTemplateId: FormTemplateId,
    destinationId: DestinationId,
    formId: FormId): Action[AnyContent] =
    Action.async { implicit request =>
      implicit val l = LangADT.En
      val customerId = CustomerId("")
      val retrievals = AnonymousRetrievals(SessionId(""))

      withHandlebarPayload {
        for {
          form <- recov(gformConnector.getForm(formId))(s"No form id $formId found.")
          formTemplate <- recov(gformConnector.getFormTemplate(formTemplateId))(
                           s"No formTemplate id $formTemplateId found.")
          res <- fetchHandlebarPayload(form, formTemplate, customerId, destinationId, retrievals)
        } yield res
      }
    }

  def handlebarPayload(formTemplateId: FormTemplateId, destinationId: DestinationId) =
    auth.asyncWithMaybeAccessCode(formTemplateId, None) { implicit request => implicit lang => cache =>
      import cache._
      withHandlebarPayload {
        for {
          customerId <- fromFutureA(customerIdRecalculation.evaluateCustomerId(cache))
          res        <- fetchHandlebarPayload(form, formTemplate, customerId, destinationId, retrievals)
        } yield res
      }
    }

  private def withHandlebarPayload(
    eitherT: EitherT[Future, UnexpectedState, Result]
  )(
    implicit me: MonadError[Future, Throwable]
  ): Future[Result] =
    eitherT.fold(error => BadRequest(error.error), identity)

  private def fetchHandlebarPayload(
    form: Form,
    formTemplate: FormTemplate,
    customerId: CustomerId,
    destinationId: DestinationId,
    retrievals: MaterialisedRetrievals)(
    implicit l: LangADT,
    me: MonadError[Future, Throwable],
    hc: HeaderCarrier): EitherT[Future, UnexpectedState, Result] = {

    val emailParameters = EmailParametersRecalculated(Map.empty)
    val affinityGroup = AffinityGroupUtil.fromRetrievals(retrievals)

    for {
      structuredFormData <- fromFutureA(StructuredFormDataBuilder[Future](form, formTemplate, lookupRegistry))

      submissionData = SubmissionData(
        "htmlForPDF",
        FrontEndSubmissionVariablesBuilder(retrievals, formTemplate, customerId),
        structuredFormData,
        emailParameters)

      httpResponse <- recov(
                       gformConnector
                         .renderHandlebarPayload(
                           formTemplate._id,
                           form._id,
                           destinationId,
                           customerId,
                           submissionData,
                           affinityGroup
                         ))("Error when calling gform service.")

    } yield Ok(httpResponse.body)

  }

  def askFormId(formTemplateId: FormTemplateId) = auth.async(formTemplateId) {
    implicit request => implicit lang => cache =>
      import cache._

      val formId = FormId(retrievals, formTemplateId, None)
      Future.successful(Ok(formId.value))
  }
}
