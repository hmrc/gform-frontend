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

package uk.gov.hmrc.gform.testonly

import akka.stream.scaladsl.Source
import akka.util.ByteString
import cats.MonadError
import cats.data.EitherT
import cats.instances.future._
import com.typesafe.config.{ ConfigFactory, ConfigRenderOptions }
import play.api.libs.json.JsValue
import play.api.{ Configuration, Mode }
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.ProxyActions
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.fileupload.Attachments
import uk.gov.hmrc.gform.gform.{ CustomerId, FrontEndSubmissionVariablesBuilder, StructuredFormDataBuilder }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.CustomerIdRecalculation
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.{ FormModel, FormModelBuilder, SectionSelectorType }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, AffinityGroupUtil, LangADT, PdfHtml, SourceOrigin, SubmissionData }
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParametersRecalculated, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.play.config.ServicesConfig

class TestOnlyController(
  proxy: ProxyActions,
  gformConnector: GformConnector,
  lookupRegistry: LookupRegistry,
  auth: AuthenticatedRequestActions,
  controllerComponents: MessagesControllerComponents,
  override protected val mode: Mode,
  override protected val runModeConfiguration: Configuration
)(implicit ec: ExecutionContext)
    extends FrontendController(controllerComponents: MessagesControllerComponents) with ServicesConfig {

  def proxyToGform(path: String): Action[Source[ByteString, _]] = proxy(gformBaseUrl)(path)

  def proxyToFileUpload(path: String): Action[Source[ByteString, _]] = proxy(fileUploadBaseUrl)(path)

  def proxyToSave4Later(path: String): Action[Source[ByteString, _]] = proxy(save4Later)(path)

  def whatsInSession(): Action[AnyContent] = Action { implicit request =>
    Ok(Json.toJson(request.session.data))
  }

  def clearSession(): Action[AnyContent] = Action { implicit request =>
    Ok("session cleared").withSession()
  }

  def config() = Action { r =>
    val result: JsValue = Json.parse(ConfigFactory.load().root().render(ConfigRenderOptions.concise()))
    Ok(result)
  }

  def getEnvelopeId(formId: FormId) = Action.async { implicit request =>
    gformConnector.getForm(formId).map(form => Ok(form.envelopeId.value))
  }

  private lazy val gformBaseUrl = baseUrl("gform")
  private lazy val fileUploadBaseUrl = baseUrl("file-upload")
  private lazy val save4Later = baseUrl("save4later")

  private def recov[A](f: Future[A])(errorMsg: String)(implicit ec: ExecutionContext): FOpt[A] =
    fromFutureOptA(f.map(Right.apply).recover { case e => Left(UnexpectedState(errorMsg + "\n" + e.getMessage)) })

  def handlebarModel(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      implicit request => implicit lang => cache => _ => formModelOptics =>
        import cache._
        implicit val data = cache.variadicFormData[SectionSelectorType.WithAcknowledgement]
        val customerId =
          CustomerIdRecalculation
            .evaluateCustomerId[DataOrigin.Mongo, SectionSelectorType.WithAcknowledgement](
              cache,
              formModelOptics.formModelVisibilityOptics)

        withHandlebarPayload {
          for {
            res <- fetchHandlebarModel(
                    form,
                    formTemplate,
                    formModelOptics.formModelVisibilityOptics,
                    customerId,
                    retrievals)
          } yield res
        }
    }

  private def fetchHandlebarModel(
    form: Form,
    formTemplate: FormTemplate,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    customerId: CustomerId,
    retrievals: MaterialisedRetrievals
  )(
    implicit
    l: LangADT,
    me: MonadError[Future, Throwable],
    hc: HeaderCarrier
  ): EitherT[Future, UnexpectedState, Result] = {

    val emailParameters = EmailParametersRecalculated(Map.empty)
    val affinityGroup = AffinityGroupUtil.fromRetrievals(retrievals)

    for {
      structuredFormData <- fromFutureA(
                             StructuredFormDataBuilder[DataOrigin.Mongo, Future](
                               formModelVisibilityOptics,
                               formTemplate.destinations,
                               lookupRegistry))

      submissionData = SubmissionData(
        PdfHtml("htmlForPDF"),
        FrontEndSubmissionVariablesBuilder(retrievals, formTemplate, formModelVisibilityOptics, customerId),
        structuredFormData,
        emailParameters,
        Attachments.empty
      )

      httpResponse <- recov(
                       gformConnector
                         .renderHandlebarModel(
                           formTemplate._id,
                           form._id,
                           customerId,
                           submissionData,
                           affinityGroup
                         ))("Error when calling gform service.")

    } yield Ok(httpResponse.body)

  }

  def handlebarPayload(
    formTemplateId: FormTemplateId,
    destinationId: DestinationId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      implicit request => implicit lang => cache => _ => formModelOptics =>
        import cache._
        implicit val data = cache.variadicFormData[SectionSelectorType.WithAcknowledgement]
        val customerId =
          CustomerIdRecalculation
            .evaluateCustomerId[DataOrigin.Mongo, SectionSelectorType.WithAcknowledgement](
              cache,
              formModelOptics.formModelVisibilityOptics)

        withHandlebarPayload {
          for {
            res <- fetchHandlebarPayload(
                    form,
                    formTemplate,
                    formModelOptics.formModelVisibilityOptics,
                    customerId,
                    destinationId,
                    retrievals)
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
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    customerId: CustomerId,
    destinationId: DestinationId,
    retrievals: MaterialisedRetrievals
  )(
    implicit
    l: LangADT,
    me: MonadError[Future, Throwable],
    hc: HeaderCarrier
  ): EitherT[Future, UnexpectedState, Result] = {

    val emailParameters = EmailParametersRecalculated(Map.empty)
    val affinityGroup = AffinityGroupUtil.fromRetrievals(retrievals)

    for {
      structuredFormData <- fromFutureA(
                             StructuredFormDataBuilder[DataOrigin.Mongo, Future](
                               formModelVisibilityOptics,
                               formTemplate.destinations,
                               lookupRegistry))

      submissionData = SubmissionData(
        PdfHtml("htmlForPDF"),
        FrontEndSubmissionVariablesBuilder(retrievals, formTemplate, formModelVisibilityOptics, customerId),
        structuredFormData,
        emailParameters,
        Attachments.empty
      )

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
}
