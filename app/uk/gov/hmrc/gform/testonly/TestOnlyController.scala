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

package uk.gov.hmrc.gform.testonly

import akka.stream.scaladsl.Source
import akka.util.ByteString
import cats.data.EitherT
import cats.instances.future._
import com.typesafe.config.{ ConfigFactory, ConfigRenderOptions }
import play.api.i18n.{ I18nSupport, Messages }
import play.api.libs.json.JsValue
import play.api.libs.json.{ Json, OFormat }

import play.api.mvc._

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.ProxyActions
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.fileupload.Attachments
import uk.gov.hmrc.gform.gform.{ CustomerId, DestinationEvaluator, FrontEndSubmissionVariablesBuilder, StructuredFormDataBuilder }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.CustomerIdRecalculation
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, AffinityGroupUtil, LangADT, PdfHtml, SubmissionData }
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParametersRecalculated, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping

import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.gform.validation._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent

class TestOnlyController(
  i18nSupport: I18nSupport,
  proxy: ProxyActions,
  gformConnector: GformConnector,
  lookupRegistry: LookupRegistry,
  auth: AuthenticatedRequestActions,
  servicesConfig: ServicesConfig,
  controllerComponents: MessagesControllerComponents,
  validationService: ValidationService
)(implicit ec: ExecutionContext)
    extends FrontendController(controllerComponents: MessagesControllerComponents) {

  def proxyToGform(path: String): Action[Source[ByteString, _]] = proxy(gformBaseUrl)(path)

  def proxyToFileUpload(path: String): Action[Source[ByteString, _]] = proxy(fileUploadBaseUrl)(path)

  def proxyToSave4Later(path: String): Action[Source[ByteString, _]] = proxy(save4Later)(path)

  def whatsInSession(): Action[AnyContent] = Action { implicit request =>
    Ok(Json.toJson(request.session.data))
  }

  def clearSession(): Action[AnyContent] = Action { request =>
    Ok("session cleared").withSession()
  }

  def config() = Action { r =>
    val result: JsValue = Json.parse(ConfigFactory.load().root().render(ConfigRenderOptions.concise()))
    Ok(result)
  }

  private lazy val gformBaseUrl = servicesConfig.baseUrl("gform")
  private lazy val fileUploadBaseUrl = servicesConfig.baseUrl("file-upload")
  private lazy val save4Later = servicesConfig.baseUrl("save4later")

  private def recov[A](f: Future[A])(errorMsg: String)(implicit ec: ExecutionContext): FOpt[A] =
    fromFutureOptA(f.map(Right.apply).recover { case e => Left(UnexpectedState(errorMsg + "\n" + e.getMessage)) })

  def handlebarModel(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      implicit request => implicit lang => cache => _ => formModelOptics =>
        import i18nSupport._
        import cache._
        val customerId =
          CustomerIdRecalculation
            .evaluateCustomerId[DataOrigin.Mongo, SectionSelectorType.WithAcknowledgement](
              cache,
              formModelOptics.formModelVisibilityOptics
            )

        withHandlebarPayload {
          for {
            res <-
              fetchHandlebarModel(form, formTemplate, formModelOptics.formModelVisibilityOptics, customerId, retrievals)
          } yield res
        }
    }

  private def fetchHandlebarModel(
    form: Form,
    formTemplate: FormTemplate,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    customerId: CustomerId,
    retrievals: MaterialisedRetrievals
  )(implicit
    l: LangADT,
    m: Messages,
    hc: HeaderCarrier
  ): EitherT[Future, UnexpectedState, Result] = {

    val emailParameters = EmailParametersRecalculated(Map.empty)
    val affinityGroup = AffinityGroupUtil.fromRetrievals(retrievals)

    for {
      structuredFormData <- fromFutureA(
                              StructuredFormDataBuilder[DataOrigin.Mongo, Future](
                                formModelVisibilityOptics,
                                formTemplate.destinations,
                                formTemplate.expressionsOutput,
                                lookupRegistry
                              )
                            )

      submissionData = SubmissionData(
                         PdfHtml("htmlForPDF"),
                         None,
                         FrontEndSubmissionVariablesBuilder(
                           retrievals,
                           formTemplate,
                           formModelVisibilityOptics,
                           customerId
                         ),
                         structuredFormData,
                         emailParameters,
                         Attachments.empty,
                         l,
                         None,
                         DestinationEvaluator(formTemplate, formModelVisibilityOptics)
                       )

      httpResponse <- recov(
                        gformConnector
                          .renderHandlebarModel(
                            formTemplate._id,
                            form._id,
                            customerId,
                            submissionData,
                            affinityGroup
                          )
                      )("Error when calling gform service.")

    } yield Ok(httpResponse.body)

  }

  def handlebarPayload(
    formTemplateId: FormTemplateId,
    destinationId: DestinationId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      implicit request => implicit lang => cache => _ => formModelOptics =>
        import i18nSupport._
        import cache._
        val customerId =
          CustomerIdRecalculation
            .evaluateCustomerId[DataOrigin.Mongo, SectionSelectorType.WithAcknowledgement](
              cache,
              formModelOptics.formModelVisibilityOptics
            )

        withHandlebarPayload {
          for {
            res <- fetchHandlebarPayload(
                     form,
                     formTemplate,
                     formModelOptics.formModelVisibilityOptics,
                     customerId,
                     destinationId,
                     retrievals
                   )
          } yield res
        }
    }

  private def withHandlebarPayload(
    eitherT: EitherT[Future, UnexpectedState, Result]
  ): Future[Result] =
    eitherT.fold(error => BadRequest(error.error), identity)

  private def fetchHandlebarPayload(
    form: Form,
    formTemplate: FormTemplate,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    customerId: CustomerId,
    destinationId: DestinationId,
    retrievals: MaterialisedRetrievals
  )(implicit
    l: LangADT,
    m: Messages,
    hc: HeaderCarrier
  ): EitherT[Future, UnexpectedState, Result] = {

    val emailParameters = EmailParametersRecalculated(Map.empty)
    val affinityGroup = AffinityGroupUtil.fromRetrievals(retrievals)

    for {
      structuredFormData <- fromFutureA(
                              StructuredFormDataBuilder[DataOrigin.Mongo, Future](
                                formModelVisibilityOptics,
                                formTemplate.destinations,
                                formTemplate.expressionsOutput,
                                lookupRegistry
                              )
                            )

      submissionData = SubmissionData(
                         PdfHtml("htmlForPDF"),
                         None,
                         FrontEndSubmissionVariablesBuilder(
                           retrievals,
                           formTemplate,
                           formModelVisibilityOptics,
                           customerId
                         ),
                         structuredFormData,
                         emailParameters,
                         Attachments.empty,
                         l,
                         None,
                         DestinationEvaluator(formTemplate, formModelVisibilityOptics)
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
                          )
                      )("Error when calling gform service.")

    } yield Ok(httpResponse.body)

  }

  def errorMessages(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        import play.api.i18n._

        val messagesApi: MessagesApi = controllerComponents.messagesApi
        val englishMessages: Messages = messagesApi.preferred(Seq(Lang("en")))
        val welshMessages: Messages = messagesApi.preferred(Seq(Lang("cy")))
        for {
          envelopeWithMapping <- Future.successful(EnvelopeWithMapping.empty)
          englishValidationResult <-
            validationService
              .validateFormModel(
                cache.toCacheData,
                envelopeWithMapping,
                formModelOptics.formModelVisibilityOptics,
                None
              )(implicitly[HeaderCarrier], englishMessages, l, sse)
          welshValidationResult <-
            validationService
              .validateFormModel(
                cache.toCacheData,
                envelopeWithMapping,
                formModelOptics.formModelVisibilityOptics,
                None
              )(implicitly[HeaderCarrier], welshMessages, l, sse)
          englishReport = reports(englishValidationResult.formFieldValidationResults)(LangADT.En)
          welshReport = reports(welshValidationResult.formFieldValidationResults)(LangADT.Cy)
        } yield Ok(Json.toJson(englishReport ++ welshReport))
    }

  private def reports(
    listValidation: List[FormFieldValidationResult]
  )(implicit l: LangADT): List[FieldErrorReport] =
    listValidation
      .filter(_.isNotOk)
      .flatMap { formFieldValidationResults =>
        val formComponent = formFieldValidationResults.formComponent
        formFieldValidationResults match {
          case ComponentField(_, data) =>
            data.map { case (key, value) =>
              FieldErrorReport.make(key.toHtmlId, formComponent, value.fieldErrors.toList)
            }
          case otherwise =>
            List(FieldErrorReport.make(formComponent.id.value, formComponent, otherwise.fieldErrors.toList))
        }
      }

  case class FieldErrorReport(
    fieldId: String,
    errorMessageType: String,
    label: String,
    shortName: String,
    errorShortName: String,
    errorShortNameStart: String,
    errorExample: String,
    errorMessage: String,
    messages: List[String]
  )

  object FieldErrorReport {
    implicit val format: OFormat[FieldErrorReport] = Json.format[FieldErrorReport]
    def make(fieldId: String, formComponent: FormComponent, messages: List[String])(implicit l: LangADT) =
      FieldErrorReport(
        fieldId = fieldId,
        errorMessageType = "Dynamic",
        label = formComponent.label.rawValue,
        shortName = formComponent.shortName.map(_.rawValue).getOrElse("undefined"),
        errorShortName = formComponent.errorShortName.map(_.rawValue).getOrElse("undefined"),
        errorShortNameStart = formComponent.errorShortNameStart.map(_.rawValue).getOrElse("undefined"),
        errorExample = formComponent.errorExample.map(_.rawValue).getOrElse("undefined"),
        errorMessage = formComponent.errorMessage.map(_.rawValue).getOrElse("undefined"),
        messages = messages
      )
  }

}
