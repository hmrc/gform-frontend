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
import cats.implicits._
import cats.instances.future._
import com.typesafe.config.{ ConfigFactory, ConfigRenderOptions }
import play.api.i18n.{ I18nSupport, Messages }
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.mvc._
import play.twirl.api.HtmlFormat

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.ProxyActions
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.fileupload.Attachments
import uk.gov.hmrc.gform.gform.{ CustomerId, DestinationEvaluator, FrontEndSubmissionVariablesBuilder, StructuredFormDataBuilder, UserSessionBuilder }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.CustomerIdRecalculation
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.{ SectionSelectorType, UserSession }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, AffinityGroupUtil, LangADT, PdfHtml, SubmissionData }
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParametersRecalculated, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId }
import uk.gov.hmrc.govukfrontend.views.html.components.GovukTable
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.{ HtmlContent, Text }
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.{ HeadCell, Table, TableRow }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.gform.views.html.hardcoded.pages.destinations

class TestOnlyController(
  i18nSupport: I18nSupport,
  proxy: ProxyActions,
  gformConnector: GformConnector,
  lookupRegistry: LookupRegistry,
  auth: AuthenticatedRequestActions,
  servicesConfig: ServicesConfig,
  frontendAppConfig: FrontendAppConfig,
  controllerComponents: MessagesControllerComponents
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
                         DestinationEvaluator(formTemplate, formModelVisibilityOptics),
                         UserSession.empty
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

  def handlebarPayloads(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    implicit request => implicit lang => cache => _ => formModelOptics =>
      import i18nSupport._
      val res: Result = cache.formTemplate.destinations.fold { destinationList =>
        val ids: List[(DestinationId, String, Boolean)] = destinationList.destinations.collect {
          case d: Destination.DataStore         => (d.id, "hmrcIlluminate", d.convertSingleQuotes.getOrElse(false))
          case d: Destination.HandlebarsHttpApi => (d.id, "handlebarsHttpApi", d.convertSingleQuotes.getOrElse(false))
        }

        val rows: List[List[TableRow]] = ids.map { case (destinationId, destinationType, convertSingleQuotes) =>
          val processPayloadLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
            destinationId.id,
            uk.gov.hmrc.gform.testonly.routes.TestOnlyController
              .handlebarPayload(formTemplateId, destinationId, maybeAccessCode)
          )
          val embeddedLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
            "Embedded",
            uk.gov.hmrc.gform.testonly.routes.TestOnlyController
              .handlebarPayloadEmbedded(formTemplateId, destinationId, maybeAccessCode)
          )
          val sourceLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
            "Source",
            uk.gov.hmrc.gform.testonly.routes.TestOnlyController
              .handlebarPayloadSource(formTemplateId, destinationId, maybeAccessCode)
          )
          List(
            TableRow(
              content = HtmlContent(processPayloadLink)
            ),
            TableRow(
              content = Text(destinationType)
            ),
            TableRow(
              content = HtmlContent(embeddedLink)
            ),
            TableRow(
              content = HtmlContent(sourceLink)
            ),
            TableRow(
              content = Text(convertSingleQuotes.toString)
            )
          )
        }

        val head =
          Seq(
            HeadCell(
              content = Text("Output json")
            ),
            HeadCell(
              content = Text("Destination type")
            ),
            HeadCell(
              content = Text("Embedded")
            ),
            HeadCell(
              content = Text("Source")
            ),
            HeadCell(
              content = Text("convertSingleQuotes")
            )
          )

        val govukTable = new GovukTable()(
          Table(
            rows = rows,
            head = Some(head)
          )
        )

        Ok(destinations(cache.formTemplate, maybeAccessCode, true, Option.empty[String], frontendAppConfig, govukTable))
      }(_ =>
        Ok(
          destinations(
            cache.formTemplate,
            maybeAccessCode,
            false,
            Some("Print destination has no payload"),
            frontendAppConfig,
            HtmlFormat.empty
          )
        )
      )

      res.pure[Future]

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
            userSession <- fromFutureA(UserSessionBuilder[Future](cache))
            res <- fetchHandlebarPayload(
                     form,
                     formTemplate,
                     formModelOptics.formModelVisibilityOptics,
                     customerId,
                     destinationId,
                     retrievals,
                     userSession
                   )
          } yield res
        }
    }

  def handlebarPayloadEmbedded(
    formTemplateId: FormTemplateId,
    destinationId: DestinationId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      request => lang => cache => _ => formModelOptics =>
        val res: Result = cache.formTemplate.destinations.fold { destinationList =>
          val ids: Option[Option[String]] = destinationList.destinations.collectFirst {
            case d: Destination.DataStore if d.id === destinationId         => d.payload
            case d: Destination.HandlebarsHttpApi if d.id === destinationId => d.payload
          }
          ids.flatten match {
            case None          => BadRequest(s"No payload found on destination $destinationId")
            case Some(payload) => Ok(payload)
          }
        }(_ => BadRequest("No destination with payload found"))

        res.pure[Future]
    }

  def handlebarPayloadSource(
    formTemplateId: FormTemplateId,
    destinationId: DestinationId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      implicit request => lang => cache => _ => formModelOptics =>
        gformConnector.handlebarPayloadSource(formTemplateId, destinationId).map(Ok(_))
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
    retrievals: MaterialisedRetrievals,
    userSession: UserSession
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
                         DestinationEvaluator(formTemplate, formModelVisibilityOptics),
                         userSession
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
}
