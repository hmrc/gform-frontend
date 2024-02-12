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

import cats.implicits.catsSyntaxEq
import akka.stream.scaladsl.Source
import akka.util.ByteString
import cats.data.EitherT
import cats.implicits._
import com.typesafe.config.{ ConfigFactory, ConfigRenderOptions }
import play.api.i18n.{ I18nSupport, Messages }
import play.api.libs.json.{ JsObject, JsValue }
import play.api.libs.json.Json
import play.api.mvc._
import play.twirl.api.{ Html, HtmlFormat }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.ProxyActions
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.fileupload.Attachments
import uk.gov.hmrc.gform.gform.{ CustomerId, DestinationEvaluator, FrontEndSubmissionVariablesBuilder, NewFormController, StructuredFormDataBuilder, UserSessionBuilder }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.CustomerIdRecalculation
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.{ SectionSelectorType, UserSession }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, AffinityGroupUtil, LangADT, PdfHtml, SubmissionData }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.SdesDestination.{ DataStore, DataStoreLegacy, Dms, HmrcIlluminate }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParametersRecalculated, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, SdesDestination }
import uk.gov.hmrc.govukfrontend.views.html.components.GovukTable
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.{ HtmlContent, Text }
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.{ HeadCell, Table, TableRow }
import uk.gov.hmrc.gform.views.html.formatInstant
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.gform.views.html.hardcoded.pages.{ destinations, save_form_page, snapshot_page, snapshots_page, update_snapshot }
import uk.gov.hmrc.gform.auth.models.OperationWithoutForm
import uk.gov.hmrc.gform.BuildInfo

import java.time.Instant

class TestOnlyController(
  i18nSupport: I18nSupport,
  proxy: ProxyActions,
  gformConnector: GformConnector,
  lookupRegistry: LookupRegistry,
  auth: AuthenticatedRequestActions,
  servicesConfig: ServicesConfig,
  frontendAppConfig: FrontendAppConfig,
  controllerComponents: MessagesControllerComponents,
  newFormController: NewFormController
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

        val maybeReturnToSummaryContent = Option(
          uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
            "Return to summary page",
            uk.gov.hmrc.gform.gform.routes.AcknowledgementController
              .changeStateAndRedirectToCYA(formTemplateId, maybeAccessCode)
          )
        )
          .filter(_ =>
            destinationList.destinations.size === 1 && destinationList.destinations.exists {
              case Destination.StateTransition(_, _, _, _) => true
              case _                                       => false
            } && cache.form.status === Submitted
          )

        val envelopeId = cache.form.envelopeId
        val isObjectStore = cache.formTemplate.isObjectStore

        def createDownloadContent(destination: SdesDestination): Option[HtmlFormat.Appendable] =
          Option(
            uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
              destination.description,
              uk.gov.hmrc.gform.testonly.routes.TestOnlyController
                .proxyToGform(s"gform/test-only/object-store/${destination.downloadPath}/envelopes/${envelopeId.value}")
            )
          ).filter(_ =>
            isObjectStore && destinationList.destinations.exists {
              case Destination.HmrcDms(_, _, _, _, _, _, _, _, _, _, _)              => destination === Dms
              case Destination.DataStore(_, dsType, _, _, _, _, _, _, _, _, _, _, _) => destination === dsType
              case _                                                                 => false
            } && cache.form.status === Submitted
          )

        val maybeDownloadContents =
          Seq(Dms, DataStore, DataStoreLegacy, HmrcIlluminate).flatMap(createDownloadContent) match {
            case nonEmptyContents if nonEmptyContents.nonEmpty => Some(nonEmptyContents)
            case _                                             => None
          }

        Ok(
          destinations(
            cache.formTemplate,
            cache.form.envelopeId,
            maybeAccessCode,
            true,
            Option.empty[String],
            frontendAppConfig,
            govukTable,
            maybeReturnToSummaryContent,
            maybeDownloadContents,
            cache.form._id.value
          )
        )
      }(_ =>
        Ok(
          destinations(
            cache.formTemplate,
            cache.form.envelopeId,
            maybeAccessCode,
            false,
            Some("Print destination has no payload"),
            frontendAppConfig,
            HtmlFormat.empty,
            None,
            None,
            cache.form._id.value
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

  def saveForm(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    implicit request => _ => cache => _ => formModelOptics =>
      // import i18nSupport._
      val currentFormId = cache.form._id
      val description = request.body.asFormUrlEncoded.get("description").head
      val saveRequest = SaveRequest(currentFormId, Description(description), GformFrontendVersion(BuildInfo.version))
      for {
        snapshotOverview <- gformConnector.saveForm(saveRequest)
      } yield Redirect(
        uk.gov.hmrc.gform.testonly.routes.TestOnlyController.snapshotPage(
          snapshotOverview.templateId,
          snapshotOverview.snapshotId,
          maybeAccessCode,
          None,
          snapshotOverview.templateId
        )
      )
  }

  def updateSnapshot(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    implicit request => _ => cache => _ => formModelOptics =>
      val snapshotId = request.body.asFormUrlEncoded.get("snapshotId").head
      val formData = request.body.asFormUrlEncoded.get("formData").head
      val description = request.body.asFormUrlEncoded.get("description").head
      val updateRequest =
        UpdateSnapshotRequest(SnapshotId(snapshotId), Json.parse(formData).as[JsObject], Description(description))
      for {
        snapshotOverview <- gformConnector.updateSnapshot(updateRequest)

      } yield Redirect(
        uk.gov.hmrc.gform.testonly.routes.TestOnlyController.snapshotPage(
          snapshotOverview.templateId,
          snapshotOverview.snapshotId,
          maybeAccessCode,
          None,
          snapshotOverview.templateId
        )
      )
  }

  def updateFormData(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    implicit request => _ => cache => _ => formModelOptics =>
      val formData = request.body.asFormUrlEncoded.get("formData").head
      val currentFormId = cache.form._id
      val updateRequest = UpdateFormDataRequest(currentFormId, Json.parse(formData).as[JsObject])
      for {
        saveReply <- gformConnector.updateFormData(updateRequest)
      } yield Redirect(uk.gov.hmrc.gform.gform.routes.NewFormController.newOrContinue(formTemplateId))

  }

  def saveFormPage(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    implicit request => implicit lang => cache => _ => formModelOptics =>
      import i18nSupport._
      Future.successful(
        Ok(save_form_page(cache.formTemplate, maybeAccessCode, frontendAppConfig, cache.form._id.value))
      )
  }

  def restoreAll(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]) =
    controllerComponents.actionBuilder.async { implicit request =>
      val maybeSnapshotId = request.body.asFormUrlEncoded.get("snapshotId").headOption
      // composite auth config redirects to the original url
      // so we can't use authWithoutRetrievingForm in a POST request here
      maybeSnapshotId match {
        case Some(snapshotId) =>
          Future.successful(
            Redirect(
              uk.gov.hmrc.gform.testonly.routes.TestOnlyController.restoreAllGet(
                formTemplateId,
                maybeAccessCode,
                SnapshotId(snapshotId)
              )
            )
          )
        case None => throw new IllegalArgumentException("snapshotId is required")
      }
    }

  def restoreAllGet(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode], snapshotId: SnapshotId) =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.ShowAccessCode) {
      implicit request => implicit lang => cache =>
        val formTemplateContext = request.attrs(FormTemplateKey)
        for {
          // create a brand new form with snapshot's template id
          _ <- gformConnector.restoreSnapshotTemplate(snapshotId.value)
          _ <- newFormController.continue(cache, formTemplateContext.formTemplate)
        } yield Redirect(
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController.restoreContinue(
            formTemplateId,
            snapshotId.value,
            maybeAccessCode
          )
        )
    }

  def restoreContinue(
    formTemplateId: FormTemplateId,
    snapshotId: String,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    implicit request => _ => cache => _ => formModelOptics =>
      restore(snapshotId, cache.form._id.value)
  }

  def restoreCurrentForm(formId: String) =
    Action.async { implicit request =>
      val savedId = request.body.asFormUrlEncoded.get("snapshotId").head
      restore(savedId, formId)
    }

  def restoreCurrent(snapshotId: String, formId: String) =
    Action.async { implicit request =>
      restore(snapshotId, formId)
    }
  private def restore(snapshotId: String, formId: String)(implicit hc: HeaderCarrier) =
    for {
      snapshot <- gformConnector.restoreForm(snapshotId, formId)
    } yield Redirect(
      uk.gov.hmrc.gform.gform.routes.NewFormController.newOrContinue(snapshot.templateId)
    )

  def updateSnapshotPage(
    formTemplateId: FormTemplateId,
    snapshotId: SnapshotId,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    implicit request => implicit lang => cache => _ => formModelOptics =>
      import i18nSupport._
      for {
        snapshotOverivew <- gformConnector.snapshotOverview(snapshotId)
      } yield Ok(
        update_snapshot(
          cache.formTemplate,
          maybeAccessCode,
          frontendAppConfig,
          snapshotId,
          snapshotOverivew.description,
          snapshotOverivew.formData.map(_.toString()).getOrElse("")
        )
      )
  }

  def snapshotPage(
    formTemplateId: FormTemplateId,
    snapshotId: SnapshotId,
    maybeAccessCode: Option[AccessCode],
    currentFormId: Option[FormId],
    targetTemplateId: FormTemplateId
  ) = auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.ShowAccessCode) {
    implicit request => implicit lang => cache =>
      import i18nSupport._
      for {
        snapshotOverivew <- gformConnector.snapshotOverview(snapshotId)
      } yield {
        val isDataRestore = currentFormId.isDefined
        val versions: Html = Html(
          s"""
             |gform: ${snapshotOverivew.gformVersion.value}</br>
             |gform-frontend: ${snapshotOverivew.gformFrontendVersion.value}
             |""".stripMargin
        )
        val actionUrl =
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController.restoreAll(targetTemplateId, maybeAccessCode).path
        Ok(
          snapshot_page(
            cache.formTemplate,
            maybeAccessCode,
            frontendAppConfig,
            snapshotId,
            snapshotOverivew.description,
            versions,
            Json.prettyPrint(snapshotOverivew.formData.getOrElse(Json.obj())),
            isDataRestore,
            actionUrl
          )
        )
      }
  }

  def getSnapshots(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      implicit request => implicit lang => cache => _ => formModelOptics =>
        import i18nSupport._
        val currentFormId = cache.form._id
        for {
          snapshots <- gformConnector.getSnapshots().map(_.sortBy(_.savedAt)(Ordering[Instant].reverse))
        } yield {
          val html = renderSnapshots(snapshots, currentFormId, formTemplateId.value, maybeAccessCode)
          Ok(
            snapshots_page(
              cache.formTemplate,
              maybeAccessCode,
              frontendAppConfig,
              html,
              cache.form._id.value
            )
          )
        }
    }

  def renderSnapshots(
    snapshots: List[SnapshotOverview],
    currentFormId: FormId,
    currentTemplateId: String,
    accessCode: Option[AccessCode]
  ): Html = {

    val tableRows: List[List[TableRow]] = snapshots.map { snapshot =>
      List(
        TableRow(
          content = HtmlContent(Html(snapshot.templateId.value))
        ),
        TableRow(
          content = HtmlContent(Html(snapshot.description.value)),
          attributes = Map("style" -> "word-break: break-word")
        ),
        TableRow(
          content = Text(formatInstant(snapshot.savedAt))
        ),
        TableRow(
          content = HtmlContent {
            val url = uk.gov.hmrc.gform.testonly.routes.TestOnlyController
              .snapshotPage(
                FormTemplateId(currentTemplateId),
                snapshot.snapshotId,
                accessCode,
                Some(currentFormId),
                snapshot.templateId
              )
              .url
            s"""<a class=govuk-link href='$url'>${snapshot.snapshotId.value}</a>"""
          }
        ),
        TableRow(
          content = HtmlContent {
            val url = uk.gov.hmrc.gform.testonly.routes.TestOnlyController
              .updateSnapshotPage(FormTemplateId(currentTemplateId), snapshot.snapshotId, accessCode)
              .url
            s"""<a class=govuk-link href='$url'>edit</a>"""
          }
        )
      )
    }

    val header: List[HeadCell] = List(
      HeadCell(
        content = Text("Template id")
      ),
      HeadCell(
        content = Text("Description")
      ),
      HeadCell(
        content = Text("Saved at")
      ),
      HeadCell(
        content = Text("Snapshot id (click to restore)")
      ),
      HeadCell(
        content = Text("")
      )
    )

    val table = Table(
      head = Some(header),
      rows = tableRows,
      firstCellIsHeader = true
    )

    new GovukTable()(table)
  }

}
