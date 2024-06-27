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
import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.util.ByteString
import cats.data.EitherT
import cats.data.Validated.Valid
import cats.implicits._
import com.typesafe.config.{ ConfigFactory, ConfigRenderOptions }
import play.api.i18n.{ I18nSupport, Messages }
import play.api.libs.json.{ JsObject, JsValue }
import play.api.libs.json.Json
import play.api.mvc._
import play.twirl.api.{ Html, HtmlFormat }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.auth.core.retrieve.v2._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.gform.auth.models.{ MaterialisedRetrievals, OperationWithForm, OperationWithoutForm }
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.ProxyActions
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.objectStore.Attachments
import uk.gov.hmrc.gform.gform.{ AcknowledgementController, CustomerId, DestinationEvaluator, FrontEndSubmissionVariablesBuilder, NewFormController, StructuredFormDataBuilder, SummaryController, UserSessionBuilder }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.CustomerIdRecalculation
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.{ SectionSelectorType, UserSession }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, AffinityGroupUtil, LangADT, PdfHtml, SubmissionData }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormIdData, InProgress, UserData }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.SdesDestination.{ DataStore, DataStoreLegacy, Dms, HmrcIlluminate }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParametersRecalculated, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, SdesDestination }
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.{ HtmlContent, Text }
import uk.gov.hmrc.govukfrontend.views.viewmodels.hint.Hint
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.{ HeadCell, Table, TableRow }
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.gform.views.html.formatInstant
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import uk.gov.hmrc.gform.views.html.hardcoded.pages.{ br, save_form_page, snapshot_delete_acknowledgement, snapshot_delete_confirmation, snapshot_page, snapshot_restore_options, snapshots_page, update_snapshot }
import uk.gov.hmrc.gform.views.html.debug.snippets.inputWrapper
import uk.gov.hmrc.gform.views.html.debug.toolbox
import uk.gov.hmrc.gform.BuildInfo
import snapshot._
import SnapshotForms._
import uk.gov.hmrc.play.bootstrap.binders.{ OnlyRelative, RedirectUrl }
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl.idFunctor
import uk.gov.hmrc.gform.views.html.summary.snippets.bulleted_list
import uk.gov.hmrc.govukfrontend.views.viewmodels.content

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
  newFormController: NewFormController,
  authLoginStubService: AuthLoginStubService,
  summaryController: SummaryController,
  acknowledgementController: AcknowledgementController
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
            userSession <- fromFutureA(UserSessionBuilder[Future](cache))
            res <-
              fetchHandlebarModel(
                form,
                formTemplate,
                formModelOptics.formModelVisibilityOptics,
                customerId,
                retrievals,
                userSession
              )
          } yield res
        }
    }

  private def fetchHandlebarModel(
    form: Form,
    formTemplate: FormTemplate,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    customerId: CustomerId,
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

  private def formTab(formTemplateId: FormTemplateId, accessCode: Option[AccessCode]): HtmlFormat.Appendable = {
    val returnToSummaryLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "Return to summary page",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.changeStateAndRedirectToCYA(formTemplateId, accessCode)
    )

    val viewFormDataLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View form data",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.getFormData(formTemplateId, accessCode)
    )

    val viewSourceJsonTemplateLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View source json template",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.proxyToGform("gform/formtemplates/" + formTemplateId.value)
    )

    val saveCurrentFormLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "Save current form",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.saveFormPage(formTemplateId, accessCode)
    )

    val restoreFormLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "Restore a form",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.getSnapshots(formTemplateId, accessCode, UserInputs())
    )

    val links =
      List(returnToSummaryLink, viewFormDataLink, viewSourceJsonTemplateLink, saveCurrentFormLink, restoreFormLink)

    bulleted_list(links)
  }

  private def submittedDataTab(formTemplate: FormTemplate, accessCode: Option[AccessCode], envelopeId: EnvelopeId) = {

    def createDownloadContent(destination: SdesDestination) =
      uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
        s"Download files for ${destination.description}",
        uk.gov.hmrc.gform.testonly.routes.TestOnlyController
          .proxyToGform(s"gform/test-only/object-store/${destination.downloadPath}/envelopes/${envelopeId.value}")
      )

    val downloadContents = List(Dms, DataStore, DataStoreLegacy, HmrcIlluminate).map(createDownloadContent)

    val dataStoreWorkItemLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View data-store-work-item entry",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController
        .proxyToGform("gform/data-store-work-item/envelopeId/" + envelopeId.value)
    )

    val dmsWorkItemLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View dms-work-item entry",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController
        .proxyToGform("gform/dms-work-item/envelopeId/" + envelopeId.value)
    )

    val viewSDESLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View sdes submission",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController
        .proxyToGform("gform/sdes/envelopeId/" + envelopeId.value)
    )

    val links = downloadContents ++ List(dataStoreWorkItemLink, dmsWorkItemLink, viewSDESLink)
    bulleted_list(links)
  }

  private def developmentToolsTab(
    formTemplate: FormTemplate,
    accessCode: Option[AccessCode],
    envelopeId: EnvelopeId
  ) = {

    val envelope = inputWrapper("Envelope ID", envelopeId.value)

    val govukTable = formTemplate.destinations.fold { destinationList =>
      val ids: List[(DestinationId, String, Boolean)] = destinationList.destinations.collect {
        case d: Destination.DataStore         => (d.id, "hmrcIlluminate", d.convertSingleQuotes.getOrElse(false))
        case d: Destination.HandlebarsHttpApi => (d.id, "handlebarsHttpApi", d.convertSingleQuotes.getOrElse(false))
        case d: Destination.HmrcDms           => (d.id, "hmrcDms", d.convertSingleQuotes.getOrElse(false))
      }

      val rows: List[List[TableRow]] = ids.map { case (destinationId, destinationType, convertSingleQuotes) =>
        val processPayloadLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
          destinationId.id,
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController
            .handlebarPayload(formTemplate._id, destinationId, accessCode)
        )
        val embeddedLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
          "Embedded",
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController
            .handlebarPayloadEmbedded(formTemplate._id, destinationId, accessCode)
        )
        val sourceLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
          "Source",
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController
            .handlebarPayloadSource(formTemplate._id, destinationId, accessCode)
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

      new GovukTable()(
        Table(
          caption = Some("Destinations with handlebar payload"),
          captionClasses = "govuk-table__caption--m",
          rows = rows,
          head = Some(head)
        )
      )
    }(_ => HtmlFormat.empty)

    val viewHandlebarModelLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View handlebar json model",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.handlebarModel(formTemplate._id, accessCode)
    )

    val viewSourceTemplateLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View source json template",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.proxyToGform("gform/formtemplates/" + formTemplate._id.value)
    )

    val viewInternalTemplateLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View internal json template ('convertSingleQuotes' has been applied to 'includeIf' etc.)",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController
        .proxyToGform("gform/formtemplates/" + formTemplate._id.value + "/internal")
    )

    val viewEnvelopeFilesLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View envelope files",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController.proxyToGform("gform/envelopes/" + envelopeId.value)
    )

    val viewUploadedFilesLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View the uploaded files from bucket",
      uk.gov.hmrc.gform.testonly.routes.ObjectStoreAdminController.objectStoreContent(envelopeId)
    )

    val viewTranslationLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View translation source",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyController
        .proxyToGform("gform/translation/" + formTemplate._id.value + "/debug")
    )

    val links = List(
      viewHandlebarModelLink,
      viewSourceTemplateLink,
      viewInternalTemplateLink,
      viewEnvelopeFilesLink,
      viewUploadedFilesLink,
      viewTranslationLink
    )

    val bulletedList = bulleted_list(links)

    val tableInset = new GovukInsetText()(
      InsetText(
        content = content.HtmlContent(
          HtmlFormat.fill(
            List(
              Html("Embedded - payload is embedded into json template itself ('convertSingleQuotes' has been applied)"),
              br(),
              Html("Source - original payload from *.hbs file")
            )
          )
        )
      )
    )

    HtmlFormat.fill(List(envelope, br(), bulletedList, br(), govukTable, tableInset))
  }

  private def reportsTab(formTemplate: FormTemplate, accessCode: Option[AccessCode]) = {
    val viewFormTemplateDetailsLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View form template details",
      uk.gov.hmrc.gform.testonly.routes.FormTemplateExtractController.extract(formTemplate._id, accessCode)
    )

    val viewErrorReport = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View error report",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyErrorMessageController.errorsHtml(formTemplate._id, accessCode)
    )

    val viewErrorUsageReport = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      "View error usage report",
      uk.gov.hmrc.gform.testonly.routes.TestOnlyErrorMessageController.errorsUsageHtml(formTemplate._id, accessCode)
    )

    val links = List(viewFormTemplateDetailsLink, viewErrorReport, viewErrorUsageReport)

    bulleted_list(links)
  }

  def handleToolbox(
    formTemplateId: FormTemplateId,
    accessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, accessCode) {
    implicit request => implicit lang => cache => _ => formModelOptics =>
      import i18nSupport._

      val govukTabs = new GovukTabs()(
        Tabs(
          items = Seq(
            TabItem(
              id = Some("form"),
              label = "Form",
              panel = TabPanel(
                content = HtmlContent(formTab(formTemplateId, accessCode))
              )
            ),
            TabItem(
              id = Some("submitted-data"),
              label = "Submitted data",
              panel = TabPanel(
                content = HtmlContent(submittedDataTab(cache.formTemplate, accessCode, cache.form.envelopeId))
              )
            ),
            TabItem(
              id = Some("reports"),
              label = "Reports",
              panel = TabPanel(
                content = HtmlContent(reportsTab(cache.formTemplate, accessCode))
              )
            ),
            TabItem(
              id = Some("development-tools"),
              label = "Development tools",
              panel = TabPanel(
                content = HtmlContent(developmentToolsTab(cache.formTemplate, accessCode, cache.form.envelopeId))
              )
            )
          )
        )
      )

      Ok(toolbox(cache.formTemplate, cache.form.envelopeId, accessCode, frontendAppConfig, govukTabs)).pure[Future]

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
            case h: Destination.HmrcDms if h.id === destinationId           => h.payload
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

  private val defaultRetrievals = Retrievals.credentials and
    Retrievals.allEnrolments and
    Retrievals.affinityGroup and
    Retrievals.groupIdentifier and
    Retrievals.nino and
    Retrievals.email and
    Retrievals.name and
    Retrievals.confidenceLevel and
    Retrievals.itmpName and
    Retrievals.itmpDateOfBirth and
    Retrievals.itmpAddress and
    Retrievals.credentialRole and
    Retrievals.credentialStrength and
    Retrievals.gatewayInformation and
    Retrievals.agentInformation

  def saveForm(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    Action.async { implicit request =>
      implicit val hc: HeaderCarrier =
        HeaderCarrierConverter.fromRequestAndSession(request, request.session)
      auth
        .authorised(AuthProviders(AuthProvider.GovernmentGateway))
        .retrieve(defaultRetrievals) {
          case maybeCredentials ~
              enrolments ~
              maybeAffinityGroup ~
              maybeGroupIdentifier ~
              maybeNino ~
              maybeEmail ~
              maybeName ~
              confidenceLevel ~
              itmpName ~
              itmpDateOfBirth ~
              itmpAddress ~
              maybeCredentialRole ~
              credentialStrength ~
              maybeGatewayInformation ~
              agentInformation =>
            val authWizardData = GovernmentGatewayFormData()
              .withCredentials(maybeCredentials)
              .withEnrolments(enrolments)
              .withAffinityGroup(maybeAffinityGroup)
              .withGroupIdentifier(maybeGroupIdentifier)
              .withNino(maybeNino)
              .withEmail(maybeEmail)
              .withName(maybeName)
              .withConfidenceLevel(confidenceLevel)
              .withItmpData(itmpName, itmpDateOfBirth, itmpAddress)
              .withAgent(agentInformation)
              .withCredentialRole(maybeCredentialRole)
              .withCredentialStrength(credentialStrength)
              .withGatewayToken(maybeGatewayInformation)
            Some(authWizardData).pure[Future]
        }
        .recover(_ => None)
        .flatMap { authWizardData =>
          saveFormUserData
            .bindFromRequest()
            .fold(
              formWithErrors => BadRequest("save form errors ${formWithErrors.errorsAsJson}").pure[Future],
              userData => {
                val currentFormId = FormId(userData.currentFormId)
                val description = userData.description
                val saveRequest =
                  SaveRequest(
                    currentFormId,
                    Description(description),
                    GformFrontendVersion(BuildInfo.version),
                    authWizardData
                  )
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
            )

        }
    }

  def updateSnapshot(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    implicit request => implicit lang => cache => _ => formModelOptics =>
      import i18nSupport._
      updateSnapshotUserData
        .bindFromRequest()
        .fold(
          formWithErrors =>
            BadRequest(
              update_snapshot(
                cache.formTemplate,
                maybeAccessCode,
                frontendAppConfig,
                SnapshotId(formWithErrors("snapshotId").value.getOrElse("")),
                Description(formWithErrors("description").value.getOrElse("")),
                formWithErrors("formData").value.getOrElse(""),
                formWithErrors
              )
            ).pure[Future],
          userData => {
            val updateRequest =
              UpdateSnapshotRequest(
                SnapshotId(userData.snapshotId),
                Json.parse(userData.formData).as[JsObject],
                Description(userData.description)
              )
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
        )
  }

  def updateFormData(
    formTemplateId: FormTemplateId,
    snapshotId: SnapshotId,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    implicit request => _ => cache => _ => formModelOptics =>
      restoreOptionUserData
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest("update form Data errors ${formWithErrors.errorsAsJson}").pure[Future],
          userData => {
            val currentFormId = cache.form._id
            if (userData.restoreType === restoreOptionCurrentSession) {
              val updateRequest = UpdateFormDataRequest(snapshotId, currentFormId)
              for {
                saveReply <- gformConnector.updateFormData(updateRequest)
              } yield Redirect(uk.gov.hmrc.gform.gform.routes.NewFormController.newOrContinue(formTemplateId))
            } else {
              for {
                overview <- gformConnector.snapshotOverview(snapshotId)
              } yield Redirect(
                uk.gov.hmrc.gform.testonly.routes.TestOnlyController.restoreAllToOriginal(snapshotId, maybeAccessCode)
              )
            }
          }
        )

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

  def restoreAllToOriginal(snapshotId: SnapshotId, maybeAccessCode: Option[AccessCode]) =
    controllerComponents.actionBuilder.async { implicit request =>
      doRestoreAll(snapshotId, maybeAccessCode, useOriginalTemplate = true)
    }

  def restoreAll(snapshotId: SnapshotId, maybeAccessCode: Option[AccessCode]) =
    controllerComponents.actionBuilder.async { implicit request =>
      doRestoreAll(snapshotId, maybeAccessCode, useOriginalTemplate = false)
    }

  private def doRestoreAll(snapshotId: SnapshotId, maybeAccessCode: Option[AccessCode], useOriginalTemplate: Boolean)(
    implicit request: Request[AnyContent]
  ) =
    for {
      s <- gformConnector.snapshotOverview(snapshotId)
      redirectUrl = uk.gov.hmrc.gform.testonly.routes.TestOnlyController
                      .restoreAllGet(
                        if (useOriginalTemplate) s.originalTemplateId else s.templateId,
                        maybeAccessCode,
                        snapshotId,
                        useOriginalTemplate
                      )
                      .url
      result <- s.ggFormData match {
                  case Some(ggFormData) =>
                    authLoginStubService
                      .getSession(ggFormData.withRedirectionUrl(redirectUrl))
                      .map(Redirect(redirectUrl).withSession)
                  case None => Redirect(redirectUrl).pure[Future]
                }
    } yield result

  def restoreAllGet(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    snapshotId: SnapshotId,
    useOriginalTemplate: Boolean
  ) =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.ShowAccessCode) {
      implicit request => implicit lang => cache =>
        val formTemplateContext = request.attrs(FormTemplateKey)
        for {
          _ <- if (useOriginalTemplate) {
                 ().pure[Future]
               } else gformConnector.restoreSnapshotTemplate(snapshotId.value)
          _ <- newFormController.continue(cache, formTemplateContext.formTemplate)
        } yield Redirect(
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController.restoreContinue(
            formTemplateId,
            snapshotId.value,
            useOriginalTemplate,
            maybeAccessCode
          )
        )
    }

  def restoreContinue(
    formTemplateId: FormTemplateId,
    snapshotId: String,
    useOriginalTemplate: Boolean,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    implicit request => _ => cache => _ => formModelOptics =>
      restore(snapshotId, cache.form._id.value, useOriginalTemplate)
  }

  private def restore(snapshotId: String, formId: String, useOriginalTemplate: Boolean)(implicit hc: HeaderCarrier) =
    for {
      snapshot <- gformConnector.restoreForm(snapshotId, formId, useOriginalTemplate)
      restoreTemplateId = if (useOriginalTemplate) snapshot.originalTemplateId else snapshot.templateId
    } yield Redirect(
      uk.gov.hmrc.gform.gform.routes.NewFormController.newOrContinue(restoreTemplateId)
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
          snapshotOverivew.formData.map(_.toString()).getOrElse(""),
          updateSnapshotUserData
        )
      )
  }

  def snapshotPage(
    formTemplateId: FormTemplateId,
    snapshotId: SnapshotId,
    maybeAccessCode: Option[AccessCode],
    currentFormId: Option[FormId],
    targetTemplateId: FormTemplateId
  ) =
    controllerComponents.actionBuilder.async { implicit request =>
      import i18nSupport._
      implicit val lang: LangADT = LangADT.En
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
        val shareUrl =
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController
            .restoreAll(snapshotId, maybeAccessCode)
            .path
        val shareUrlText = frontendAppConfig.gformFrontendBaseUrl + shareUrl
        val restoreOptionsUrl =
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController
            .selectRestoreOptions(snapshotId, formTemplateId, maybeAccessCode)
            .path
        Ok(
          snapshot_page(
            maybeAccessCode,
            frontendAppConfig,
            snapshotId,
            snapshotOverivew.description,
            versions,
            Json.prettyPrint(snapshotOverivew.formData.getOrElse(Json.obj())),
            isDataRestore,
            shareUrl,
            shareUrlText,
            restoreOptionsUrl,
            Json.prettyPrint(snapshotOverivew.ggFormData.map(Json.toJson(_)).getOrElse(Json.obj()))
          )
        )
      }
    }

  def getSnapshots(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    userInputs: UserInputs
  ) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
      implicit request => implicit lang => cache => _ => formModelOptics =>
        import i18nSupport._
        val currentFormId = cache.form._id
        val currentUrl = request.uri
        val validatedFrom = for {
          from <- userInputs.from
        } yield DateTimeValidator.validateUserInput("from", from)
        val validatedTo = for {
          to <- userInputs.to
        } yield DateTimeValidator.validateUserInput("to", to)
        val (from, to) = (validatedFrom, validatedTo) match {
          case (Some(Valid(from)), Some(Valid(to))) => (from, to)
          case (Some(Valid(from)), _)               => (from, None)
          case (_, Some(Valid(to)))                 => (None, to)
          case _                                    => (None, None)
        }
        val filter = SnapshotFilter(
          from,
          to,
          userInputs.snapshotIdFilter,
          userInputs.descriptionFilter,
          userInputs.templateIdFilter
        )
        for {
          snapshots <- gformConnector.getSnapshots(filter).map(_.sortBy(_.savedAt)(Ordering[Instant].reverse))
        } yield {
          val html = renderSnapshots(snapshots, currentFormId, formTemplateId.value, maybeAccessCode, currentUrl)

          val templateItems = SelectItem(
            value = userInputs.templateIdFilter,
            text = userInputs.templateIdFilter.getOrElse("")
          ) :: snapshots.map(_.originalTemplateId.value).distinct.sorted.map { v =>
            SelectItem(
              value = Some(v),
              text = v
            )
          }
          val govukLabel = new GovukLabel()
          val govukHint = new GovukHint()
          val govukErrorMessage = new GovukErrorMessage()
          val govukSelect = new GovukSelect(govukErrorMessage, govukHint, govukLabel)
          val select = govukSelect(
            Select(
              id = "templateIdFilter",
              name = "templateIdFilter",
              items = templateItems,
              label = Label(
                content = Text("Filter by template id")
              )
            )
          )
          Ok(
            snapshots_page(
              cache.formTemplate,
              maybeAccessCode,
              frontendAppConfig,
              html,
              cache.form._id.value,
              UserInputs.fromDateInput(userInputs, DateTimeValidator.mayBeErrors(validatedFrom)),
              UserInputs.toDateInput(userInputs, DateTimeValidator.mayBeErrors(validatedTo)),
              userInputs.snapshotIdFilter,
              userInputs.descriptionFilter,
              select
            )
          )
        }
    }

  def renderSnapshots(
    snapshots: List[SnapshotOverview],
    currentFormId: FormId,
    currentTemplateId: String,
    accessCode: Option[AccessCode],
    currentUrl: String
  ): Html = {

    val tableRows: List[List[TableRow]] = snapshots.map { snapshot =>
      List(
        TableRow(
          content = HtmlContent(Html(snapshot.originalTemplateId.value))
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
        ),
        TableRow(
          content = HtmlContent {
            val url = uk.gov.hmrc.gform.testonly.routes.TestOnlyController
              .snapshotDeleteConfirmation(snapshot.snapshotId, RedirectUrl(currentUrl))
              .url
            s"""<a class=govuk-link href='$url'>delete</a>"""
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

  def snapshotDeleteConfirmation(snapshotId: SnapshotId, backUrl: RedirectUrl) =
    controllerComponents.actionBuilder.async { implicit request =>
      import i18nSupport._
      implicit val lang: LangADT = LangADT.En
      val actionUrl = uk.gov.hmrc.gform.testonly.routes.TestOnlyController.deleteSnapshot(snapshotId).url
      Ok(
        snapshot_delete_confirmation(
          frontendAppConfig,
          snapshotId,
          actionUrl,
          backUrl.get(OnlyRelative).url
        )
      ).pure[Future]
    }

  def deleteSnapshot(snapshotId: SnapshotId) =
    controllerComponents.actionBuilder.async { implicit request =>
      import i18nSupport._
      implicit val lang: LangADT = LangADT.En
      deleteSnapshotUserData
        .bindFromRequest()
        .fold(
          formWithErrors => BadRequest("delete snapshot error: ${formWithErrors.errorsAsJson}").pure[Future],
          userData =>
            gformConnector
              .deleteSnapshot(userData.snapshotId)
              .map { _ =>
                Ok(snapshot_delete_acknowledgement(frontendAppConfig, snapshotId, userData.backUrl))
              }
        )
    }

  private val restoreOptionCurrentSession = "restoreOptionCurrentSession"
  private val restoreOptionOriginalTemplate = "restoreOptionOriginalTemplate"
  def selectRestoreOptions(snapshotId: SnapshotId, currentTemplateId: FormTemplateId, accessCode: Option[AccessCode]) =
    controllerComponents.actionBuilder.async { implicit request =>
      import i18nSupport._
      implicit val lang: LangADT = LangADT.En

      val govukErrorMessage: GovukErrorMessage = new GovukErrorMessage()
      val govukLabel: GovukLabel = new GovukLabel()
      val govukFieldset: GovukFieldset = new GovukFieldset()
      val govukHint: GovukHint = new GovukHint()
      val fieldset = Some(
        Fieldset(
          legend = Some(
            Legend(
              content = Text("Select restore options:"),
              isPageHeading = true,
              classes = "govuk-fieldset__legend--l"
            )
          )
        )
      )

      val option1 = RadioItem(
        value = Some(restoreOptionCurrentSession),
        content = Text(
          "Load restored data into current form"
        ),
        hint = Some(
          Hint(
            content = HtmlContent(
              "Load form data into current user session/form (sharing data between 2 different form templates/sessions)"
            )
          )
        ),
        checked = true
      )

      val option2 = RadioItem(
        value = Some(restoreOptionOriginalTemplate),
        content = Text("Close current form and load restored data in its original form"),
        hint = Some(
          Hint(content =
            HtmlContent("Load form where the snapshot was taken (recreate a new sessions with snapshot form and data)")
          )
        )
      )

      val radios = Radios(
        fieldset = fieldset,
        name = "restoreType",
        items = List(option1, option2)
      )

      val govukRadios = new GovukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)
      val actionUrl = uk.gov.hmrc.gform.testonly.routes.TestOnlyController
        .updateFormData(currentTemplateId, snapshotId, accessCode)
        .url
      Ok(snapshot_restore_options(frontendAppConfig, snapshotId, govukRadios, actionUrl)).pure[Future]
    }

  def filterSnapshotsPost(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    controllerComponents.actionBuilder.async { implicit request =>
      val answers: Map[String, String] = request.body.asFormUrlEncoded
        .map(_.collect {
          case (field, value :: _) if value.trim.nonEmpty =>
            (field, value.trim)
        })
        .getOrElse(Map.empty[String, String])

      val fromUserInputs = DateTimeUserInput("from", answers)
      val toUserInputs = DateTimeUserInput("to", answers)
      val templateIdFilter =
        if (answers.get("currentTemplateId").contains("on")) Some(formTemplateId.value)
        else answers.get("templateIdFilter")
      val userInputs =
        UserInputs(
          Some(fromUserInputs),
          Some(toUserInputs),
          answers.get("snapshotIdFilter"),
          answers.get("descFilter"),
          templateIdFilter
        )

      Future.successful(
        Redirect(
          uk.gov.hmrc.gform.testonly.routes.TestOnlyController.getSnapshots(
            formTemplateId,
            maybeAccessCode,
            userInputs
          )
        )
      )
    }

  def getFormData(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) = auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, maybeAccessCode) {
    _ => _ => cache => _ => formModelOptics =>
      Future.successful(
        Ok(Json.toJson(cache.form))
      )
  }

  def generateSummaryHtml(formTemplateId: FormTemplateId, accessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      accessCode,
      OperationWithForm.DownloadSummaryPdf
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      summaryController
        .createPDFHtml(cache, formModelOptics)
        .map { html =>
          Ok(html.html).as(HTML)
        }
    }

  def generateAcknowledgementHtml(formTemplateId: FormTemplateId, accessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.WithAcknowledgement](
      formTemplateId,
      accessCode,
      OperationWithForm.ViewAcknowledgement
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      acknowledgementController
        .createPDFHtml(cache, accessCode, formModelOptics, sendAuditEvent = false)
        .map { html =>
          Ok(html.html).as(HTML)
        }
    }

  def changeStateAndRedirectToCYA(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.ForceReturnToCYA
    ) { implicit request => _ => cache => _ => _ =>
      for {
        _ <- gformConnector.deleteGeneratedFiles(cache.form.envelopeId)
        res <-
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
                Redirect(
                  uk.gov.hmrc.gform.gform.routes.SummaryController
                    .summaryById(formTemplateId, maybeAccessCode, None, None, true, None)
                )
              )
            }
      } yield res
    }

  def fingerPrint(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.ForceReturnToCYA
    ) { _ => _ => cache => _ => _ =>
      Ok(cache.form.formData.fingerprint).pure[Future]
    }
}
