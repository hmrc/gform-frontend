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

import org.apache.pekko.actor.Scheduler
import cats.instances.future._
import play.api.BuiltInComponents
import play.api.i18n.Messages
import uk.gov.hmrc.gform.addresslookup.{ AddressLookupController, AddressLookupModule }
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.api.{ BankAccountInsightsAsyncConnector, CompanyInformationAsyncConnector, DelegatedAgentAuthAsyncConnector, NinoInsightsAsyncConnector }
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.auth.{ AgentEnrolmentController, AuthModule, ErrorController }
import uk.gov.hmrc.gform.bars.BankAccountReputationAsyncConnector
import uk.gov.hmrc.gform.payment.PaymentController
import uk.gov.hmrc.gform.capture.CaptureController
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.ControllersModule
import uk.gov.hmrc.gform.objectStore.{ ObjectStoreController, ObjectStoreModule }
import uk.gov.hmrc.gform.gform.handlers.{ FormControllerRequestHandler, FormValidator }
import uk.gov.hmrc.gform.gform.processor.FormProcessor
import uk.gov.hmrc.gform.gformbackend.{ GformBackEndService, GformBackendModule }
import uk.gov.hmrc.gform.graph.GraphModule
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.{ ProcessDataService, TaxPeriodStateChecker }
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.pdf.PDFRenderService
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule
import uk.gov.hmrc.gform.summary.SummaryRenderingService
import uk.gov.hmrc.gform.summarypdf.{ FopService, PdfGeneratorService }
import uk.gov.hmrc.gform.tasklist.{ TaskListController, TaskListModule }
import uk.gov.hmrc.gform.upscan.{ UpscanController, UpscanModule }
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.play.bootstrap.binders.AbsoluteWithHostnameFromAllowlist
import uk.gov.hmrc.play.language.LanguageUtils

import scala.jdk.CollectionConverters._
import scala.concurrent.{ ExecutionContext, Future }

class GformModule(
  akkaModule: AkkaModule,
  configModule: ConfigModule,
  wSHttpModule: WSHttpModule,
  controllersModule: ControllersModule,
  authModule: AuthModule,
  gformBackendModule: GformBackendModule,
  objectStoreModule: ObjectStoreModule,
  taskListModule: TaskListModule,
  upscanModule: UpscanModule,
  addressLookupModule: AddressLookupModule,
  validationModule: ValidationModule,
  auditingModule: AuditingModule,
  playBuiltInsModule: PlayBuiltInsModule,
  graphModule: GraphModule,
  lookupRegistry: LookupRegistry,
  englishMessages: Messages,
  builtInComponents: BuiltInComponents
)(implicit
  ec: ExecutionContext
) {

  private val sectionRenderingService: SectionRenderingService = new SectionRenderingService(
    configModule.frontendAppConfig,
    lookupRegistry
  )

  val enrolmentController = new EnrolmentController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    sectionRenderingService,
    validationModule.validationService,
    authModule.enrolmentService,
    graphModule.recalculation,
    authModule.taxEnrolmentsConnector,
    authModule.ggConnector,
    configModule.frontendAppConfig,
    controllersModule.messagesControllerComponents,
    graphModule.smartStringEvaluatorFactory,
    englishMessages
  )

  val taxPeriodStateChecker = new TaxPeriodStateChecker[Future, Throwable] {
    def error: Throwable = new Exception("Call to des to retrieve obligation-data has failed")
  }

  val processDataService: ProcessDataService[Future] =
    new ProcessDataService[Future](graphModule.recalculation, taxPeriodStateChecker)

  val formControllerRequestHandler = new FormControllerRequestHandler(new FormValidator())

  val fastForwardService: FastForwardService = new FastForwardService(
    objectStoreModule.objectStoreService,
    validationModule.validationService,
    gformBackendModule.gformConnector,
    processDataService,
    formControllerRequestHandler,
    graphModule.smartStringEvaluatorFactory
  )

  val emailAuthController: EmailAuthController = new EmailAuthController(
    playBuiltInsModule.i18nSupport,
    controllersModule.messagesControllerComponents,
    controllersModule.nonAuthenticatedRequestActions,
    configModule.frontendAppConfig,
    gformBackendModule.gformConnector
  )

  val objectStoreController = new ObjectStoreController(
    objectStoreModule.objectStoreService,
    controllersModule.authenticatedRequestActions,
    gformBackendModule.gformConnector,
    fastForwardService,
    playBuiltInsModule.i18nSupport,
    controllersModule.messagesControllerComponents,
    configModule.frontendAppConfig
  )

  val taskListController: TaskListController =
    new TaskListController(
      playBuiltInsModule.i18nSupport,
      controllersModule.authenticatedRequestActions,
      taskListModule.taskListRenderingService,
      objectStoreModule.objectStoreService,
      controllersModule.messagesControllerComponents,
      fastForwardService,
      sectionRenderingService
    )

  private val barsBasePath =
    configModule.serviceConfig.getString("microservice.services.bars.base-path")
  private val barsBaseUrl = s"${configModule.serviceConfig.baseUrl("bars")}$barsBasePath"

  val bankAccountReputationConnector =
    new BankAccountReputationAsyncConnector(wSHttpModule.auditableWSHttp, barsBaseUrl)

  private val chBasePath =
    configModule.serviceConfig.getString("microservice.services.companies-house-api-proxy.base-path")
  private val companyHouseBaseUrl = s"${configModule.serviceConfig.baseUrl("companies-house-api-proxy")}$chBasePath"

  val companyInformationConnector =
    new CompanyInformationAsyncConnector(wSHttpModule.auditableWSHttp, companyHouseBaseUrl)

  private val ninoInsightsBasePath =
    configModule.serviceConfig.getString("microservice.services.nino-insights.base-path")
  private val ninoInsightsUrl = s"${configModule.serviceConfig.baseUrl("nino-insights")}$ninoInsightsBasePath"
  private val authorizationToken = configModule.typesafeConfig.getString("internal-auth.token")

  val ninoInsightsConnector =
    new NinoInsightsAsyncConnector(wSHttpModule.auditableWSHttp, ninoInsightsUrl, authorizationToken)

  private val bankAccountInsightsBasePath =
    configModule.serviceConfig.getString("microservice.services.bank-account-insights.base-path")
  private val bankAccountInsightsUrl =
    s"${configModule.serviceConfig.baseUrl("bank-account-insights")}$bankAccountInsightsBasePath"

  val bankAccountInsightsConnector =
    new BankAccountInsightsAsyncConnector(wSHttpModule.auditableWSHttp, bankAccountInsightsUrl, authorizationToken)

  val agentAccessControlConnector =
    new DelegatedAgentAuthAsyncConnector(
      authModule.authConnector
    )

  val fileSystemConnector = new FileSystemConnector()

  val addToListProcessor = new FormProcessor(
    playBuiltInsModule.i18nSupport,
    processDataService,
    gformBackendModule.gformConnector,
    fileSystemConnector,
    validationModule.validationService,
    fastForwardService,
    graphModule.recalculation,
    objectStoreModule.objectStoreService,
    formControllerRequestHandler,
    bankAccountReputationConnector,
    companyInformationConnector,
    ninoInsightsConnector,
    addressLookupModule.addressLookupService,
    bankAccountInsightsConnector,
    agentAccessControlConnector,
    englishMessages
  )

  val confirmationService = new ConfirmationService(
    addToListProcessor
  )

  val formController: FormController = new FormController(
    configModule.appConfig,
    configModule.frontendAppConfig,
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    objectStoreModule.objectStoreService,
    upscanModule.upscanService,
    validationModule.validationService,
    sectionRenderingService,
    gformBackendModule.gformConnector,
    processDataService,
    formControllerRequestHandler,
    fastForwardService,
    graphModule.recalculation,
    addToListProcessor,
    confirmationService,
    controllersModule.messagesControllerComponents,
    auditingModule.auditService
  )

  val addToListController = new FormAddToListController(
    configModule.frontendAppConfig,
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    processDataService,
    gformBackendModule.gformConnector,
    addToListProcessor,
    controllersModule.messagesControllerComponents
  )

  val saveAcknowledgementController = new SaveAcknowledgementController(
    playBuiltInsModule.i18nSupport,
    configModule.frontendAppConfig,
    controllersModule.authenticatedRequestActions,
    controllersModule.messagesControllerComponents,
    gformBackendModule.gformConnector
  )

  val summaryRenderingService = new SummaryRenderingService(
    sectionRenderingService,
    playBuiltInsModule.i18nSupport,
    objectStoreModule.objectStoreService,
    validationModule.validationService,
    configModule.frontendAppConfig
  )

  val pdfGeneratorService = new PdfGeneratorService(configModule.environment)

  val pdfRenderService = new PDFRenderService(objectStoreModule.objectStoreService, validationModule.validationService)

  val gformBackEndService = new GformBackEndService(
    gformBackendModule.gformConnector,
    sectionRenderingService,
    pdfRenderService,
    lookupRegistry,
    graphModule.smartStringEvaluatorFactory,
    graphModule.recalculation
  )

  val nonRepudiationHelpers = new NonRepudiationHelpers(auditingModule)

  val submissionService = new SubmissionService(
    playBuiltInsModule.i18nSupport,
    gformBackEndService,
    nonRepudiationHelpers,
    auditingModule.auditService,
    objectStoreModule.objectStoreService
  )

  val fopService = new FopService(configModule.environment)

  val summaryController: SummaryController = new SummaryController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    objectStoreModule.objectStoreService,
    validationModule.validationService,
    fopService,
    pdfGeneratorService,
    pdfRenderService,
    gformBackendModule.gformConnector,
    configModule.frontendAppConfig,
    summaryRenderingService,
    submissionService,
    controllersModule.messagesControllerComponents,
    configModule.appConfig
  )

  val printSectionController: PrintSectionController = new PrintSectionController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    pdfGeneratorService,
    sectionRenderingService,
    summaryRenderingService,
    controllersModule.messagesControllerComponents
  )

  val acknowledgementPdfService: AcknowledgementPdfService = new AcknowledgementPdfService(
    playBuiltInsModule.i18nSupport,
    nonRepudiationHelpers,
    sectionRenderingService,
    auditingModule.auditService,
    gformBackendModule.gformConnector,
    pdfRenderService,
    pdfGeneratorService,
    fopService
  )

  val acknowledgementController: AcknowledgementController = new AcknowledgementController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    acknowledgementPdfService,
    sectionRenderingService,
    controllersModule.messagesControllerComponents
  )

  val errorController = new ErrorController(
    configModule.frontendAppConfig,
    playBuiltInsModule.i18nSupport,
    controllersModule.messagesControllerComponents,
    controllersModule.nonAuthenticatedRequestActions
  )

  val agentEnrolmentController = new AgentEnrolmentController(
    configModule.appConfig,
    configModule.frontendAppConfig,
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    controllersModule.messagesControllerComponents
  )

  val declarationController = new DeclarationController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    objectStoreModule.objectStoreService,
    validationModule.validationService,
    sectionRenderingService,
    gformBackendModule.gformConnector,
    processDataService,
    gformBackEndService,
    submissionService,
    controllersModule.messagesControllerComponents
  )

  val newFormController: NewFormController = new NewFormController(
    configModule.frontendAppConfig,
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    objectStoreModule.objectStoreService,
    gformBackendModule.gformConnector,
    fastForwardService,
    auditingModule.auditService,
    graphModule.recalculation,
    controllersModule.messagesControllerComponents,
    gformBackEndService,
    ninoInsightsConnector,
    englishMessages,
    acknowledgementPdfService
  )

  val reviewService = new ReviewService(gformBackEndService, lookupRegistry)

  val reviewController = new ReviewController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    gformBackEndService,
    reviewService,
    controllersModule.messagesControllerComponents
  )

  val languageSwitchController: LanguageSwitchController =
    new LanguageSwitchController(
      controllersModule.authenticatedRequestActions,
      new LanguageUtils(playBuiltInsModule.langs, configModule.playConfiguration)(playBuiltInsModule.messagesApi),
      lookupRegistry,
      gformBackendModule.gformConnector,
      configModule.frontendAppConfig,
      controllersModule.messagesControllerComponents,
      builtInComponents.defaultActionBuilder
    )

  val lookupController = new LookupController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    lookupRegistry,
    controllersModule.messagesControllerComponents
  )

  val signOutController: SignOutController =
    new SignOutController(
      configModule.frontendAppConfig,
      controllersModule.messagesControllerComponents,
      controllersModule.nonAuthenticatedRequestActions,
      controllersModule.authenticatedRequestActions,
      auditingModule.auditService
    )

  val staticPagesController: StaticPagesController =
    new StaticPagesController(
      controllersModule.authenticatedRequestActions,
      playBuiltInsModule.i18nSupport,
      configModule.frontendAppConfig,
      controllersModule.messagesControllerComponents
    )

  val identityVerificationController: IdentityVerificationController =
    new IdentityVerificationController(
      controllersModule.authenticatedRequestActions,
      playBuiltInsModule.i18nSupport,
      configModule.frontendAppConfig,
      controllersModule.messagesControllerComponents
    )

  implicit val s: Scheduler = akkaModule.actorSystem.scheduler
  val upscanController: UpscanController =
    new UpscanController(
      controllersModule.authenticatedRequestActions,
      fastForwardService,
      gformBackEndService,
      upscanModule.upscanService,
      playBuiltInsModule.i18nSupport,
      controllersModule.messagesControllerComponents,
      configModule.appConfig
    )

  val compositeAuthController: CompositeAuthController = new CompositeAuthController(
    playBuiltInsModule.i18nSupport,
    controllersModule.messagesControllerComponents,
    controllersModule.nonAuthenticatedRequestActions,
    configModule.frontendAppConfig
  )

  val addressLookupController: AddressLookupController =
    new AddressLookupController(
      controllersModule.authenticatedRequestActions,
      addressLookupModule.addressLookupService,
      configModule.frontendAppConfig,
      playBuiltInsModule.i18nSupport,
      controllersModule.messagesControllerComponents,
      graphModule.recalculation,
      formControllerRequestHandler,
      validationModule.validationService,
      fastForwardService,
      lookupRegistry
    )

  val buttonController: PaymentController = new PaymentController(
    controllersModule.authenticatedRequestActions,
    wSHttpModule.auditableWSHttp,
    controllersModule.messagesControllerComponents,
    configModule.appConfig,
    configModule.serviceConfig
  )

  val captureController: CaptureController = new CaptureController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    controllersModule.messagesControllerComponents
  )

  val downloadController: DownloadController =
    new DownloadController(
      controllersModule.messagesControllerComponents,
      controllersModule.authenticatedRequestActions,
      configModule.environment
    )

  val imageController: ImageController =
    new ImageController(
      controllersModule.messagesControllerComponents,
      controllersModule.authenticatedRequestActions,
      configModule.environment
    )

  private val allowedHosts = configModule.typesafeConfig.getStringList("redirectAllowedHosts").asScala.toSet
  private val redirectUrlPolicy = AbsoluteWithHostnameFromAllowlist(allowedHosts)
  val redirectController: RedirectController =
    new RedirectController(controllersModule.messagesControllerComponents, redirectUrlPolicy)
}
