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

package uk.gov.hmrc.gform.gform

import cats.instances.future._

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.auth.{ AgentEnrolmentController, AuthModule, ErrorController }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.{ ControllersModule, ErrResponder }
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gform.handlers.{ FormControllerRequestHandler, FormValidator }
import uk.gov.hmrc.gform.gformbackend.{ GformBackEndService, GformBackendModule }
import uk.gov.hmrc.gform.graph.GraphModule
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.{ ProcessDataService, TaxPeriodStateChecker }
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule
import uk.gov.hmrc.gform.summary.SummaryRenderingService
import uk.gov.hmrc.gform.summarypdf.{ PdfGeneratorConnector, PdfGeneratorService }
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.play.language.LanguageUtils

class GformModule(
  configModule: ConfigModule,
  wSHttpModule: WSHttpModule,
  controllersModule: ControllersModule,
  pdfGeneratorConnector: PdfGeneratorConnector,
  authModule: AuthModule,
  gformBackendModule: GformBackendModule,
  fileUploadModule: FileUploadModule,
  validationModule: ValidationModule,
  auditingModule: AuditingModule,
  playBuiltInsModule: PlayBuiltInsModule,
  graphModule: GraphModule,
  lookupRegistry: LookupRegistry,
  errorResponder: ErrResponder
)(
  implicit ec: ExecutionContext
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
    configModule.appConfig,
    graphModule.recalculation,
    authModule.taxEnrolmentsConnector,
    authModule.ggConnector,
    configModule.frontendAppConfig,
    controllersModule.messagesControllerComponents,
    graphModule.smartStringEvaluatorFactory
  )

  val taxPeriodStateChecker = new TaxPeriodStateChecker[Future, Throwable] {
    def error: Throwable = new Exception("Call to des to retrieve obligation-data has failed")
  }

  val processDataService: ProcessDataService[Future] =
    new ProcessDataService[Future](graphModule.recalculation, taxPeriodStateChecker)

  val formControllerRequestHandler = new FormControllerRequestHandler(new FormValidator())

  val fastForwardService: FastForwardService = new FastForwardService(
    fileUploadModule.fileUploadService,
    validationModule.validationService,
    gformBackendModule.gformConnector,
    processDataService,
    formControllerRequestHandler,
    graphModule.smartStringEvaluatorFactory
  )

  val newFormController: NewFormController = new NewFormController(
    configModule.frontendAppConfig,
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    fileUploadModule.fileUploadService,
    gformBackendModule.gformConnector,
    fastForwardService,
    graphModule.recalculation,
    controllersModule.messagesControllerComponents
  )

  val formController: FormController = new FormController(
    configModule.appConfig,
    configModule.frontendAppConfig,
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    fileUploadModule.fileUploadService,
    validationModule.validationService,
    sectionRenderingService,
    gformBackendModule.gformConnector,
    processDataService,
    formControllerRequestHandler,
    lookupRegistry.extractors,
    fastForwardService,
    graphModule.recalculation,
    controllersModule.messagesControllerComponents
  )

  val summaryRenderingService = new SummaryRenderingService(
    playBuiltInsModule.i18nSupport,
    fileUploadModule.fileUploadService,
    graphModule.recalculation,
    validationModule.validationService,
    configModule.frontendAppConfig
  )

  val pdfGeneratorService = new PdfGeneratorService(
    playBuiltInsModule.i18nSupport,
    pdfGeneratorConnector,
    summaryRenderingService
  )

  val summaryController: SummaryController = new SummaryController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    fileUploadModule.fileUploadService,
    validationModule.validationService,
    pdfGeneratorService,
    gformBackendModule.gformConnector,
    configModule.frontendAppConfig,
    errorResponder,
    summaryRenderingService,
    controllersModule.messagesControllerComponents
  )

  val printSectionController: PrintSectionController = new PrintSectionController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    pdfGeneratorService,
    sectionRenderingService,
    summaryRenderingService,
    controllersModule.messagesControllerComponents
  )

  val acknowledgementController: AcknowledgementController = new AcknowledgementController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    pdfGeneratorService,
    sectionRenderingService,
    summaryRenderingService,
    gformBackendModule.gformConnector,
    new NonRepudiationHelpers(auditingModule),
    controllersModule.messagesControllerComponents,
    graphModule.recalculation,
    auditingModule.auditService
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

  val gformBackEndService = new GformBackEndService(
    gformBackendModule.gformConnector,
    summaryRenderingService,
    lookupRegistry
  )

  val declarationController = new DeclarationController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    auditingModule.auditService,
    fileUploadModule.fileUploadService,
    validationModule.validationService,
    sectionRenderingService,
    gformBackendModule.gformConnector,
    processDataService,
    gformBackEndService,
    controllersModule.messagesControllerComponents
  )

  val reviewService = new ReviewService(gformBackEndService, lookupRegistry, graphModule.recalculation)

  val reviewController = new ReviewController(
    controllersModule.authenticatedRequestActions,
    gformBackEndService,
    reviewService,
    controllersModule.messagesControllerComponents
  )

  val languageSwitchController: LanguageSwitchController =
    new LanguageSwitchController(
      configModule.playConfiguration,
      new LanguageUtils(playBuiltInsModule.langs, configModule.playConfiguration)(playBuiltInsModule.messagesApi),
      configModule.frontendAppConfig,
      controllersModule.messagesControllerComponents
    )

  val lookupController = new LookupController(
    controllersModule.authenticatedRequestActions,
    lookupRegistry,
    controllersModule.messagesControllerComponents
  )

  val signOutController: SignOutController =
    new SignOutController(
      configModule.frontendAppConfig,
      controllersModule.messagesControllerComponents,
      controllersModule.nonAuthenticatedRequestActions
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
}
