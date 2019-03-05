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

package uk.gov.hmrc.gform.gform

import cats.instances.future._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.auth.{ AgentEnrolmentController, AuthModule, ErrorController }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.ControllersModule
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.graph.GraphModule
import uk.gov.hmrc.gform.models.ProcessDataService
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorModule
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule

class GformModule(
  configModule: ConfigModule,
  wSHttpModule: WSHttpModule,
  controllersModule: ControllersModule,
  pdfGeneratorModule: PdfGeneratorModule,
  authModule: AuthModule,
  gformBackendModule: GformBackendModule,
  fileUploadModule: FileUploadModule,
  validationModule: ValidationModule,
  auditingModule: AuditingModule,
  playBuiltInsModule: PlayBuiltInsModule,
  graphModule: GraphModule
) {

  private val sectionRenderingService: SectionRenderingService = new SectionRenderingService(
    configModule.frontendAppConfig
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
    authModule.ggConnector
  )

  val processDataService: ProcessDataService[Future, Throwable] =
    new ProcessDataService[Future, Throwable](graphModule.recalculation)

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
    controllersModule.obligationService,
    controllersModule.formService
  )

  val summaryController: SummaryController = new SummaryController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    fileUploadModule.fileUploadService,
    validationModule.validationService,
    pdfGeneratorModule.pdfGeneratorService,
    gformBackendModule.gformConnector,
    configModule.frontendAppConfig,
    controllersModule.errResponder,
    graphModule.recalculation,
    controllersModule.obligationService
  )

  val acknowledgementController: AcknowledgementController = new AcknowledgementController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    pdfGeneratorModule.pdfGeneratorService,
    sectionRenderingService,
    summaryController,
    authModule.authService,
    gformBackendModule.gformConnector,
    new NonRepudiationHelpers(auditingModule)
  )

  val errorController = new ErrorController(
    configModule.frontendAppConfig,
    playBuiltInsModule.i18nSupport
  )

  val agentEnrolmentController = new AgentEnrolmentController(
    configModule.appConfig,
    configModule.frontendAppConfig,
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions
  )

  val declarationController = new DeclarationController(
    playBuiltInsModule.i18nSupport,
    configModule.frontendAppConfig,
    controllersModule.authenticatedRequestActions,
    gformBackendModule.gformConnector,
    auditingModule.auditService,
    summaryController,
    pdfGeneratorModule.pdfGeneratorService,
    sectionRenderingService,
    validationModule.validationService,
    authModule.authService,
    graphModule.recalculation
  )
}
