/*
 * Copyright 2018 HM Revenue & Customs
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

import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.auth.{ AuthModule, EnrolmentService, ErrorController }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.controllers.ControllersModule
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.keystore.{ KeystoreModule, RepeatingComponentService }
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.playcomponents.{ PlayBuiltInsModule, RoutingModule }
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorModule
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule

class GformModule(
    configModule: ConfigModule,
    wSHttpModule: WSHttpModule,
    keystoreModule: KeystoreModule,
    controllersModule: ControllersModule,
    pdfGeneratorModule: PdfGeneratorModule,
    authModule: AuthModule,
    gformBackendModule: GformBackendModule,
    fileUploadModule: FileUploadModule,
    validationModule: ValidationModule,
    auditingModule: AuditingModule,
    playBuiltInsModule: PlayBuiltInsModule
) {

  private val authContextPrepop = new AuthContextPrepop

  private val prepopService: PrepopService = new PrepopService(eeittConnector, authContextPrepop, keystoreModule.repeatingComponentService)

  private val sectionRenderingService: SectionRenderingService = new SectionRenderingService(
    keystoreModule.repeatingComponentService,
    prepopService,
    configModule.frontendAppConfig
  )

  //TODO: there is another eeittConnector - instantiate them in common module
  private lazy val eeittConnector = new EeittConnector(
    configModule.serviceConfig.baseUrl("eeitt") + "/eeitt",
    wSHttpModule.auditableWSHttp
  )

  val enrolmentController = new EnrolmentController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    sectionRenderingService,
    validationModule.validationService,
    keystoreModule.repeatingComponentService,
    gformBackendModule.gformConnector,
    authModule.enrolmentService,
    configModule.appConfig
  )

  val formController: FormController = new FormController(
    configModule.appConfig,
    configModule.frontendAppConfig,
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    keystoreModule.repeatingComponentService,
    fileUploadModule.fileUploadService,
    validationModule.validationService,
    sectionRenderingService,
    gformBackendModule.gformConnector
  )

  val summaryController: SummaryController = new SummaryController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    keystoreModule.repeatingComponentService,
    fileUploadModule.fileUploadService,
    validationModule.validationService,
    pdfGeneratorModule.pdfGeneratorService,
    gformBackendModule.gformConnector,
    configModule.frontendAppConfig,
    controllersModule.errResponder
  )

  val acknowledgementController: AcknowledgementController = new AcknowledgementController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    pdfGeneratorModule.pdfGeneratorService,
    sectionRenderingService,
    keystoreModule.repeatingComponentService,
    summaryController,
    authModule.authService,
    gformBackendModule.gformConnector,
    new NonRepudiationHelpers(auditingModule)
  )

  val errorController = new ErrorController(
    configModule.frontendAppConfig,
    playBuiltInsModule.i18nSupport
  )

  val declarationController = new DeclarationController(
    playBuiltInsModule.i18nSupport,
    configModule.frontendAppConfig,
    controllersModule.authenticatedRequestActions,
    gformBackendModule.gformConnector,
    auditingModule.auditService,
    keystoreModule.repeatingComponentService,
    summaryController,
    pdfGeneratorModule.pdfGeneratorService,
    sectionRenderingService,
    validationModule.validationService,
    authModule.authService
  )
}
