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

package uk.gov.hmrc.gform.auth

import javax.inject.Inject
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.wshttp.WSHttpModule

class AuthModule(configModule: ConfigModule, wSHttpModule: WSHttpModule) { self =>

  lazy val authConnector = new AuthConnector(
    configModule.serviceConfig.baseUrl("auth"),
    wSHttpModule.auditableWSHttp
  )

  private lazy val eeittConnector = new EeittConnector(
    s"${configModule.serviceConfig.baseUrl("eeitt")}/eeitt",
    wSHttpModule.auditableWSHttp
  )

  private lazy val ggConnector = new GovernmentGatewayConnector(
    configModule.serviceConfig.baseUrl("gg"),
    wSHttpModule.auditableWSHttp
  )

  private lazy val taxEnrolmentsConnector = new TaxEnrolmentsConnector(
    configModule.serviceConfig.baseUrl("tax-enrolments"),
    wSHttpModule.auditableWSHttp
  )

  val enrolmentService: EnrolmentService = new EnrolmentService(
    configModule.serviceConfig.getConfBool("enrolment-service.use-tax-enrolments", false),
    configModule.serviceConfig.getConfString("gg.enrol.portalId", ""),
    ggConnector,
    taxEnrolmentsConnector
  )

  lazy val eeittAuthorisationDelegate = new EeittAuthorisationDelegate(eeittConnector, configModule)

  lazy val authService: AuthService = new AuthService
}
