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

package uk.gov.hmrc.gform.auth

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule

class AuthModule(configModule: ConfigModule, wSHttpModule: WSHttpModule)(implicit
  ec: ExecutionContext
) {
  self =>

  lazy val authConnector = new AuthConnector(
    configModule.serviceConfig.baseUrl("auth"),
    wSHttpModule.httpClient,
    configModule.playConfiguration
  )

  lazy val ggConnector = new GovernmentGatewayConnector(
    configModule.serviceConfig.baseUrl("gg"),
    wSHttpModule.httpClient
  )

  lazy val taxEnrolmentsConnector = new TaxEnrolmentsConnector(
    configModule.serviceConfig.baseUrl("tax-enrolments"),
    wSHttpModule.httpClient
  )

  lazy val selfEmployedIncomeSupportEligibilityConnector = new SelfEmployedIncomeSupportEligibilityConnector(
    configModule.serviceConfig.baseUrl("seiss"),
    wSHttpModule.httpClient
  )

  lazy val enrolmentStoreProxyConnector = new EnrolmentStoreProxyConnector(
    configModule.serviceConfig.baseUrl("enrolment-store-proxy"),
    wSHttpModule.httpClient
  )

  lazy val delegatedEnrolmentService = new DelegatedEnrolmentService(
    enrolmentStoreProxyConnector
  )

  val enrolmentService: EnrolmentService = new EnrolmentService(
    configModule.typesafeConfig.getBoolean("enrolment-service.use-tax-enrolments"),
    configModule.serviceConfig.getConfString("gg.enrol.portalId", "")
  )

  lazy val authService: AuthService = new AuthService(
    configModule.appConfig
  )

}
