/*
 * Copyright 2017 HM Revenue & Customs
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

import play.api.mvc.{ AnyContent, Request }
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.connectors.{ EeittConnector, Verification }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.RegimeId
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class EeittAuthorisationDelegate(eeittConnector: EeittConnector, configModule: ConfigModule) {

  def authenticate(regimeId: RegimeId, userDetails: UserDetails)(implicit hc: HeaderCarrier, ex: ExecutionContext, request: Request[AnyContent]): Future[EeittAuthResult] = {

    val authResultF = eeittConnector.isAllowed(userDetails.groupIdentifier, regimeId, userDetails.affinityGroup)

    authResultF.map {
      case Verification(true) => EeittAuthorisationSuccessful
      case Verification(false) => EeittUnauthorisationFailed(eeittLoginUrl())
    }
  }

  private def eeittLoginUrl()(implicit request: Request[AnyContent]): String = {

    val continueUrl = java.net.URLEncoder.encode(
      configModule.appConfig.`gform-frontend-base-url` + request.uri, "UTF-8"
    )
    val eeittLoginUrl = s"${configModule.serviceConfig.baseUrl("eeitt-frontend")}/eeitt-auth/enrollment-verification"
    s"${eeittLoginUrl}?callbackUrl=${continueUrl}"
  }
}

sealed trait EeittAuthResult
final object EeittAuthorisationSuccessful extends EeittAuthResult
final case class EeittUnauthorisationFailed(loginUrl: String) extends EeittAuthResult