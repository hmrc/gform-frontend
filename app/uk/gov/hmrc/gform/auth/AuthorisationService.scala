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

import play.api.mvc.Result
import uk.gov.hmrc.gform.auth.models.{ AuthResult, UnAuthenticated, UserDetails }
import uk.gov.hmrc.gform.gformbackend.model.{ AuthConfigModule, FormTemplate, FormTypeId }
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthorisationService(eeittAuth: EeittAuthorisationDelegate, authConnector: AuthConnector /*xxxAuthDelegate*/ ) {

  def getUserDetail(implicit authContext: AuthContext, hc: HeaderCarrier) =
    authConnector.getUserDetails[UserDetails](authContext)

  def doAuthorise(formTemplate: FormTemplate, userDetails: UserDetails)(implicit hc: HeaderCarrier): Future[AuthResult] = {
    formTemplate.authConfig.authModule.value match {
      case AuthConfigModule.legacyEEITTAuth => eeittAuth.legacyAuth(formTemplate, userDetails)
      case _ => Future.successful(UnAuthenticated) //TODO Dave New Auth Method
    }
  }

}
