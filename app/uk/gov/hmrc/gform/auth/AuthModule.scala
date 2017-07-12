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

import javax.inject.Inject

import play.api.mvc.Results.Redirect
import play.api.mvc.{ AnyContent, Request, Result }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.play.frontend.auth
import uk.gov.hmrc.play.frontend.auth._
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.HttpGet

import scala.concurrent.Future

class AuthModule @Inject() (configModule: ConfigModule, wSHttpModule: WSHttpModule) { self =>

  lazy val authConnector = new AuthConnector {
    override val serviceUrl: String = configModule.serviceConfig.baseUrl("auth")
    override val http: HttpGet = wSHttpModule.auditableWSHttp
  }

  val authActions: auth.Actions = new uk.gov.hmrc.play.frontend.auth.Actions {
    def authConnector: AuthConnector = self.authConnector
  }

  val authenticatedBy: auth.Actions#AuthenticatedBy = new authActions.AuthenticatedBy(governmentGateway, taxRegime, alwaysVisiblePageVisibility)

  /********************* private *********************/

  private lazy val governmentGateway = new GovernmentGateway {
    override def redirectToLogin(implicit request: Request[_]): Future[Result] = {
      val queryStringParams = Map("continue" -> Seq(continueURL + request.uri))
      Future.successful(Redirect(loginURL, queryStringParams))
    }
    override def continueURL: String = configModule.appConfig.`gform-frontend-base-url`
    override def loginURL: String = configModule.appConfig.`government-gateway-sign-in-url`
  }

  private lazy val alwaysVisiblePageVisibility = new PageVisibilityPredicate {
    def apply(authContext: AuthContext, request: Request[AnyContent]): Future[PageVisibilityResult] = Future.successful(PageIsVisible)
  }

  private lazy val taxRegime: Option[TaxRegime] = None
}
