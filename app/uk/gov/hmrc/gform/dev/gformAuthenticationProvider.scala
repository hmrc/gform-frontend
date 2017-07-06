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

package uk.gov.hmrc.gform.dev

import javax.inject.{ Inject, Singleton }
import play.api.Configuration
import play.api.mvc.{ Cookie, Request, Result }
import play.api.mvc.Results.Redirect
import play.api.libs.ws.WSClient
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.crypto.{ ApplicationCrypto, Crypted }
import uk.gov.hmrc.play.frontend.auth.GovernmentGateway

@Singleton
class gformAuthenticationProvider @Inject() (configuration: Configuration, ws: WSClient) extends GovernmentGateway {

  private val gformFrontendBaseUrl = configuration.getString("gform-frontend-base-url").getOrElse("")
  private val governmentGatewaySignInUrl = configuration.getString("government-gateway-sign-in-url").getOrElse("")

  val crypto = ApplicationCrypto.SessionCookieCrypto

  def decrypt(encryptedCookie: String): String = crypto.decrypt(Crypted(encryptedCookie)).value

  override def redirectToLogin(implicit request: Request[_]): Future[Result] = {

    val payload = Map(
      "authorityId" -> Seq("543212300667"),
      "redirectionUrl" -> Seq("http://www.google.com"),
      "credentialStrength" -> Seq("weak"),
      "confidenceLevel" -> Seq("50"),
      "affinityGroup" -> Seq("Individual"),
      "nino" -> Seq("CS700100B"),
      "gatewayToken" -> Seq(""),
      "enrolment[0].name" -> Seq("IR-SA"),
      "enrolment[0].taxIdentifier[0].name" -> Seq("UTR"),
      "enrolment[0].taxIdentifier[0].value" -> Seq("1234567891"),
      "enrolment[0].state" -> Seq("Activated")
    )

    ws.url(governmentGatewaySignInUrl).withFollowRedirects(false).post(payload).map { wsResponse =>
      val cookies =
        wsResponse.cookies.flatMap { wsCookie =>
          for {
            name <- wsCookie.name
            value <- wsCookie.value
          } yield {
            if (name == "mdtp") {
              Some(Cookie(name, decrypt(value)))
            } else {
              None
            }
          }
        }
      Redirect(gformFrontendBaseUrl + request.uri).withCookies(cookies.flatten: _*)
    }
  }

  def continueURL: String = "not used since we override redirectToLogin"

  def loginURL: String = governmentGatewaySignInUrl
}
