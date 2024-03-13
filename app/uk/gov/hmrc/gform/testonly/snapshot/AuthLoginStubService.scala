/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.testonly.snapshot

import cats.data.EitherT
import cats.syntax.either._
import cats.syntax.eq._
import play.api.http.Status._
import play.api.mvc.{ Cookie, Session, SessionCookieBaker }
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.crypto.Crypted
import uk.gov.hmrc.gform.auth.AuthLoginStubConnector
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.SessionCookieCrypto

class AuthLoginStubService(
  connector: AuthLoginStubConnector,
  sessionCookieCrypto: SessionCookieCrypto,
  sessionCookieBaker: SessionCookieBaker
)(implicit ec: ExecutionContext) {

  def getSession(
    loginData: GovernmentGatewayFormData
  ): Future[Session] =
    login(loginData).fold(
      e => throw new Exception(s"can not authenticate user ${e.error}"),
      identity
    )

  private def login(
    loginData: GovernmentGatewayFormData
  ): EitherT[Future, UnexpectedState, Session] =
    connector.login(loginData).subflatMap { httpResponse =>
      val status = httpResponse.status
      val location = httpResponse.header("location")
      if (status =!= SEE_OTHER)
        Left(
          UnexpectedState(
            s"Call to login came back with status $status. Body was ${httpResponse.body}"
          )
        )
      else if (!location.contains(loginData.redirectionUrl))
        Left(
          UnexpectedState(
            s"Location header value [${location.getOrElse("")}] of response was not ${loginData.redirectionUrl}"
          )
        )
      else {
        val sessionOpt =
          for {
            cookie <- httpResponse.cookie("mdtp")
            decrypted = Cookie(name = "mdtp", value = sessionCookieCrypto.crypto.decrypt(Crypted(cookie.value)).value)
            session = sessionCookieBaker.decodeFromCookie(Some(decrypted))
          } yield session

        Either.fromOption(sessionOpt, UnexpectedState("Could not extract session"))
      }
    }

}
