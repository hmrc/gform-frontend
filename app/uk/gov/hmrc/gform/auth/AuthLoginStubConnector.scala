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

package uk.gov.hmrc.gform.auth

import cats.data.EitherT
import cats.syntax.either._
import play.api.libs.ws.WSClient
import play.api.libs.ws.WSResponse
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.control.NonFatal
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.testonly._
import org.slf4j.{ Logger, LoggerFactory }

class AuthLoginStubConnector(baseUrl: String, wsClient: WSClient) {
  val serviceUrl = baseUrl

  private val logger: Logger = LoggerFactory.getLogger(getClass)
  private val authLoginStubUrl: String =
    s"$baseUrl/auth-login-stub/gg-sign-in"

  logger.info(s"AuthLoginStubUrl: $authLoginStubUrl ")
  def login(
    loginData: GovernmentGatewayFormData
  )(implicit ec: ExecutionContext): EitherT[Future, UnexpectedState, WSResponse] = {

    val formData =
      GovernmentGatewayFormData.toUrlEncoded(loginData)

    logger.info(s"AuthLoginStubUrl: $authLoginStubUrl ")
    EitherT[Future, UnexpectedState, WSResponse](
      wsClient
        .url(authLoginStubUrl)
        .withFollowRedirects(false)
        .withHttpHeaders("Content-Type" -> "application/x-www-form-urlencoded")
        .post(formData)
        .map(_.asRight[UnexpectedState])
        .recover { case NonFatal(e) => UnexpectedState(e.getMessage()).asLeft[WSResponse] }
    )
  }

}
