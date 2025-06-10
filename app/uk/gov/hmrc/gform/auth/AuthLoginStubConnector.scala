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
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpResponse, StringContextOps }
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.control.NonFatal
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.testonly.snapshot._
class AuthLoginStubConnector(baseUrl: String, httpClientV2: HttpClientV2) {
  private val authLoginStubUrl: String =
    s"$baseUrl/auth-login-stub/gg-sign-in"
  def login(
    loginData: GovernmentGatewayFormData
  )(implicit ec: ExecutionContext): EitherT[Future, UnexpectedState, HttpResponse] = {
    val formData = GovernmentGatewayFormData.toUrlEncoded(loginData)

    implicit val hc: HeaderCarrier = HeaderCarrier()
    implicit val rawHttpReads: HttpReads[HttpResponse] = HttpReads.Implicits.readRaw
    EitherT[Future, UnexpectedState, HttpResponse](
      httpClientV2
        .post(url"$authLoginStubUrl")
        .setHeader("Content-Type" -> "application/x-www-form-urlencoded")
        .transform(_.withFollowRedirects(false))
        .withBody(formData)
        .execute[HttpResponse]
        .map(_.asRight[UnexpectedState])
        .recover { case NonFatal(e) => UnexpectedState(e.getMessage).asLeft[HttpResponse] }
    )
  }
}
