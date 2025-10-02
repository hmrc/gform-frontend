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

import java.time.Instant
import java.util.UUID
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.mvc.Results.Redirect
import play.api.mvc.Session
import uk.gov.hmrc.gform.testonly.snapshot._
import uk.gov.hmrc.http.{ SessionId, SessionKeys }
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, StringContextOps }
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class AuthLoginApiConnector(baseUrl: String, httpClientV2: HttpClientV2) {

  private val authLoginStubUrl = url"$baseUrl/government-gateway/session/login"

  // This is based on auth-login-stub:
  // https://github.com/hmrc/auth-login-stub/blob/b88e26404bc9516c259a8b8d10ea7854721f2b53/app/wizard/gg/connector/GovernmentGatewayApiConnector.scala#L29-L97
  def createNewSession(
    loginData: GovernmentGatewayFormData
  )(implicit ec: ExecutionContext): Future[Result] = {

    val payload = Json.obj(
      "credId"             -> loginData.credId,
      "affinityGroup"      -> loginData.affinityGroup,
      "confidenceLevel"    -> loginData.confidenceLevel.toInt,
      "credentialStrength" -> loginData.credentialStrength,
      "credentialRole"     -> loginData.credentialRole,
      "usersName"          -> loginData.usersName,
      "enrolments" -> Json.toJson(loginData.enrolments.map { enrolment =>
        Json.obj(
          "key" -> enrolment.name,
          "identifiers" -> enrolment.taxIdentifier.map { taxIdentifier =>
            Json.obj(
              "key"   -> taxIdentifier.key,
              "value" -> taxIdentifier.value
            )
          },
          "state" -> enrolment.state
        )
      }),
      "delegatedEnrolments" -> Json.arr(),
      "nino"                -> loginData.nino.map(_.value),
      "groupIdentifier"     -> loginData.groupIdentifier,
      "gatewayToken"        -> loginData.gatewayToken,
      "email"               -> loginData.email,
      "itmpData"            -> Json.toJson(loginData.itmpData),
      "agentId"             -> loginData.agent.flatMap(_.agentId),
      "agentCode"           -> loginData.agent.flatMap(_.agentCode),
      "agentFriendlyName"   -> loginData.agent.flatMap(_.agentFriendlyName)
    )

    implicit val hc: HeaderCarrier = HeaderCarrier()
    httpClientV2
      .post(authLoginStubUrl)
      .withBody(payload)
      .execute[HttpResponse]
      .flatMap {
        case response @ HttpResponse(201, _, _) =>
          (
            response.header(HeaderNames.AUTHORIZATION),
            response.header(HeaderNames.LOCATION)
          ) match {
            case (Some(token), Some(sessionUri)) =>
              Future.successful(
                Redirect(loginData.redirectionUrl)
                  .withSession(
                    Session {
                      val session = s"session-${UUID.randomUUID}"
                      Map(
                        SessionKeys.sessionId            -> SessionId(session).value,
                        SessionKeys.authToken            -> token,
                        SessionKeys.lastRequestTimestamp -> Instant.now.toEpochMilli.toString
                      )
                    }
                  )
              )

            case _ =>
              Future.failed(
                new RuntimeException(
                  "Internal Error, missing headers or gatewayToken in response from auth-login-api"
                )
              )
          }
        case response =>
          Future.failed(
            new RuntimeException(
              s"response from /government-gateway/session/login was ${response.status}. Body ${response.body}"
            )
          )
      }
  }
}
