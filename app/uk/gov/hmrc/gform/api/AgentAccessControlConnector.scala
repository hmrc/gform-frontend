/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.api

import cats.implicits.catsSyntaxEq
import org.slf4j.{ Logger, LoggerFactory }
import play.api.http.Status
import play.api.libs.json.{ JsString, Json }
import uk.gov.hmrc.domain.EmpRef
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, DataRetrieve, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

trait AgentAccessControlConnector[F[_]] {
  def mtdVatAuth(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): F[ServiceCallResponse[DataRetrieve.Response]]

  def payeAuth(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): F[ServiceCallResponse[DataRetrieve.Response]]

}

class AgentAccessControlAsyncConnector(ws: WSHttp, baseUrl: String, auth: AuthenticatedRequestActions)(implicit
  ex: ExecutionContext
) extends AgentAccessControlConnector[Future] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private val FORBIDDEN = "FORBIDDEN"
  private val AUTHORISED = "AUTHORISED"
  private val JSON_KEY = "authorised"

  private val mtdVatAuth = "mtd-vat-auth"
  private val payeAuth = "epaye-auth"

  private val mtdVatAuthSuffix = "{{vatRegistrationNumber}}"

  private def getBaseUrl(authRule: String) = s"$baseUrl/$authRule/agent/{{agentCode}}/client/"
// TODO:
//  private def getBaseUrl(authRule: String, agentCode: String, clientRef: String) = s"$baseUrl/$authRule/agent/$agentCode/client/$clientRef"

  override def mtdVatAuth(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): Future[ServiceCallResponse[DataRetrieve.Response]] = {
    val url: String = request.fillPlaceholders(getBaseUrl(mtdVatAuth) + mtdVatAuthSuffix)
    callAgentAccessControl(dataRetrieve, url, mtdVatAuth)
  }

  override def payeAuth(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): Future[ServiceCallResponse[DataRetrieve.Response]] = {
    val refCheck = Try {
      val payeReference = request.params.find(_._1 === "payeReference").map(t => EmpRef.fromIdentifiers(t._2))
      val encodedRef = payeReference.map(_.encodedValue)
      val url: String = getBaseUrl(payeAuth) + encodedRef.getOrElse("")
      callAgentAccessControl(dataRetrieve, url, payeAuth)
    }

    refCheck match {
      case Success(result) => result
      case Failure(exception) =>
        logger.error(exception.getMessage)
        Future.successful(processResponse(dataRetrieve, FORBIDDEN, payeAuth))
    }
  }

  private def callAgentAccessControl(dataRetrieve: DataRetrieve, rawUrl: String, identifier: String)(implicit
    hc: HeaderCarrier
  ) =
    for {
      maybeAgentCode <- auth.getAgentCodeRetrieval.map(_.value)
      maybeUrl = maybeAgentCode.map(code => rawUrl.replaceAll("\\{\\{agentCode}}", code))
      res <- maybeUrl.fold(Future.successful(processResponse(dataRetrieve, FORBIDDEN, identifier))) { url =>
               ws.GET[HttpResponse](url)
                 .map { httpResponse =>
                   val responseString = httpResponse.status match {
                     case Status.OK           => AUTHORISED
                     case Status.UNAUTHORIZED => httpResponse.body.toUpperCase
                     case _                   => FORBIDDEN
                   }
                   processResponse(dataRetrieve, responseString, identifier)
                 }
                 .recover { case ex =>
                   logger.error(s"Unknown error when calling agent access control ($identifier)", ex)
                   CannotRetrieveResponse
                 }
             }
    } yield res

  private def processResponse(dataRetrieve: DataRetrieve, responseString: String, identifier: String) = dataRetrieve
    .processResponse(Json.obj((JSON_KEY, JsString(responseString))))
    .fold(
      invalid => {
        logger.error(
          s"Fetching agent access control ($identifier) found result, but marshalling of data failed with: $invalid"
        )
        CannotRetrieveResponse
      },
      valid => {
        logger.info(s"Fetching agent access control ($identifier) returned: Success.")
        ServiceResponse(valid)
      }
    )

}
