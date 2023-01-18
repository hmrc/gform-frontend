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

package uk.gov.hmrc.gform.api

import org.slf4j.{ Logger, LoggerFactory }
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }
import scala.language.higherKinds

trait NinoInsightsConnector[F[_]] {
  def insights(ninoInsightsRequest: NinoInsightCheck.Request): F[ServiceCallResponse[NinoInsightCheck.Response]]
}

class NinoInsightsAsyncConnector(ws: WSHttp, baseUrl: String, authorizationToken: String)(implicit ex: ExecutionContext)
    extends NinoInsightsConnector[Future] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def insights(
    ninoInsightsRequest: NinoInsightCheck.Request
  ): Future[ServiceCallResponse[NinoInsightCheck.Response]] = {
    implicit val hc = HeaderCarrier()
    val url = s"$baseUrl/check/insights"

    val header = Seq(
      "Authorization" -> authorizationToken,
      "Content-Type"  -> "application/json"
    )

    ws.POST[NinoInsightCheck.Request, HttpResponse](
      url,
      ninoInsightsRequest,
      header
    ).map { httpResponse =>
      val status = httpResponse.status
      status match {
        case 200 =>
          httpResponse.json
            .validate[NinoInsightCheck.Response]
            .fold(
              invalid => {
                logger.error(
                  s"Calling nino insights returned $status, but marshalling of data failed with: $invalid"
                )
                CannotRetrieveResponse
              },
              valid => {
                logger.info(s"Calling nino insights returned $status: Success.")
                ServiceResponse(valid)
              }
            )
        case other =>
          logger.error(
            s"Problem when calling nino insights. Http status: $other, body: ${httpResponse.body}"
          )
          CannotRetrieveResponse
      }
    }.recover { case ex =>
      logger.error("Unknown problem when calling nino insights", ex)
      CannotRetrieveResponse
    }
  }
}

object NinoInsightCheck {
  case class Request(nino: String)

  def create(nino: String) = Request(nino)

  object Request {
    implicit val format: Format[Request] = Json.format[Request]
  }

  case class Response(riskScore: Int, reason: String)

  object Response {

    private val apiReads: Reads[Response] =
      ((__ \ "riskScore").read[Int] and
        (__ \ "reason").read[String])(Response.apply _)

    private val apiWrites: Writes[Response] =
      ((__ \ "riskScore").write[Int] and
        (__ \ "reason").write[String])(unlift(Response.unapply))

    implicit val format: Format[Response] = Format(apiReads, apiWrites)
  }
}
