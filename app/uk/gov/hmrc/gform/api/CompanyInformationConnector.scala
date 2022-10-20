/*
 * Copyright 2022 HM Revenue & Customs
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

import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpReads.Implicits.readFromJson

import scala.concurrent.{ ExecutionContext, Future }
import scala.language.higherKinds

trait CompanyInformationConnector[F[_]] {
  def companyProfile(
    companyNumber: CompanyProfile.Request
  )(implicit hc: HeaderCarrier): F[ServiceCallResponse[CompanyProfile.Response]]
}

class CompanyInformationAsyncConnector(ws: WSHttp, baseUrl: String)(implicit ex: ExecutionContext)
    extends CompanyInformationConnector[Future] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  override def companyProfile(
    companyNumber: CompanyProfile.Request
  )(implicit hc: HeaderCarrier): Future[ServiceCallResponse[CompanyProfile.Response]] = {
    val url = s"$baseUrl/companies-house-api-proxy/company/${companyNumber.companyNumber}"

    ws.GET[CompanyProfile.Response](url)
      .map { response =>
        ServiceResponse(response)
      }
      .recover { case ex =>
        logger.error("Unknown problem when calling company profile", ex)
        CannotRetrieveResponse
      }
  }
}

object CompanyProfile {
  case class Request(
    companyNumber: String
  )

  def create(companyNumber: String) = Request(companyNumber)

  def createFullRegisteredAddress(r: RegisteredAddress) =
    s"${r.addressLine1} ${r.addressLine2.getOrElse("")} ${r.postalCode} ${r.locality} ${r.region.getOrElse("")}"

  object Request {
    implicit val format: Format[Request] = Json.format[Request]
  }

  case class RegisteredAddress(
    addressLine1: String,
    addressLine2: Option[String],
    postalCode: String,
    locality: String,
    region: Option[String]
  )

  object RegisteredAddress {
    private val apiReads: Reads[RegisteredAddress] =
      ((__ \ "address_line_1").read[String] and
        (__ \ "address_line_2").readNullable[String] and
        (__ \ "postal_code").read[String] and
        (__ \ "locality").read[String] and
        (__ \ "region").readNullable[String])(RegisteredAddress.apply _)

    private val apiWrites: Writes[RegisteredAddress] =
      ((__ \ "address_line_1").write[String] and
        (__ \ "address_line_2").writeNullable[String] and
        (__ \ "postal_code").write[String] and
        (__ \ "locality").write[String] and
        (__ \ "region").writeNullable[String])(unlift(RegisteredAddress.unapply))

    implicit val format: Format[RegisteredAddress] = Format(apiReads, apiWrites)
  }

  case class Response(
    name: String,
    status: Option[String],
    registeredAddress: RegisteredAddress
  )

  object Response {

    private val apiReads: Reads[Response] =
      ((__ \ "company_name").read[String] and
        (__ \ "company_status").readNullable[String] and
        (__ \ "registered_office_address").read[RegisteredAddress])(Response.apply _)

    private val apiWrites: Writes[Response] =
      ((__ \ "company_name").write[String] and
        (__ \ "company_status").writeNullable[String] and
        (__ \ "registered_office_address").write[RegisteredAddress])(unlift(Response.unapply))

    implicit val format: Format[Response] = Format(apiReads, apiWrites)
  }
}