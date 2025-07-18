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

import cats.implicits.catsSyntaxEq
import org.slf4j.{ Logger, LoggerFactory }
import play.api.http.Status
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, DataRetrieve, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, StringContextOps }

import java.time.LocalDate
import scala.concurrent.{ ExecutionContext, Future }

trait CompanyInformationConnector[F[_]] {
  def companyProfile(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): F[ServiceCallResponse[DataRetrieve.Response]]

  def companyOfficers(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): F[ServiceCallResponse[DataRetrieve.Response]]

  def companyInsolvency(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): F[ServiceCallResponse[DataRetrieve.Response]]
}

class CompanyInformationAsyncConnector(httpClient: HttpClientV2, baseUrl: String)(implicit ex: ExecutionContext)
    extends CompanyInformationConnector[Future] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private val profileUrlWithPlaceholders = s"$baseUrl/companieshouse/company/{{companyNumber}}"
  private val profileIdentifier = "company profile"
  private val officersUrlWithPlaceholders = s"$baseUrl/companieshouse/company/{{companyNumber}}/officers"
  private val officersIdentifier = "company officers"
  private val insolvencyUrlWithPlaceholders = s"$baseUrl/companieshouse/company/{{companyNumber}}/insolvency"
  private val insolvencyIdentifier = "company insolvency details"

  override def companyInsolvency(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): Future[ServiceCallResponse[DataRetrieve.Response]] = {
    val url = request.fillPlaceholders(insolvencyUrlWithPlaceholders)

    httpClient
      .get(url"$url")
      .execute[HttpResponse]
      .map { httpResponse =>
        httpResponse.status match {
          case Status.OK =>
            transformInsolvencyResponse(httpResponse.json)
              .fold(
                invalid => {
                  logger.error(
                    s"Calling $insolvencyIdentifier returned successfully, but marshalling of data failed with: $invalid"
                  )
                  CannotRetrieveResponse
                },
                validResponse =>
                  dataRetrieve
                    .processResponse(validResponse)
                    .fold(
                      invalid => {
                        logger.error(
                          s"Calling internal $insolvencyIdentifier returned successfully, but marshalling of data failed with: $invalid"
                        )
                        CannotRetrieveResponse
                      },
                      valid => {
                        logger.info(s"Calling $insolvencyIdentifier returned Success.")
                        ServiceResponse(valid)
                      }
                    )
              )
          case Status.NOT_FOUND =>
            logger.info(s"Calling $insolvencyIdentifier returned successfully, but none were found: $httpResponse")
            ServiceResponse[DataRetrieve.Response](DataRetrieve.Response.Array(List.empty))
          case other =>
            logger.error(s"Problem when calling $insolvencyIdentifier. Http status: $other, body: ${httpResponse.body}")
            CannotRetrieveResponse
        }
      }
      .recover { ex =>
        logger.error(s"Unknown problem when calling $insolvencyIdentifier", ex)
        CannotRetrieveResponse
      }
  }

  private def transformInsolvencyResponse(json: JsValue): Either[String, JsValue] =
    json.validate[Insolvency] match {
      case JsSuccess(insolvency, _) =>
        val practitioners = insolvency.cases.flatMap { insolvencyCase =>
          val today = LocalDate.now
          insolvencyCase.practitioners
            .filter(_.ceased_to_act_on.fold(true)(_.isAfter(today)))
            .map { practitioner =>
              Json.obj(
                "caseNumber"  -> JsString(insolvencyCase.number),
                "caseType"    -> JsString(insolvencyCase.`type`),
                "name"        -> JsString(practitioner.name),
                "role"        -> JsString(practitioner.role),
                "address"     -> Json.toJson(practitioner.address),
                "appointedOn" -> JsString(practitioner.appointed_on.map(_.toString).getOrElse(""))
              )
            }
        }
        Right(Json.toJson(practitioners))

      case JsError(unexpected) => Left(s"Expected response for $unexpected")
    }

  override def companyProfile(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): Future[ServiceCallResponse[DataRetrieve.Response]] = {
    val url = request.fillPlaceholders(profileUrlWithPlaceholders)

    httpClient
      .get(url"$url")
      .execute[HttpResponse]
      .map { httpResponse =>
        httpResponse.status match {
          case Status.OK =>
            dataRetrieve
              .processResponse(httpResponse.json)
              .fold(
                invalid => {
                  logger.error(
                    s"Calling $profileIdentifier returned successfully, but marshalling of data failed with: $invalid"
                  )
                  CannotRetrieveResponse
                },
                valid => {
                  logger.info(s"Calling $profileIdentifier returned Success.")
                  ServiceResponse(valid)
                }
              )
          case Status.NOT_FOUND =>
            logger.info(s"Calling $profileIdentifier returned successfully, but no company was found: $httpResponse")
            ServiceResponse[DataRetrieve.Response](DataRetrieve.Response.Object(Map.empty))
          case other =>
            logger.error(s"Problem when calling $profileIdentifier. Http status: $other, body: ${httpResponse.body}")
            CannotRetrieveResponse
        }
      }
      .recover { ex =>
        logger.error(s"Unknown problem when calling $profileIdentifier", ex)
        CannotRetrieveResponse
      }
  }

  override def companyOfficers(dataRetrieve: DataRetrieve, request: DataRetrieve.Request)(implicit
    hc: HeaderCarrier
  ): Future[ServiceCallResponse[DataRetrieve.Response]] = {
    val url = request.fillPlaceholders(officersUrlWithPlaceholders)

    httpClient
      .get(url"$url")
      .execute[HttpResponse]
      .map { httpResponse =>
        httpResponse.status match {
          case Status.OK =>
            processOfficersResponse(httpResponse.json)
              .fold(
                invalid => {
                  logger.error(
                    s"Calling $officersIdentifier returned successfully, but marshalling of data failed with: $invalid"
                  )
                  CannotRetrieveResponse
                },
                validResponse =>
                  dataRetrieve
                    .processResponse(validResponse)
                    .fold(
                      invalid => {
                        logger.error(
                          s"Calling internal $officersIdentifier returned successfully, but marshalling of data failed with: $invalid"
                        )
                        CannotRetrieveResponse
                      },
                      valid => {
                        logger.info(s"Calling $officersIdentifier returned Success.")
                        ServiceResponse(valid)
                      }
                    )
              )
          case Status.NOT_FOUND =>
            logger.info(s"Calling $officersIdentifier returned successfully, but no company was found: $httpResponse")
            ServiceResponse[DataRetrieve.Response](DataRetrieve.Response.Object(Map.empty))
          case other =>
            logger.error(s"Problem when calling $officersIdentifier. Http status: $other, body: ${httpResponse.body}")
            CannotRetrieveResponse
        }
      }
      .recover { ex =>
        logger.error(s"Unknown problem when calling $officersIdentifier", ex)
        CannotRetrieveResponse
      }
  }

  private def processOfficersResponse(json: JsValue): JsResult[JsObject] =
    json.validate[Officers] match {
      case JsSuccess(officers, _) =>
        val activeOfficers = officers.items.filterNot(_.resignedOn.nonEmpty)
        val activeDirectors = activeOfficers.count(_.officerRole === "director")
        val activeSecretaries = activeOfficers.count(_.officerRole === "secretary")
        val activeLlpMembers = activeOfficers.count(_.officerRole === "llp-member")
        val result = Json.obj(
          "active_directors"   -> JsNumber(activeDirectors),
          "active_secretaries" -> JsNumber(activeSecretaries),
          "active_llp_members" -> JsNumber(activeLlpMembers)
        )

        JsSuccess(result)
      case unexpected => JsError(s"Expected array response for $unexpected")
    }

}
