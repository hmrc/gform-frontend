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
import play.api.libs.json.{ JsString, Json }
import uk.gov.hmrc.auth.core.{ AuthConnector => _, _ }
import uk.gov.hmrc.domain.EmpRef
import uk.gov.hmrc.gform.auth.AuthConnector
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, DataRetrieve, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

trait DelegatedAgentAuthConnector[F[_]] {
  def mtdVatAuth(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): F[ServiceCallResponse[DataRetrieve.Response]]

  def payeAuth(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): F[ServiceCallResponse[DataRetrieve.Response]]
}

case class DelegatedAuth(rule: String, parameter: String)

class DelegatedAgentAuthAsyncConnector(
  ws: WSHttp,
  baseUrl: String,
  val authConnector: AuthConnector
)(implicit
  ex: ExecutionContext
) extends DelegatedAgentAuthConnector[Future] with AuthorisedFunctions {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private val JSON_AUTH_KEY = "authorised"

  private val mtdVat = DelegatedAuth("mtd-vat-auth", "vatRegistrationNumber")
  private val epaye = DelegatedAuth("epaye-auth", "payeReference")

  override def mtdVatAuth(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): Future[ServiceCallResponse[DataRetrieve.Response]] = {

    val vrn: String = getParam(request, mtdVat.parameter)

    val authCheck: Future[Boolean] = authorised(
      Enrolment("HMRC-MTD-VAT")
        .withIdentifier("VRN", vrn)
        .withDelegatedAuthRule(mtdVat.rule)
    )(Future.successful(true))

    processAuthCheck(dataRetrieve, mtdVat.rule, authCheck)
  }

  override def payeAuth(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): Future[ServiceCallResponse[DataRetrieve.Response]] = {

    val ref: EmpRef = EmpRef.fromIdentifiers(getParam(request, epaye.parameter))

    val authCheck: Future[Boolean] = authorised(
      Enrolment("IR-PAYE")
        .withIdentifier("TaxOfficeNumber", ref.taxOfficeNumber)
        .withIdentifier("TaxOfficeReference", ref.taxOfficeReference)
        .withDelegatedAuthRule(epaye.rule)
    )(Future.successful(true))

    processAuthCheck(dataRetrieve, epaye.rule, authCheck)
  }

  private def getParam(request: DataRetrieve.Request, param: String): String =
    request.params.find(_._1 === param).map(t => t._2).getOrElse("")

  private def processAuthCheck(
    dataRetrieve: DataRetrieve,
    identifier: String,
    authCheck: Future[Boolean]
  ): Future[ServiceCallResponse[DataRetrieve.Response]] = authCheck
    .flatMap(authorised => Future.successful(processResponse(dataRetrieve, identifier, authorised)))
    .recover { case _ =>
      processResponse(dataRetrieve, identifier, authorised = false)
    }

  private def processResponse(
    dataRetrieve: DataRetrieve,
    identifier: String,
    authorised: Boolean
  ): ServiceCallResponse[DataRetrieve.Response] = dataRetrieve
    .processResponse(Json.obj((JSON_AUTH_KEY, JsString(s"$authorised"))))
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
