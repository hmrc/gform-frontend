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

package uk.gov.hmrc.gform.gform

import cats.syntax.all._
import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.api.{ BankAccountInsightsConnector, CompanyInformationConnector, DelegatedAgentAuthConnector, HipConnector, NinoInsightsConnector }
import uk.gov.hmrc.gform.bars.BankAccountReputationConnector
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

object DataRetrieveService {

  def retrieveDataResult(
    dataRetrieve: DataRetrieve,
    form: Option[Form],
    request: DataRetrieve.Request,
    bankAccountReputationConnector: Option[BankAccountReputationConnector[Future]],
    companyInformationConnector: Option[CompanyInformationConnector[Future]],
    ninoInsightsConnector: Option[NinoInsightsConnector[Future]],
    bankAccountInsightConnector: Option[BankAccountInsightsConnector[Future]],
    gformConnector: Option[GformConnector],
    fileSystemConnector: Option[FileSystemConnector],
    delegatedAgentAuthConnector: Option[DelegatedAgentAuthConnector[Future]],
    hipConnector: Option[HipConnector[Future]]
  )(implicit ex: ExecutionContext, hc: HeaderCarrier): Future[Option[DataRetrieveResult]] = {
    val maybeRequestParams = form.flatMap(f => DataRetrieve.requestParamsFromCache(f, dataRetrieve.id))
    val maybeExecutor
      : Option[(DataRetrieve, DataRetrieve.Request) => Future[ServiceCallResponse[DataRetrieve.Response]]] =
      dataRetrieve.tpe match {
        case DataRetrieve.Type("validateBankDetails") => bankAccountReputationConnector.map(_.validateBankDetails)
        case DataRetrieve.Type("businessBankAccountExistence") =>
          bankAccountReputationConnector.map(_.businessBankAccountExistence)
        case DataRetrieve.Type("personalBankAccountExistence") =>
          bankAccountReputationConnector.map(_.personalBankAccountExistence)
        case DataRetrieve.Type("personalBankAccountExistenceWithName") =>
          bankAccountReputationConnector.map(_.personalBankAccountExistence)
        case DataRetrieve.Type("companyHouseProfile")        => companyInformationConnector.map(_.companyProfile)
        case DataRetrieve.Type("companyHouseActiveOfficers") => companyInformationConnector.map(_.companyOfficers)
        case DataRetrieve.Type("ninoInsights")               => ninoInsightsConnector.map(_.insights)
        case DataRetrieve.Type("bankAccountInsights")        => bankAccountInsightConnector.map(_.insights)
        case DataRetrieve.Type("employments")                => hipConnector.map(_.employmentSummary)
        case DataRetrieve.Type("hmrcRosmRegistrationCheck")  => gformConnector.map(_.getDesOrganisation)
        case DataRetrieve.Type("agentDetails")               => gformConnector.map(_.getDesAgentDetails)
        case DataRetrieve.Type("hmrcTaxRates")               => fileSystemConnector.map(_.getHmrcTaxRate)
        case DataRetrieve.Type("delegatedAgentAuthVat")      => delegatedAgentAuthConnector.map(_.mtdVatAuth)
        case DataRetrieve.Type("delegatedAgentAuthPaye")     => delegatedAgentAuthConnector.map(_.payeAuth)
        case _                                               => Option.empty
      }
    maybeExecutor.flatTraverse { executor =>
      DataRetrieveService
        .retrieveData(
          dataRetrieve,
          request,
          maybeRequestParams,
          executor
        )
    }
  }

  private def retrieveData(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request,
    maybeRequestParams: Option[JsValue],
    executor: (DataRetrieve, DataRetrieve.Request) => Future[ServiceCallResponse[DataRetrieve.Response]]
  )(implicit ex: ExecutionContext): Future[Option[DataRetrieveResult]] = {

    val requestParams = request.paramsAsJson()

    if (request.notReady() || maybeRequestParams.contains(requestParams)) {
      Future.successful(None)
    } else {

      executor(dataRetrieve, request).map {
        case ServiceResponse(result) =>
          Some(
            DataRetrieveResult(
              dataRetrieve.id,
              result.toRetrieveDataType(),
              requestParams
            )
          )
        // We need to fail the journey here, otherwise expressions like
        // ${dataRetrieve.bankDetails.isValid='no'} etc.
        // will always evaluate to false
        case CannotRetrieveResponse | NotFound => throw new Exception("Cannot retrieve data")
      }
    }
  }

}
