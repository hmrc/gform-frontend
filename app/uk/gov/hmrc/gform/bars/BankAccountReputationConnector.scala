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

package uk.gov.hmrc.gform.bars

import cats.implicits.catsSyntaxEq
import uk.gov.hmrc.gform.gform.{ DataRetrieveConnectorBlueprint, ExceptionalResponse }
import uk.gov.hmrc.gform.sharedmodel.RetrieveDataType.{ ListType, ObjectType }
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, ServiceCallResponse }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.client.HttpClientV2

import scala.concurrent.{ ExecutionContext, Future }

trait BankAccountReputationConnector[F[_]] {
  def validateBankDetails(dataRetrieve: DataRetrieve, request: DataRetrieve.Request)(implicit
    hc: HeaderCarrier
  ): F[ServiceCallResponse[DataRetrieve.Response]]

  def businessBankAccountExistence(dataRetrieve: DataRetrieve, request: DataRetrieve.Request)(implicit
    hc: HeaderCarrier
  ): F[ServiceCallResponse[DataRetrieve.Response]]

  def personalBankAccountExistence(dataRetrieve: DataRetrieve, request: DataRetrieve.Request)(implicit
    hc: HeaderCarrier
  ): F[ServiceCallResponse[DataRetrieve.Response]]

  def isFailure(response: DataRetrieve.Response): Boolean
}

class BankAccountReputationAsyncConnector(httpClient: HttpClientV2, baseUrl: String)(implicit ex: ExecutionContext)
    extends BankAccountReputationConnector[Future] {

  val validateBankDetailsExceptionalResponses = Some(
    List(
      ExceptionalResponse(
        400,
        "SORT_CODE_ON_DENY_LIST",
        """{"accountNumberIsWellFormatted":"indeterminate","nonStandardAccountDetailsRequiredForBacs":"inapplicable","sortCodeIsPresentOnEISCD":"no","sortCodeBankName":"","sortCodeSupportsDirectDebit":"no","sortCodeSupportsDirectCredit":"no","iban":""}"""
      )
    )
  )

  val businessAndPersonalExceptionalResponses = Some(
    List(
      ExceptionalResponse(
        400,
        "SORT_CODE_ON_DENY_LIST",
        """{"accountNumberIsWellFormatted":"indeterminate","accountExists":"indeterminate","nameMatches":"indeterminate","nonStandardAccountDetailsRequiredForBacs":"inapplicable","sortCodeIsPresentOnEISCD":"no","sortCodeBankName":"","sortCodeSupportsDirectDebit":"no","sortCodeSupportsDirectCredit":"no","iban":""}"""
      )
    )
  )

  val validateBankDetailsB =
    new DataRetrieveConnectorBlueprint(
      httpClient,
      baseUrl + "/validate/bank-details",
      "validate bank details",
      validateBankDetailsExceptionalResponses,
      enableResponseValidation = true
    )
  val businessBankAccountExistenceB =
    new DataRetrieveConnectorBlueprint(
      httpClient,
      baseUrl + "/verify/business",
      "business bank account existence",
      businessAndPersonalExceptionalResponses,
      enableResponseValidation = true
    )
  val personalBankAccountExistenceB =
    new DataRetrieveConnectorBlueprint(
      httpClient,
      baseUrl + "/verify/personal",
      "personal bank account existence",
      businessAndPersonalExceptionalResponses,
      enableResponseValidation = true
    )

  override def validateBankDetails(dataRetrieve: DataRetrieve, request: DataRetrieve.Request)(implicit
    hc: HeaderCarrier
  ): Future[ServiceCallResponse[DataRetrieve.Response]] =
    validateBankDetailsB.post(dataRetrieve, request)

  override def businessBankAccountExistence(dataRetrieve: DataRetrieve, request: DataRetrieve.Request)(implicit
    hc: HeaderCarrier
  ): Future[ServiceCallResponse[DataRetrieve.Response]] =
    businessBankAccountExistenceB.post(dataRetrieve, request)

  override def personalBankAccountExistence(dataRetrieve: DataRetrieve, request: DataRetrieve.Request)(implicit
    hc: HeaderCarrier
  ): Future[ServiceCallResponse[DataRetrieve.Response]] =
    personalBankAccountExistenceB.post(dataRetrieve, request)

  override def isFailure(response: DataRetrieve.Response): Boolean =
    response.toRetrieveDataType() match {
      case ObjectType(data) =>
        val accountNumWellFormattedValue = data.getOrElse(DataRetrieve.Attribute("accountNumberIsWellFormatted"), "")
        val accountExistsValue = data.getOrElse(DataRetrieve.Attribute("accountExists"), "")
        val nameMatchesValue = data.getOrElse(DataRetrieve.Attribute("nameMatches"), "")

        val commonPredicate = accountNumWellFormattedValue =!= "no"
        val failureCheck1 = commonPredicate && accountExistsValue === "inapplicable"
        val failureCheck2 =
          commonPredicate && accountExistsValue === "yes" && nameMatchesValue =!= "yes" && nameMatchesValue =!= "partial"

        failureCheck1 || failureCheck2
      case ListType(_) => throw new IllegalStateException("List type is illegal for BARs check response")
    }
}
