/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import java.time.LocalDate

import com.softwaremill.quicklens._
import uk.gov.hmrc.gform.graph.Data
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormCtx, HmrcTaxPeriod, IdType, RegimeType }

trait ObligationValidatorTestFixture {

  val formComponentId = FormComponentId("1")
  val cachedData: Data = Map(formComponentId -> Seq("16AZ"), FormComponentId("2") -> Seq("17BB"))
  val taxPeriod = HmrcTaxPeriod(IdType("id"), FormCtx("ctx"), RegimeType("AAA"))
  val recalculatedTaxPeriodKey: RecalculatedTaxPeriodKey = RecalculatedTaxPeriodKey(formComponentId, taxPeriod)
  val date = LocalDate.now
  val detailOne = ObligationDetail("0", date, date, date, "15BB")
  val detailTwo = ObligationDetail("0", date, date, date, "16AZ")
  val detailThree = ObligationDetail("0", date, date, date, "17AZ")
  val details: List[ObligationDetail] = detailOne :: detailTwo :: Nil
  val obligationDetails: List[ObligationDetails] = ObligationDetails(details) :: Nil
  val obligations = Obligation(obligationDetails)

  val taxResponse =
    TaxResponse(HmrcTaxPeriodWithEvaluatedId(recalculatedTaxPeriodKey, IdNumberValue("1")), obligations)

  val moreObligationsAvailable =
    obligations.modify(_.obligations).setTo(List(ObligationDetails(List(detailOne, detailTwo, detailThree))))
  val lessObligationsUnavailable =
    obligations.modify(_.obligations).setTo(List(ObligationDetails(List(detailOne))))
  val lessObligationsAvailable = obligations.modify(_.obligations).setTo(List(ObligationDetails(List(detailTwo))))

  val differentObligationsAvailable =
    obligations
      .modify(_.obligations)
      .setTo(
        obligationDetails.tail ::: ObligationDetails(ObligationDetail("0", date, date, date, "different") :: Nil) :: Nil)

  implicit class ResponseWithObligations(taxResponse: TaxResponse) {
    def withObligations(obligation: Obligation): TaxResponse =
      taxResponse.modify(_.obligation).setTo(obligation)
  }
}
