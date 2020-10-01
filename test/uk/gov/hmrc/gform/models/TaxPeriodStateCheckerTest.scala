/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.data.NonEmptyList
import cats.implicits._
import java.time.LocalDate
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.models.gform.{ ForceReload, NoSpecificAction }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class TaxPeriodStateCheckerTest extends Spec {

  type EitherEffect[A] = Either[Exception, A]

  val obligationError = new Exception("Obligation retrieval failed")

  val taxPeriod = HmrcTaxPeriod(IdType("id"), FormCtx(FormComponentId("ctx")), RegimeType("AAA"))
  val localDate = LocalDate.of(2011, 1, 1)
  val obligationDetailA = ObligationDetail("O", localDate, localDate, localDate, "A")
  val obligationDetailB = ObligationDetail("O", localDate, localDate, localDate, "B")

  val idNumber1 = IdNumberValue("111")
  val idNumber2 = IdNumberValue("222")

  val recalculatedTaxPeriod: RecalculatedTaxPeriodKey =
    RecalculatedTaxPeriodKey(FormComponentId("123"), taxPeriod)

  val mongo = HmrcTaxPeriodWithEvaluatedId(recalculatedTaxPeriod, idNumber1)
  val browser = HmrcTaxPeriodWithEvaluatedId(recalculatedTaxPeriod, idNumber2)

  val mongoDesResponse =
    NonEmptyList.one(TaxResponse(mongo, Obligation(List(ObligationDetails(List(obligationDetailA))))))
  val desData = NonEmptyList.one(TaxResponse(browser, Obligation(List(ObligationDetails(List(obligationDetailB))))))

  val mongoTaxPeriod: NonEmptyList[HmrcTaxPeriodWithEvaluatedId] =
    NonEmptyList.one(mongo)
  val browserTaxPeriod: NonEmptyList[HmrcTaxPeriodWithEvaluatedId] =
    NonEmptyList.one(browser)

  val getAllTaxPeriods
    : NonEmptyList[HmrcTaxPeriodWithEvaluatedId] => EitherEffect[NonEmptyList[ServiceCallResponse[TaxResponse]]] =
    _ => {
      val desResponse: NonEmptyList[ServiceCallResponse[TaxResponse]] = desData.map(ServiceResponse(_))
      desResponse.pure[EitherEffect]
    }

  val desObligations = RetrievedObligations(desData)
  val mongoObligations = RetrievedObligations(mongoDesResponse)

  new TaxPeriodStateChecker[EitherEffect, Exception] {

    def error = obligationError

    it should "trigger a call to DES if IdNumberValue changes" in {
      callDesIfNeeded(getAllTaxPeriods, Some(browserTaxPeriod), mongoObligations, NoSpecificAction) should be(
        Right(desObligations)
      )

      callDesIfNeeded(getAllTaxPeriods, Some(browserTaxPeriod), NotChecked, NoSpecificAction) should be(
        Right(desObligations)
      )
    }

    it should "return cached obligations if evaluatedTaxPeriod doesn't exists (is None)" in {
      callDesIfNeeded(getAllTaxPeriods, None, NotChecked, NoSpecificAction) should be(
        Right(NotChecked)
      )

      callDesIfNeeded(getAllTaxPeriods, None, mongoObligations, NoSpecificAction) should be(
        Right(mongoObligations)
      )
    }

    it should "trigger a call to DES if ForceReload is specified" in {
      callDesIfNeeded(getAllTaxPeriods, Some(mongoTaxPeriod), mongoObligations, NoSpecificAction) should be(
        Right(mongoObligations)
      )

      callDesIfNeeded(getAllTaxPeriods, Some(mongoTaxPeriod), mongoObligations, ForceReload) should be(
        Right(desObligations)
      )
    }
  }
}
