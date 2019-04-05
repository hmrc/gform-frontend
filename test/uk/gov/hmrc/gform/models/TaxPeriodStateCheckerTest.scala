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

import cats.Id
import cats.data.NonEmptyList
import cats.implicits._
import org.scalatest.Assertion
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.models.gform.{ NewUser, ReturningUser }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class TaxPeriodStateCheckerTest extends Spec {

  it should "trigger a call to DES if tax period can be evaluated and needs to be refreshed" in
    new TaxPeriodStateChecker[Id] {
      override val needRefresh
        : (Map[RecalculatedTaxPeriodKey, IdNumberValue], Map[RecalculatedTaxPeriodKey, IdNumberValue]) => Boolean =
        (_, _) => true

      withFixture { (state, evaluatedTaxPeriod, desResponse, obligations) =>
        val getAllTaxPeriods: NonEmptyList[HmrcTaxPeriodWithEvaluatedId] => Id[NonEmptyList[TaxResponse]] =
          _ => desResponse.pure[Id]

        callDesIfNeeded(getAllTaxPeriods, Some(evaluatedTaxPeriod), obligations, state, Map.empty, NewUser) should be(
          RetrievedObligations(desResponse)
        )
      }
    }

  it should "trigger a call to DES if obligations are NotChecked" in
    new TaxPeriodStateChecker[Id] {
      override val needRefresh
        : (Map[RecalculatedTaxPeriodKey, IdNumberValue], Map[RecalculatedTaxPeriodKey, IdNumberValue]) => Boolean =
        (_, _) => false

      withFixture { (state, evaluatedTaxPeriod, desResponse, _) =>
        val getAllTaxPeriods: NonEmptyList[HmrcTaxPeriodWithEvaluatedId] => Id[NonEmptyList[TaxResponse]] =
          _ => desResponse.pure[Id]

        callDesIfNeeded(getAllTaxPeriods, Some(evaluatedTaxPeriod), NotChecked, state, Map.empty, NewUser) should be(
          RetrievedObligations(desResponse)
        )
      }
    }

  it should "do not trigger a call to DES if tax period can be evaluated but do not need to be refreshed" in
    new TaxPeriodStateChecker[Id] {
      override val needRefresh
        : (Map[RecalculatedTaxPeriodKey, IdNumberValue], Map[RecalculatedTaxPeriodKey, IdNumberValue]) => Boolean =
        (_, _) => false

      withFixture { (state, evaluatedTaxPeriod, desResponse, obligations) =>
        val getAllTaxPeriods: NonEmptyList[HmrcTaxPeriodWithEvaluatedId] => Id[NonEmptyList[TaxResponse]] =
          _ => desResponse.pure[Id]

        callDesIfNeeded(getAllTaxPeriods, Some(evaluatedTaxPeriod), obligations, state, Map.empty, NewUser) should be(
          obligations
        )
      }
    }

  it should "do not call DES if tax period can not be evaluated" in new TaxPeriodStateChecker[Id] {
    override val needRefresh
      : (Map[RecalculatedTaxPeriodKey, IdNumberValue], Map[RecalculatedTaxPeriodKey, IdNumberValue]) => Boolean =
      (_, _) => true

    withFixture { (state, _, desResponse, obligations) =>
      val getAllTaxPeriods: NonEmptyList[HmrcTaxPeriodWithEvaluatedId] => Id[NonEmptyList[TaxResponse]] =
        _ => desResponse.pure[Id]

      callDesIfNeeded(getAllTaxPeriods, None, obligations, state, Map.empty, ReturningUser) should be(obligations)
    }
  }

  it should "check if new state differ from old state" in new TaxPeriodStateChecker[Id] {
    withFixture { (state, _, _, _) =>
      needRefresh(state, state) should be(false)
      needRefresh(state, state.map { case (periodKey, _) => periodKey -> IdNumberValue("666") }) should be(true)
      needRefresh(state, Map.empty) should be(true)
    }
  }

  private def withFixture(
    fn: (
      Map[RecalculatedTaxPeriodKey, IdNumberValue],
      NonEmptyList[HmrcTaxPeriodWithEvaluatedId],
      NonEmptyList[TaxResponse],
      RetrievedObligations) => Assertion): Unit = {
    val taxPeriod = HmrcTaxPeriod(IdType("id"), FormCtx("ctx"), RegimeType("AAA"))
    val periodKey = RecalculatedTaxPeriodKey(FormComponentId("idOne"), taxPeriod)
    val idNumber1 = IdNumberValue("111")
    val idNumber2 = IdNumberValue("222")

    val recalculatedTaxPeriod: RecalculatedTaxPeriodKey =
      RecalculatedTaxPeriodKey(FormComponentId("123"), taxPeriod)

    val desResponse =
      NonEmptyList.one(TaxResponse(HmrcTaxPeriodWithEvaluatedId(recalculatedTaxPeriod, idNumber1), Obligation(Nil)))
    val desResponse2 =
      NonEmptyList.one(TaxResponse(HmrcTaxPeriodWithEvaluatedId(recalculatedTaxPeriod, idNumber2), Obligation(Nil)))

    val state: Map[RecalculatedTaxPeriodKey, IdNumberValue] = Map(periodKey -> idNumber1)

    val evaluatedTaxPeriod: NonEmptyList[HmrcTaxPeriodWithEvaluatedId] =
      NonEmptyList.one(HmrcTaxPeriodWithEvaluatedId(recalculatedTaxPeriod, idNumber1))

    fn(state, evaluatedTaxPeriod, desResponse, RetrievedObligations(desResponse2))
  }
}
