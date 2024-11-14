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

package uk.gov.hmrc.gform.eval

import java.time.LocalDate
import org.scalatest.prop.TableDrivenPropertyChecks
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.eval.DateExprEval.{ evalDateConstructExpr, evalDateExpr }
import uk.gov.hmrc.gform.eval.ExpressionResult.{ AddressResult, DateResult, NumberResult, OptionResult, StringResult }
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Date, DateExpr, DateExprValue, DateExprWithOffset, DateFormCtxVar, DateValueExpr, ExactDateExprValue, FormComponentId, FormCtx, OffsetUnit, OffsetYMD, TodayDateExprValue }
import uk.gov.hmrc.gform.sharedmodel.{ ExampleEvaluationContext, VariadicFormData, VariadicValue }

class DateExprEvalSpec extends Spec with TableDrivenPropertyChecks with ExampleEvaluationContext {

  private val booleanExprResolver = BooleanExprResolver(_ => false)
  "evalDateExpr" should "evaluate a date expression with TodayDateExprValue" in {
    val expressionResult =
      evalDateExpr(
        RecData[OutOfDate](VariadicFormData.empty),
        evaluationContext,
        EvaluationResults.empty,
        booleanExprResolver
      )(
        DateValueExpr(TodayDateExprValue)
      )
    expressionResult shouldBe DateResult(LocalDate.now())
  }

  it should "evaluate a date expression with ExactDateExprValue" in {
    val expressionResult =
      evalDateExpr(
        RecData[OutOfDate](VariadicFormData.empty),
        evaluationContext,
        EvaluationResults.empty,
        booleanExprResolver
      )(
        DateValueExpr(
          ExactDateExprValue(LocalDate.now().getYear, LocalDate.now().getMonthValue, LocalDate.now().getDayOfMonth)
        )
      )
    expressionResult shouldBe DateResult(LocalDate.now())
  }

  it should "evaluate a date expression with form field, from RecData" in {
    val dateField = FormComponentId("dateFieldId")
    val expressionResult = evalDateExpr(
      RecData[OutOfDate](
        VariadicFormData.create(
          (dateField.toAtomicFormComponentId(Date.year), VariadicValue.One("1970")),
          (dateField.toAtomicFormComponentId(Date.month), VariadicValue.One("1")),
          (dateField.toAtomicFormComponentId(Date.day), VariadicValue.One("11"))
        )
      ),
      evaluationContext,
      EvaluationResults.empty,
      booleanExprResolver
    )(DateFormCtxVar(FormCtx(dateField)))
    expressionResult shouldBe DateResult(LocalDate.of(1970, 1, 11))
  }

  it should "evaluate a date expression with form field, from EvaluationResults" in {
    val dateField = FormComponentId("dateFieldId")
    val expressionResult = evalDateExpr(
      RecData[OutOfDate](VariadicFormData.empty),
      evaluationContext,
      EvaluationResults.one(FormCtx(dateField), DateResult(LocalDate.now())),
      booleanExprResolver
    )(DateFormCtxVar(FormCtx(dateField)))
    expressionResult shouldBe DateResult(LocalDate.now())
  }

  it should "evaluate a date expression with offset" in {
    val table = Table(
      ("actual", "expected"),
      (OffsetYMD(OffsetUnit.Day(1) :: Nil), LocalDate.now().plusDays(1)),
      (OffsetYMD(OffsetUnit.Month(1) :: Nil), LocalDate.now().plusMonths(1)),
      (OffsetYMD(OffsetUnit.Year(1) :: Nil), LocalDate.now().plusYears(1)),
      (OffsetYMD(OffsetUnit.Year(1) :: OffsetUnit.Month(1) :: Nil), LocalDate.now().plusYears(1).plusMonths(1))
    )
    forAll(table) { (offset: OffsetYMD, localDate: LocalDate) =>
      val actual = evalDateExpr(
        RecData[OutOfDate](VariadicFormData.empty),
        evaluationContext,
        EvaluationResults.empty,
        booleanExprResolver
      )(DateExprWithOffset(DateValueExpr(TodayDateExprValue), offset))

      val expected = DateResult(localDate)
      actual shouldBe expected
    }
  }

  it should "evaluate a date expression with multiple offsets (offsets are not commutative)" in {
    val table = Table(
      ("exactDate", "actual", "localData"),
      (
        ExactDateExprValue(2020, 5, 31),
        OffsetYMD(OffsetUnit.Day(-1) :: OffsetUnit.Month(-1) :: Nil),
        LocalDate.of(2020, 4, 30)
      ),
      (
        ExactDateExprValue(2020, 5, 31),
        OffsetYMD(OffsetUnit.Month(-1) :: OffsetUnit.Day(-1) :: Nil),
        LocalDate.of(2020, 4, 29)
      ),
      (
        ExactDateExprValue(2020, 4, 30),
        OffsetYMD(OffsetUnit.Month(1) :: OffsetUnit.Day(1) :: Nil),
        LocalDate.of(2020, 5, 31)
      ),
      (
        ExactDateExprValue(2020, 4, 30),
        OffsetYMD(OffsetUnit.Day(1) :: OffsetUnit.Month(1) :: Nil),
        LocalDate.of(2020, 6, 1)
      )
    )
    forAll(table) { (exactDate: DateExprValue, offset: OffsetYMD, localDate: LocalDate) =>
      val actual = evalDateExpr(
        RecData[OutOfDate](VariadicFormData.empty),
        evaluationContext,
        EvaluationResults.empty,
        booleanExprResolver
      )(DateExprWithOffset(DateValueExpr(exactDate), offset))

      val expected = DateResult(localDate)
      actual shouldBe expected
    }
  }

  it should "evaluate a date construct function" in {
    val table = Table(
      ("dayMonthExpr", "yearResult", "expected"),
      (
        DateValueExpr(ExactDateExprValue(1900, 2, 1)),
        DateResult(LocalDate.of(2024, 9, 10)),
        DateResult(LocalDate.of(2024, 2, 1))
      ),
      (DateValueExpr(ExactDateExprValue(1900, 2, 1)), StringResult("2024"), DateResult(LocalDate.of(2024, 2, 1))),
      (DateValueExpr(ExactDateExprValue(1900, 2, 1)), NumberResult(2024), DateResult(LocalDate.of(2024, 2, 1))),
      (DateValueExpr(ExactDateExprValue(1900, 2, 1)), OptionResult(List("2024")), DateResult(LocalDate.of(2024, 2, 1))),
      (DateValueExpr(ExactDateExprValue(1900, 2, 1)), AddressResult(List("2024")), ExpressionResult.empty),
      (DateValueExpr(ExactDateExprValue(1900, 2, 1)), StringResult("BAD"), ExpressionResult.empty)
    )
    forAll(table) { (dayMonthExpr: DateExpr, yearResult: ExpressionResult, expected: ExpressionResult) =>
      val actual = evalDateConstructExpr(
        RecData[OutOfDate](VariadicFormData.empty),
        evaluationContext,
        EvaluationResults.empty,
        booleanExprResolver
      )(dayMonthExpr, yearResult)

      actual shouldBe expected
    }
  }
}
