/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.gform.eval.DateExprEval.evalDateExpr
import uk.gov.hmrc.gform.eval.ExpressionResult.DateResult
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.{ VariadicFormData, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Date, DateExprWithOffset, DateFormCtxVar, DateValueExpr, ExactDateExprValue, FormComponentId, FormCtx, OffsetUnitDay, OffsetUnitMonth, OffsetUnitYear, TodayDateExprValue }

class DateExprEvalSpec extends Spec with TableDrivenPropertyChecks {

  "evalDateExpr" should "evaluate a date expression with TodayDateExprValue" in {
    val expressionResult = evalDateExpr(RecData[OutOfDate](VariadicFormData.empty), EvaluationResults.empty)(
      DateValueExpr(TodayDateExprValue))
    expressionResult shouldBe DateResult(LocalDate.now())
  }

  it should "evaluate a date expression with ExactDateExprValue" in {
    val expressionResult = evalDateExpr(RecData[OutOfDate](VariadicFormData.empty), EvaluationResults.empty)(
      DateValueExpr(
        ExactDateExprValue(LocalDate.now().getYear, LocalDate.now().getMonthValue, LocalDate.now().getDayOfMonth)))
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
        )),
      EvaluationResults.empty
    )(DateFormCtxVar(FormCtx(dateField)))
    expressionResult shouldBe DateResult(LocalDate.of(1970, 1, 11))
  }

  it should "evaluate a date expression with form field, from EvaluationResults" in {
    val dateField = FormComponentId("dateFieldId")
    val expressionResult = evalDateExpr(
      RecData[OutOfDate](VariadicFormData.empty),
      EvaluationResults.one(FormCtx(dateField), DateResult(LocalDate.now()))
    )(DateFormCtxVar(FormCtx(dateField)))
    expressionResult shouldBe DateResult(LocalDate.now())
  }

  it should "evaluate a date expression with offset" in {
    val table = Table(
      ("actual", "expected"),
      (
        evalDateExpr(
          RecData[OutOfDate](VariadicFormData.empty),
          EvaluationResults.empty
        )(DateExprWithOffset(DateValueExpr(TodayDateExprValue), 1, OffsetUnitDay)),
        DateResult(LocalDate.now().plusDays(1))),
      (
        evalDateExpr(
          RecData[OutOfDate](VariadicFormData.empty),
          EvaluationResults.empty
        )(DateExprWithOffset(DateValueExpr(TodayDateExprValue), 1, OffsetUnitMonth)),
        DateResult(LocalDate.now().plusMonths(1))),
      (
        evalDateExpr(
          RecData[OutOfDate](VariadicFormData.empty),
          EvaluationResults.empty
        )(DateExprWithOffset(DateValueExpr(TodayDateExprValue), 1, OffsetUnitYear)),
        DateResult(LocalDate.now().plusYears(1)))
    )
    forAll(table) { (actual: ExpressionResult, expected: ExpressionResult) =>
      actual shouldBe expected
    }

  }
}
