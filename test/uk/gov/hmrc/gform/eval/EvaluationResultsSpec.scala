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

import org.scalatest.prop.TableDrivenPropertyChecks
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.eval.ExpressionResult.{ DateResult, NumberResult, StringResult }
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.ExpandUtils.toModelComponentId
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, Count, DateCtx, DateFormCtxVar, Else, FormComponentId, FormCtx }
import uk.gov.hmrc.gform.sharedmodel.{ VariadicFormData, VariadicValue }

import java.time.LocalDate

class EvaluationResultsSpec extends Spec with TableDrivenPropertyChecks {

  "evalExpr - type dateString" should "evaluate expressions" in {

    val recData = RecData[OutOfDate](
      VariadicFormData.create(
        (toModelComponentId("dateFieldId1-year"), VariadicValue.One("1970")),
        (toModelComponentId("dateFieldId1-month"), VariadicValue.One("1")),
        (toModelComponentId("dateFieldId1-day"), VariadicValue.One("11")),
        (toModelComponentId("dateFieldId2-year"), VariadicValue.One("1971")),
        (toModelComponentId("dateFieldId2-month"), VariadicValue.One("1")),
        (toModelComponentId("dateFieldId2-day"), VariadicValue.One("11")),
        (toModelComponentId("textFieldEmpty"), VariadicValue.One("")),
        (toModelComponentId("textField"), VariadicValue.One("textFieldValue"))
      ))

    val table = Table(
      ("typeInfo", "recData", "expectedResult"),
      (
        TypeInfo(
          DateCtx(DateFormCtxVar(FormCtx(FormComponentId("dateFieldId1")))),
          StaticTypeData(ExprType.dateString, None)),
        recData,
        DateResult(LocalDate.of(1970, 1, 11))),
      (
        TypeInfo(FormCtx(FormComponentId("dateFieldId1")), StaticTypeData(ExprType.dateString, None)),
        recData,
        DateResult(LocalDate.of(1970, 1, 11))),
      (
        TypeInfo(
          Else(FormCtx(FormComponentId("dateFieldId1")), FormCtx(FormComponentId("dateFieldId2"))),
          StaticTypeData(ExprType.dateString, None)),
        recData,
        DateResult(LocalDate.of(1970, 1, 11))),
      (
        TypeInfo(
          Else(FormCtx(FormComponentId("textFieldEmpty")), FormCtx(FormComponentId("dateFieldId2"))),
          StaticTypeData(ExprType.dateString, None)),
        recData,
        DateResult(LocalDate.of(1971, 1, 11))),
      (
        TypeInfo(
          Else(FormCtx(FormComponentId("textFieldEmpty")), Constant("someConstant")),
          StaticTypeData(ExprType.dateString, None)),
        recData,
        StringResult("someConstant"))
    )

    forAll(table) { (typeInfo: TypeInfo, recData: RecData[OutOfDate], expectedResult: ExpressionResult) =>
      EvaluationResults.empty.evalExpr(typeInfo, recData, evaluationContext) shouldBe expectedResult
    }
  }

  "evalExpr - type string" should "evaluate expressions" in {

    val table = Table(
      ("typeInfo", "recData", "expectedResult", "scenario"),
      (
        TypeInfo(
          DateCtx(DateFormCtxVar(FormCtx(FormComponentId("dateFieldId1")))),
          StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("dateFieldId1-year"), VariadicValue.One("1970")),
            (toModelComponentId("dateFieldId1-month"), VariadicValue.One("1")),
            (toModelComponentId("dateFieldId1-day"), VariadicValue.One("11")),
            (toModelComponentId("dateFieldId1"), VariadicValue.One("11 January 1970"))
          )),
        StringResult("11 January 1970"),
        "DateCtx expression converted to type 'string'"),
      (
        TypeInfo(FormCtx(FormComponentId("dateFieldId1")), StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("dateFieldId1-year"), VariadicValue.One("1970")),
            (toModelComponentId("dateFieldId1-month"), VariadicValue.One("1")),
            (toModelComponentId("dateFieldId1-day"), VariadicValue.One("11")),
            (toModelComponentId("dateFieldId1"), VariadicValue.One("11 January 1970"))
          )),
        StringResult("11 January 1970"),
        "FormCtx Expression converted to type 'string'"),
      (
        TypeInfo(Count(FormComponentId("addToListQuestion")), StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("1_addToListQuestion"), VariadicValue.One("0")),
            (toModelComponentId("2_addToListQuestion"), VariadicValue.One("1")),
            (toModelComponentId("1_addToListField1"), VariadicValue.One("Hello")),
            (toModelComponentId("2_addToListField1"), VariadicValue.One("World"))
          )),
        StringResult("2"),
        "Eval Count(addToListComponent) as string"
      )
    )
    forAll(table) { (typeInfo: TypeInfo, recData: RecData[OutOfDate], expectedResult: ExpressionResult, _) =>
      EvaluationResults.empty.evalExpr(typeInfo, recData, evaluationContext) shouldBe expectedResult
    }
  }

  "evalExpr - type number" should "evaluate expressions" in {
    val recData = RecData[OutOfDate](
      VariadicFormData.create(
        (toModelComponentId("1_addToListQuestion"), VariadicValue.One("0")),
        (toModelComponentId("2_addToListQuestion"), VariadicValue.One("1")),
        (toModelComponentId("1_addToListField1"), VariadicValue.One("Hello")),
        (toModelComponentId("2_addToListField1"), VariadicValue.One("World"))
      ))

    val table = Table(
      ("typeInfo", "recData", "expectedResult", "scenario"),
      (
        TypeInfo(Count(FormComponentId("addToListQuestion")), StaticTypeData(ExprType.number, None)),
        recData,
        NumberResult(2),
        "Ref to AddToList count in number field")
    )
    forAll(table) { (typeInfo: TypeInfo, recData: RecData[OutOfDate], expectedResult: ExpressionResult, _) =>
      EvaluationResults.empty.evalExpr(typeInfo, recData, evaluationContext) shouldBe expectedResult
    }
  }
}
