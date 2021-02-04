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
import uk.gov.hmrc.gform.eval.ExpressionResult.{DateResult, StringResult}
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.ExpandUtils.toModelComponentId
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.{VariadicFormData, VariadicValue}
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{Constant, DateCtx, DateFormCtxVar, Else, FormComponentId, FormCtx}

class EvaluationResultsSpec extends Spec with TableDrivenPropertyChecks {

  "evalExpr" should "evaluate expression of type dateString" in {

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
        StringResult("someConstant")),
      (
        TypeInfo(
          Else(FormCtx(FormComponentId("textFieldEmpty")),FormCtx(FormComponentId("textField"))),
          StaticTypeData(ExprType.dateString, None)),
        recData,
        StringResult("textFieldValue"))
    )

    forAll(table) { (typeInfo: TypeInfo, recData: RecData[OutOfDate], expectedResult: ExpressionResult) =>
      EvaluationResults.empty.evalExpr(typeInfo, recData, evaluationContext) shouldBe expectedResult
    }
  }
}
