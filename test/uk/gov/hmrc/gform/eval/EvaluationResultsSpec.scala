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
import uk.gov.hmrc.gform.eval.ExpressionResult.DateResult
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.ExpandUtils.toModelComponentId
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.{ VariadicFormData, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DateCtx, DateFormCtxVar, FormComponentId, FormCtx }

class EvaluationResultsSpec extends Spec with TableDrivenPropertyChecks {

  "evalExpr" should "evaluate expression of type dateString" in {

    val recData = RecData[OutOfDate](
      VariadicFormData.create(
        (toModelComponentId("dateFieldId-year"), VariadicValue.One("1970")),
        (toModelComponentId("dateFieldId-month"), VariadicValue.One("1")),
        (toModelComponentId("dateFieldId-day"), VariadicValue.One("11"))
      ))

    val table = Table(
      ("typeInfo", "recData", "expectedResult"),
      (
        TypeInfo(
          DateCtx(DateFormCtxVar(FormCtx(FormComponentId("dateFieldId")))),
          StaticTypeData(ExprType.dateString, None)),
        recData,
        DateResult(LocalDate.of(1970, 1, 11))),
      (
        TypeInfo(FormCtx(FormComponentId("dateFieldId")), StaticTypeData(ExprType.dateString, None)),
        recData,
        DateResult(LocalDate.of(1970, 1, 11)))
    )

    forAll(table) { (typeInfo: TypeInfo, recData: RecData[OutOfDate], expectedResult: DateResult) =>
      EvaluationResults.empty.evalExpr(typeInfo, recData, evaluationContext) shouldBe expectedResult
    }
  }
}
