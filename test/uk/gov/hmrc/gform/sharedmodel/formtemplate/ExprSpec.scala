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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import org.scalatest.{ FlatSpecLike, Matchers }
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkFormComponent, mkFormTemplate, mkSection }
import uk.gov.hmrc.gform.models.ExpandUtils.toModelComponentId
import uk.gov.hmrc.gform.models.{ FormModelSupport, Interim, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.ExprGen
import uk.gov.hmrc.gform.sharedmodel.{ VariadicFormData, VariadicValue }

class ExprSpec extends FlatSpecLike with Matchers with FormModelSupport with ScalaCheckDrivenPropertyChecks {

  "Expr" should "round trip derived JSON" in {
    forAll(ExprGen.exprGen()) { obj =>
      Expr.format.reads(Expr.format.writes(obj)) should beJsSuccess(obj)
    }
  }

  "firstExprForTypeResolution" should "return PeriodFun(d1, d2) expr for type resolution of PeriodFun(d1, d2) expr" in {
    val expr = Period(
      DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
      DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
    )
    val data = VariadicFormData.create[OutOfDate](
      (toModelComponentId("startDate-year"), VariadicValue.One("2000")),
      (toModelComponentId("startDate-month"), VariadicValue.One("1")),
      (toModelComponentId("startDate-day"), VariadicValue.One("1")),
      (toModelComponentId("endDate-year"), VariadicValue.One("2000")),
      (toModelComponentId("endDate-month"), VariadicValue.One("10")),
      (toModelComponentId("endDate-day"), VariadicValue.One("1"))
    )
    val formTemplate = mkFormTemplate(
      List(
        mkSection(
          List(
            mkFormComponent("startDate", Date(AnyDate, Offset(0), None)),
            mkFormComponent("endDate", Date(AnyDate, Offset(0), None))
          )
        )
      )
    )
    val formModel = mkFormModelBuilder(formTemplate).expand[Interim, SectionSelectorType.Normal](data)
    expr.firstExprForTypeResolution(formModel) shouldBe Some(expr)
  }

  it should "return PeriodValue expr for type resolution of PeriodValue(X) expr" in {
    val expr = PeriodValue("P1Y")
    val data = VariadicFormData.create[OutOfDate]()
    val formTemplate = mkFormTemplate(
      List(
        mkSection(
          List(
            mkFormComponent("date", Date(AnyDate, Offset(0), None))
          )
        )
      )
    )
    val formModel = mkFormModelBuilder(formTemplate).expand[Interim, SectionSelectorType.Normal](data)
    expr.firstExprForTypeResolution(formModel) shouldBe Some(expr)
  }

  it should "return period1 expr for type resolution of Add(period1, period2) expr" in {
    val expr = Add(
      Period(
        DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
        DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
      ),
      PeriodValue("P1Y")
    )
    val data = VariadicFormData.create[OutOfDate](
      (toModelComponentId("startDate-year"), VariadicValue.One("2000")),
      (toModelComponentId("startDate-month"), VariadicValue.One("1")),
      (toModelComponentId("startDate-day"), VariadicValue.One("1")),
      (toModelComponentId("endDate-year"), VariadicValue.One("2000")),
      (toModelComponentId("endDate-month"), VariadicValue.One("10")),
      (toModelComponentId("endDate-day"), VariadicValue.One("1"))
    )
    val formTemplate = mkFormTemplate(
      List(
        mkSection(
          List(
            mkFormComponent("startDate", Date(AnyDate, Offset(0), None)),
            mkFormComponent("endDate", Date(AnyDate, Offset(0), None))
          )
        )
      )
    )
    val formModel = mkFormModelBuilder(formTemplate).expand[Interim, SectionSelectorType.Normal](data)
    expr.firstExprForTypeResolution(formModel) shouldBe Some(
      Period(
        DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
        DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
      )
    )
  }

  it should "return period1 expr for type resolution of Else(period1, period2) expr" in {
    val expr = Else(
      Period(
        DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
        DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
      ),
      PeriodValue("P1Y")
    )
    val data = VariadicFormData.create[OutOfDate](
      (toModelComponentId("startDate-year"), VariadicValue.One("2000")),
      (toModelComponentId("startDate-month"), VariadicValue.One("1")),
      (toModelComponentId("startDate-day"), VariadicValue.One("1")),
      (toModelComponentId("endDate-year"), VariadicValue.One("2000")),
      (toModelComponentId("endDate-month"), VariadicValue.One("10")),
      (toModelComponentId("endDate-day"), VariadicValue.One("1"))
    )
    val formTemplate = mkFormTemplate(
      List(
        mkSection(
          List(
            mkFormComponent("startDate", Date(AnyDate, Offset(0), None)),
            mkFormComponent("endDate", Date(AnyDate, Offset(0), None))
          )
        )
      )
    )
    val formModel = mkFormModelBuilder(formTemplate).expand[Interim, SectionSelectorType.Normal](data)
    expr.firstExprForTypeResolution(formModel) shouldBe Some(
      Period(
        DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
        DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
      )
    )
  }

}
