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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import org.scalatest.prop.TableDrivenPropertyChecks
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.BooleanExprGen
import uk.gov.hmrc.gform.{ GraphSpec, Spec }

class BooleanExprSpec extends Spec with GraphSpec with TableDrivenPropertyChecks {

  "format" should "round trip derived JSON" in {
    forAll(BooleanExprGen.booleanExprGen()) { obj =>
      BooleanExpr.format.reads(BooleanExpr.format.writes(obj)) should beJsSuccess(obj)
    }
  }

  "allExpressions" should "return all date expressions" in {
    val table = Table(
      ("dateExpr", "allExpressions"),
      (
        DateBefore(DateValueExpr(TodayDateExprValue), DateValueExpr(ExactDateExprValue(2020, 1, 1))).allExpressions,
        List(DateCtx(DateValueExpr(TodayDateExprValue)), DateCtx(DateValueExpr(ExactDateExprValue(2020, 1, 1))))
      ),
      (
        DateBefore(
          DateExprWithOffset(DateFormCtxVar(FormCtx(FormComponentId("someId"))), OffsetYMD(OffsetUnit.Day(1) :: Nil)),
          DateFormCtxVar(FormCtx(FormComponentId("someId")))
        ).allExpressions,
        List(
          DateCtx(
            DateExprWithOffset(DateFormCtxVar(FormCtx(FormComponentId("someId"))), OffsetYMD(OffsetUnit.Day(1) :: Nil))
          ),
          DateCtx(DateFormCtxVar(FormCtx(FormComponentId("someId"))))
        )
      )
    )
    forAll(table) { (actual: List[Expr], expected: List[Expr]) =>
      actual shouldBe expected
    }
  }
}
