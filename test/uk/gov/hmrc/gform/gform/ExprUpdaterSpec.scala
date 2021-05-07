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

package uk.gov.hmrc.gform.gform

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.{ Arbitrary, Gen, Shrink }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkFormComponent, mkSection }
import uk.gov.hmrc.gform.models.ids.IndexedComponentId
import uk.gov.hmrc.gform.models.{ FormModelSupport, Interim, SectionSelectorType, VariadicFormDataSupport }
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.ExprGen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormComponentId, FormCtx, Value }

class ExprUpdaterSpec extends ScalaCheckSuite with FormModelSupport with VariadicFormDataSupport {

  val fmb = mkFormModelFromSections(List(mkSection(mkFormComponent("a", Value))))
    .expand[Interim, SectionSelectorType.Normal](VariadicFormData.empty)

  implicit val noShrink: Shrink[Int] = Shrink.shrinkAny

  implicit val arbitraryExpr: Arbitrary[Expr] = Arbitrary(ExprGen.exprGen())
  implicit val arbitraryPosNum: Arbitrary[Int] = Arbitrary(Gen.posNum[Int])

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(2000)

  private def fetchBaseIds(expr: Expr) = expr.leafs(fmb).collect { case FormCtx(fcId) =>
    fcId
  }

  property("Expand all FormCtx with given index") {
    forAll { (expr: Expr, idx: Int) =>
      val baseIds: List[FormComponentId] = fetchBaseIds(expr)

      val exprUpdated = new ExprUpdater(idx, baseIds).expandExpr(expr)

      val expandedIds: List[FormComponentId] = fetchBaseIds(exprUpdated)

      expandedIds.foreach { fcId =>
        fcId.modelComponentId.indexedComponentId match {
          case IndexedComponentId.Indexed(_, index) => assertEquals(index, idx)
          case unexpectedPure                       => fail("Expected IndexedComponentId.Indexed", clues(unexpectedPure))
        }
      }
    }
  }
}
