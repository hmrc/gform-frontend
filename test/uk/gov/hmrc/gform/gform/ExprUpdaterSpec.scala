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

package uk.gov.hmrc.gform.gform

import munit.ScalaCheckSuite
import uk.gov.hmrc.gform.models.{ FormModelSupport, VariadicFormDataSupport }
import uk.gov.hmrc.gform.Helpers.{ toSmartString, toSmartStringExpression }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.mkFormComponent
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Choice, FormComponentId, InformationMessage, LastOption, OptionData, OptionDataValue, Radio, StandardInfo, Vertical }

class ExprUpdaterSpec extends ScalaCheckSuite with FormModelSupport with VariadicFormDataSupport {

  test("lastOption expands references to the current repeated item") {
    val choiceId = FormComponentId("approvalType")
    val updated = new ExprUpdater(2, List(choiceId), Nil).expandExpr(LastOption(choiceId))
    assertEquals(updated, LastOption(FormComponentId("2_approvalType")))
    assertEquals(new ExprUpdater(2, Nil, Nil).expandExpr(LastOption(choiceId)), LastOption(choiceId))
  }

  test("choice expansion updates the replacement info's lastOption reference") {
    val choiceId = FormComponentId("approvalType")
    val info = mkFormComponent(
      "lastInfo",
      InformationMessage(StandardInfo, toSmartStringExpression("Tell us about {0}", LastOption(choiceId)))
    )
    val option = OptionData.ValueBased(
      toSmartString("Importer"),
      None,
      None,
      None,
      OptionDataValue.StringBased("importer"),
      None,
      None
    )
    val choice =
      Choice(Radio, List(option), Vertical, Nil, None, None, toSmartString("or"), None, None, true, false, Some(info))
    val component = mkFormComponent("approvalType", choice)
    val updated = new FormComponentUpdater(component, 2, List(choiceId), Nil).updatedWithId
    val replacement = updated.`type`.asInstanceOf[Choice].whenOnlyOneChoiceRemains.get
    assertEquals(replacement.id, FormComponentId("2_lastInfo"))
    assertEquals(
      replacement.`type`.asInstanceOf[InformationMessage].infoText.allInterpolations,
      List(LastOption(FormComponentId("2_approvalType")))
    )
  }

  // val formModel = mkFormModelFromSections(List(mkSection(mkFormComponent("a", Value))))
  //   .expand[Interim, SectionSelectorType.Normal](VariadicFormData.empty)

  // implicit val noShrink: Shrink[Int] = Shrink.shrinkAny

  // implicit val arbitraryExpr: Arbitrary[Expr] = Arbitrary(ExprGen.exprGen())
  // implicit val arbitraryPosNum: Arbitrary[Int] = Arbitrary(Gen.posNum[Int])

  // override def scalaCheckTestParameters =
  //   super.scalaCheckTestParameters
  //     .withMinSuccessfulTests(2000)

  // private def fetchBaseIds(expr: Expr) = expr.leafs(formModel).collect { case FormCtx(fcId) =>
  //   fcId
  // }

  // property("Expand all FormCtx with given index") {
  //   forAll { (expr: Expr, idx: Int) =>
  //     val baseIds: List[FormComponentId] = fetchBaseIds(expr)

  //     val exprUpdated = new ExprUpdater(idx, baseIds).expandExpr(expr)

  //     val expandedIds: List[FormComponentId] = fetchBaseIds(exprUpdated)

  //     expandedIds.foreach { fcId =>
  //       fcId.modelComponentId.indexedComponentId match {
  //         case IndexedComponentId.Indexed(_, index) => assertEquals(index, idx)
  //         case unexpectedPure                       => fail("Expected IndexedComponentId.Indexed", clues(unexpectedPure))
  //       }
  //     }
  //   }
  // }
}
