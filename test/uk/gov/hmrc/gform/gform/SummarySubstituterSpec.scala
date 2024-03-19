/*
 * Copyright 2024 HM Revenue & Customs
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

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import play.api.i18n.Messages
import play.api.test.Helpers
import scala.language.implicitConversions
import uk.gov.hmrc.gform.eval.ExpressionResult
import uk.gov.hmrc.gform.eval.ExpressionResult._
import uk.gov.hmrc.gform.gform._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT }
import SummarySubstituter._

class SummarySubstituterSpec extends AnyFlatSpecLike with VariadicFormDataSupport {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)
  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages = Helpers.stubMessages(Helpers.stubMessagesApi(Map.empty))

  "SummarySubstituter" should "handle ATL expression substitution" in {

    val table = Table(
      ("description", "repeatedDetails", "sumExpr", "exprMap", "expected"),
      (
        "an expression without repeated components should result in the same expression",
        RepeatedComponentsDetails(
          Map[FormComponentId, FormComponentId]()
        ),
        Sum(Add(Constant("1"), FormCtx("atlField1"))),
        Map[Expr, ExpressionResult](
          FormCtx("1_atlField1") -> NumberResult(10.00),
          FormCtx("2_atlField1") -> NumberResult(20.00)
        ),
        Add(Constant("1"), FormCtx("atlField1"))
      ),
      (
        "an expression with addition should correctly substitute and unfold values",
        RepeatedComponentsDetails(
          Map[FormComponentId, FormComponentId](
            FormComponentId("atlField1") -> FormComponentId("atlParent")
          )
        ),
        Sum(Add(Constant("1"), FormCtx("atlField1"))),
        Map[Expr, ExpressionResult](
          FormCtx("1_atlField1") -> NumberResult(10.00),
          FormCtx("2_atlField1") -> NumberResult(20.00)
        ),
        Add(
          Add(
            Constant("0"),
            Add(
              Constant("1"),
              FormCtx("1_atlField1")
            )
          ),
          Add(
            Constant("1"),
            FormCtx("2_atlField1")
          )
        )
      ),
      (
        "hidden fcIds should not remove iteration",
        RepeatedComponentsDetails(
          Map[FormComponentId, FormComponentId](
            FormComponentId("atlField1") -> FormComponentId("atlParent"),
            FormComponentId("atlField2") -> FormComponentId("atlParent")
          )
        ),
        Sum(Add(FormCtx("atlField1"), FormCtx("atlField2"))),
        Map[Expr, ExpressionResult](
          FormCtx("1_atlField1") -> Hidden,
          FormCtx("1_atlField2") -> Hidden,
          FormCtx("2_atlField1") -> Hidden,
          FormCtx("2_atlField2") -> Hidden
        ),
        Add(
          Add(
            Constant("0"),
            Add(
              FormCtx("1_atlField1"),
              FormCtx("1_atlField2")
            )
          ),
          Add(
            FormCtx("2_atlField1"),
            FormCtx("2_atlField2")
          )
        )
      ),
      (
        "non hidden fcIds should not remove iteration",
        RepeatedComponentsDetails(
          Map[FormComponentId, FormComponentId](
            FormComponentId("atlField1") -> FormComponentId("atlParent"),
            FormComponentId("atlField2") -> FormComponentId("atlParent")
          )
        ),
        Sum(Add(FormCtx("atlField1"), FormCtx("atlField2"))),
        Map[Expr, ExpressionResult](
          FormCtx("1_atlField1") -> NumberResult(10.00),
          FormCtx("1_atlField2") -> NumberResult(10.00),
          FormCtx("2_atlField1") -> NumberResult(10.00),
          FormCtx("2_atlField2") -> NumberResult(10.00)
        ),
        Add(
          Add(
            Constant("0"),
            Add(
              FormCtx("1_atlField1"),
              FormCtx("1_atlField2")
            )
          ),
          Add(
            FormCtx("2_atlField1"),
            FormCtx("2_atlField2")
          )
        )
      ),
      (
        "an expression with hidden parent of the repeated field should be Constant(0)",
        RepeatedComponentsDetails(
          Map[FormComponentId, FormComponentId](
            FormComponentId("atlField1") -> FormComponentId("atlParent")
          )
        ),
        Sum(Add(Constant("1"), FormCtx("atlField1"))),
        Map[Expr, ExpressionResult](
          FormCtx("1_atlField1") -> Hidden,
          FormCtx("1_atlParent") -> Hidden
        ),
        Constant("0")
      )
    )

    forAll(table) { case (description, repeatedDetails, sumExpr, exprMap, expected) =>
      info(description)
      val substitutions = new SummarySubstitutions(exprMap, repeatedDetails)
      val resultExpression = implicitly[Substituter[SummarySubstitutions, Expr]].substitute(substitutions, sumExpr)
      resultExpression shouldBe expected
    }
  }

}
