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
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT }
import SummarySubstituter._

class SummarySubstituterSpec extends AnyFlatSpecLike with VariadicFormDataSupport {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)
  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages = Helpers.stubMessages(Helpers.stubMessagesApi(Map.empty))

  "SummarySubstituter" should "handle ATL expression substitution" in {

    val table = Table(
      ("description", "data", "sumExpr", "exprMap", "expected"),
      (
        "an expression without any indexed components should result in the same expression",
        variadicFormData[SourceOrigin.OutOfDate]("1_atlField1" -> "10", "2_atlField1" -> "20"),
        Sum(Constant("1")),
        Map[Expr, ExpressionResult](
          FormCtx("atlField1") -> NumberResult(10.00),
          FormCtx("atlField2") -> NumberResult(20.00)
        ),
        Constant("1")
      ),
      (
        "an expression with addition should correctly substitute and unfold values",
        variadicFormData[SourceOrigin.OutOfDate]("1_atlField1" -> "10", "2_atlField1" -> "20"),
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
        "hidden fcId on iteration `n` should not remove iteration from the sum if there are still visible fields for this iteration",
        variadicFormData[SourceOrigin.OutOfDate](
          "1_atlField1" -> "10",
          "2_atlField1" -> "10",
          "1_atlField2" -> "10",
          "2_atlField2" -> "10"
        ),
        Sum(Add(FormCtx("atlField1"), FormCtx("atlField2"))),
        Map[Expr, ExpressionResult](
          FormCtx("1_atlField1") -> NumberResult(10.00),
          FormCtx("1_atlField2") -> NumberResult(10.00),
          FormCtx("2_atlField1") -> Hidden,
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
        "hidden fcId on iteration `n` should remove iteration from the sum if there are no visible fields for this iteration",
        variadicFormData[SourceOrigin.OutOfDate](
          "1_atlField1" -> "10",
          "2_atlField1" -> "10",
          "1_atlField2" -> "10",
          "2_atlField2" -> "10"
        ),
        Sum(Add(FormCtx("atlField1"), FormCtx("atlField2"))),
        Map[Expr, ExpressionResult](
          FormCtx("1_atlField1") -> NumberResult(10.00),
          FormCtx("1_atlField2") -> NumberResult(10.00),
          FormCtx("2_atlField1") -> Hidden,
          FormCtx("2_atlField2") -> Hidden
        ),
        Add(
          Constant("0"),
          Add(
            FormCtx("1_atlField1"),
            FormCtx("1_atlField2")
          )
        )
      )
    )

    forAll(table) { case (description, data, sumExpr, exprMap, expected) =>
      info(description)
      val substitutions = new SummarySubstitutions(exprMap, data)
      val resultExpression = implicitly[Substituter[SummarySubstitutions, Expr]].substitute(substitutions, sumExpr)
      resultExpression shouldBe expected
    }
  }

  it should "handle group expression substitution" in {
    val table = Table(
      ("description", "data", "sumExpr", "exprMap", "expected"),
      (
        "a constant expression should result in the same constant",
        variadicFormData[SourceOrigin.OutOfDate]("1_groupField1" -> "1", "2_groupField1" -> "2"),
        Sum(Constant("1")),
        Map[Expr, ExpressionResult](
        ),
        Constant("1")
      ),
      (
        "an expression with addition should correctly substitute and unfold values",
        variadicFormData[SourceOrigin.OutOfDate]("1_groupField1" -> "1", "2_groupField1" -> "2"),
        Sum(Add(Constant("1"), FormCtx("groupField1"))),
        Map[Expr, ExpressionResult](),
        Add(
          Add(
            Constant("0"),
            Add(
              Constant("1"),
              FormCtx("1_groupField1")
            )
          ),
          Add(
            Constant("1"),
            FormCtx("2_groupField1")
          )
        )
      )
    )

    forAll(table) { case (description, data, sumExpr, exprMap, expected) =>
      info(description)
      val substitutions = new SummarySubstitutions(exprMap, data)
      val resultExpression = implicitly[Substituter[SummarySubstitutions, Expr]].substitute(substitutions, sumExpr)
      resultExpression shouldBe expected
    }
  }

  it should "handle empty exprMap and variadicData" in {

    val table = Table(
      ("description", "data", "sumExpr", "exprMap", "expected"),
      (
        "should not substitute a simple constant",
        variadicFormData[SourceOrigin.OutOfDate](),
        Sum(Constant("1")),
        Map[Expr, ExpressionResult](),
        Constant("1")
      ),
      (
        "should not substitute a non-trivial expression",
        variadicFormData[SourceOrigin.OutOfDate](),
        Sum(Add(Constant("1"), FormCtx("atlField1"))),
        Map[Expr, ExpressionResult](),
        Add(Constant("1"), FormCtx("atlField1"))
      )
    )

    forAll(table) { case (description, data, sumExpr, exprMap, expected) =>
      info(description)
      val substitutions = new SummarySubstitutions(exprMap, data)
      val resultExpression = implicitly[Substituter[SummarySubstitutions, Expr]].substitute(substitutions, sumExpr)
      resultExpression shouldBe expected
    }
  }

}
