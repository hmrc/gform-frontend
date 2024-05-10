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

package uk.gov.hmrc.gform.gform

import munit.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.Messages
import play.api.test.Helpers

import scala.language.implicitConversions
import uk.gov.hmrc.gform.models.VariadicFormDataSupport
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Concat, Constant, Expr, FormComponentId, IfElse, IsFalse }
import ConcatFormatSubstituter._

class ConcatFormatSubstituterSuite extends FunSuite with VariadicFormDataSupport {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)
  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages = Helpers.stubMessages(Helpers.stubMessagesApi(Map.empty))

  test("Substituter should correctly substitute ConcatFormatSubstitutions in expressions") {
    val table = TableDrivenPropertyChecks.Table(
      ("expr", "concatFormatFunction", "expected"),
      (
        IfElse(IsFalse, Constant("1"), Concat(List(Constant("1000")))),
        (_: Concat) => "1.000",
        IfElse(IsFalse, Constant("1"), Constant("1.000"))
      ),
      (
        IfElse(IsFalse, Constant("1"), Constant("2")),
        (_: Concat) => "nothing",
        IfElse(IsFalse, Constant("1"), Constant("2"))
      )
    )

    TableDrivenPropertyChecks.forAll(table) { (expr, function, expected) =>
      val substitutions = ConcatFormatSubstitutions(function)
      val resultExpression = implicitly[Substituter[ConcatFormatSubstitutions, Expr]].substitute(substitutions, expr)
      assertEquals(resultExpression, expected)
    }
  }
}
