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

package uk.gov.hmrc.gform.graph

import cats.data.NonEmptyList
import org.scalactic.source.Position
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import org.scalatest.prop.TableFor3
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.eval.ExpressionResult._
import uk.gov.hmrc.gform.eval.{ EvaluationContext, EvaluationResults, ExpressionResult }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.models.FormModelSupport
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.GraphSpec
import org.scalatest.{ FlatSpec, Matchers }

class RecalculationSpec extends FlatSpec with Matchers with GraphSpec with FormModelSupport {

  type EitherEffect[A] = Either[GraphException, A]

  private val sterling: TextConstraint = Sterling(RoundingMode.defaultRoundingMode, true)
  private def ctx(s: String): FormCtx = FormCtx(FormComponentId(s))
  private def const(s: String): Expr = Constant(s)

  def evaluationContext(formTemplate: FormTemplate): EvaluationContext =
    new EvaluationContext(
      formTemplate._id,
      SubmissionRef(EnvelopeId("")),
      None,
      ExampleData.authContext,
      ThirdPartyData.empty,
      formTemplate.authConfig,
      hc)

  "recalculation" should "recalculate single dependency" in {

    val inputData = mkDataOutOfDate(
      "a" -> "123",
      "b" -> "eee"
    )

    val expectedExprMap: Map[Expr, ExpressionResult] = Map(ctx("a") -> StringResult("123"))
    val expectedOutputData = mkDataCurrent(
      "a" -> "123",
      "b" -> "123"
    )

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Value),
          mkFormComponent("b", ctx("a"))
        )
      )
    )

    verify(inputData, expectedOutputData, expectedExprMap, sections)

  }

  it should "recalculate IdNumberValue for HmrcTaxPeriod component" in {

    val inputData = mkDataOutOfDate(
      "a" -> "123",
      "b" -> "eee"
    )

    val expectedOutputData = mkDataCurrent(
      "a" -> "123",
      "b" -> "eee"
    )

    val expectedExprMap: Map[Expr, ExpressionResult] = Map(ctx("a") -> StringResult("123"))

    val hmrcTaxPeriod = HmrcTaxPeriod(IdType("idType"), ctx("a"), RegimeType("RegimeType"))

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Value),
          mkFormComponent("b", hmrcTaxPeriod)
        )
      )
    )

    verify(inputData, expectedOutputData, expectedExprMap, sections)
  }

  it should "handle group component" in {

    val formComponentIds: TableFor3[
      VariadicFormData[SourceOrigin.OutOfDate],
      VariadicFormData[SourceOrigin.Current],
      Map[Expr, ExpressionResult]] = Table(
      ("input", "output", "expectedExprMap"),
      (mkDataOutOfDate(), mkDataCurrent(), Map(ctx("a") -> Empty)),
      (
        mkDataOutOfDate("a"                  -> "2"),
        mkDataCurrent("a"                    -> "2"),
        Map[Expr, ExpressionResult](ctx("a") -> StringResult("2"))),
      (
        mkDataOutOfDate("a" -> "2", "1_b" -> "", "1_c" -> ""),
        mkDataCurrent("a"   -> "2", "1_b" -> "", "1_c" -> ""),
        Map(ctx("a")        -> StringResult("2"))),
      (
        mkDataOutOfDate("a" -> "2", "1_b" -> "2", "2_b" -> "", "1_c" -> "", "2_c" -> ""),
        mkDataCurrent("a"   -> "2", "1_b" -> "2", "2_b" -> "", "1_c" -> "", "2_c" -> ""),
        Map(ctx("a")        -> StringResult("2"))),
      (
        mkDataOutOfDate("a" -> "2", "1_b" -> "2", "2_b" -> "2", "1_c" -> "3", "2_c" -> "3"),
        mkDataCurrent("a"   -> "2", "1_b" -> "2", "2_b" -> "2", "1_c" -> "3", "2_c" -> "3"),
        Map(ctx("a")        -> StringResult("2")))
    )
    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Value),
          mkFormComponent(
            "group",
            mkGroup(5, List(mkFormComponentEditable("b", ctx("a")), mkFormComponent("c", Value))))
        )
      )
    )

    forAll(formComponentIds) { (input, expectedOutputData, expectedExprMap) ⇒
      verify(input, expectedOutputData, expectedExprMap: Map[Expr, ExpressionResult], sections)
    }
  }

  it should "detect cycle in dependencies (graph cannot be sorted)" in {

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", ctx("b")),
          mkFormComponent("b", ctx("a"))
        )
      )
    )

    val inputData = mkDataOutOfDate()
    val expectedOutputData = mkDataCurrent()
    val expectedExprMap = Map.empty[Expr, ExpressionResult]

    the[IllegalArgumentException] thrownBy verify(inputData, expectedOutputData, expectedExprMap, sections)

  }

  it should "handle sterling addition" in {

    val constraint: TextConstraint = Sterling(RoundingMode.defaultRoundingMode, true)

    val textA = Text(constraint, Value)
    val textB = Text(constraint, Add(ctx("a"), Constant("2")))

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", textA),
          mkFormComponent("b", textB)
        )
      )
    )

    val formComponentIds = Table(
      // format: off
      ("input", "output"),
      (mkDataOutOfDate("a" -> "1"),
       mkDataCurrent("a" -> "1", "b" -> "3.00")),
      (mkDataOutOfDate("a" -> "1", "b" -> "2"),
       mkDataCurrent("a" -> "1", "b" -> "3.00"))
      // format: on
    )

    val expectedExprMap = Map(ctx("a") -> NumberResult(1), Constant("2") -> NumberResult(2))

    forAll(formComponentIds) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, expectedExprMap, sections)
    }
  }

  it should "handle text addition" in {

    val constraint: TextConstraint = ShortText.default

    val textA = Text(constraint, Value)
    val textB = Text(constraint, Add(ctx("a"), Constant("2")))

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", textA),
          mkFormComponent("b", textB)
        )
      )
    )

    val formComponentIds = Table(
      // format: off
      ("input", "output"),
      (mkDataOutOfDate("a" -> "1"),
       mkDataCurrent("a" -> "1", "b" -> "12")),
      (mkDataOutOfDate("a" -> "1", "b" -> "2"),
       mkDataCurrent("a" -> "1", "b" -> "12"))
      // format: on
    )

    val expectedExprMap = Map(ctx("a") -> StringResult("1"), Constant("2") -> NumberResult(2))

    forAll(formComponentIds) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, expectedExprMap, sections)
    }
  }

  it should "recalculate chain of dependencies" in {

    val inputData = mkDataOutOfDate(
      "a" -> "100",
      "b" -> "100",
      "c" -> "100",
      "d" -> "100"
    )

    val expectedOutputData = mkDataCurrent(
      "a" -> "100",
      "b" -> "110.00",
      "c" -> "220.00",
      "d" -> "330.00"
    )

    val sections = List(
      mkSection(mkFormComponent("a", Value, sterling) :: Nil),
      mkSection(mkFormComponent("b", Add(ctx("a"), Constant("10")), sterling) :: Nil),
      mkSection(mkFormComponent("c", Multiply(ctx("b"), Constant("2")), sterling) :: Nil),
      mkSection(mkFormComponent("d", Add(ctx("b"), ctx("c")), sterling) :: Nil)
    )

    val expectedExprMap = Map(
      FormCtx(FormComponentId("a")) -> NumberResult(100.00),
      FormCtx(FormComponentId("b")) -> NumberResult(110.00),
      FormCtx(FormComponentId("c")) -> NumberResult(220.00),
      Constant("2")                 -> NumberResult(2),
      Constant("10")                -> NumberResult(10)
    )

    verify(inputData, expectedOutputData, expectedExprMap, sections)

  }

  it should "recalculate trees of chain of dependencies" in {
    val inputData = mkDataOutOfDate(
      "a" -> "100",
      "b" -> "100",
      "c" -> "200",
      "d" -> "200"
    )

    val expectedOutputData = mkDataCurrent(
      "a" -> "100",
      "b" -> "110.00",
      "c" -> "200",
      "d" -> "220.00"
    )

    val sections = List(
      mkSection(mkFormComponent("a", Value, sterling) :: Nil),
      mkSection(mkFormComponent("b", Add(ctx("a"), Constant("10")), sterling) :: Nil),
      mkSection(mkFormComponent("c", Value, sterling) :: Nil),
      mkSection(mkFormComponent("d", Add(ctx("c"), Constant("20")), sterling) :: Nil)
    )

    val expectedExprMap = Map(
      Constant("10")                -> NumberResult(10),
      FormCtx(FormComponentId("a")) -> NumberResult(100.00),
      Constant("20")                -> NumberResult(20),
      FormCtx(FormComponentId("c")) -> NumberResult(200.00)
    )

    verify(inputData, expectedOutputData, expectedExprMap, sections)

  }

  it should "disregard invisible parts" in {
    val inputData = mkDataOutOfDate(
      "a" -> "1",
      "b" -> "10",
      "c" -> "100"
    )

    val expectedOutputData = mkDataCurrent(
      "a" -> "1",
      "b" -> "10",
      "c" -> "1.00"
    )

    val includeIf = IncludeIf(Equals(ctx("a"), Constant("0")))

    val sections =
      mkSection(List(mkFormComponent("a", Value, sterling))) ::
        mkSectionIncludeIf(List(mkFormComponent("b", Value, sterling)), includeIf) ::
        mkSection(List(mkFormComponent("c", Add(ctx("a"), ctx("b")), sterling))) :: Nil

    val expectedExprMap = Map(
      FormCtx(FormComponentId("a")) -> NumberResult(1),
      Constant("0")                 -> NumberResult(0),
      FormCtx(FormComponentId("b")) -> Hidden)

    verify(inputData, expectedOutputData, expectedExprMap, sections)

  }

  it should "not recalculate sections which are invisible based on choice component" in {

    val choice =
      Choice(YesNo, NonEmptyList.of(toSmartString("yes"), toSmartString("no")), Vertical, List.empty, None)

    val formComponentIds = Table(
      ("input", "output", "expectedExprMap"),
      (
        mkDataManyOutOfDate("a" -> "0", "b" -> "0", "c" -> "100", "d" -> "200"),
        mkDataManyCurrent("a"   -> "0", "b" -> "0", "c" -> "100", "d" -> "200"),
        Map(
          ctx("a")   -> OptionResult(List(0)),
          ctx("b")   -> OptionResult(List(0)),
          ctx("d")   -> Hidden,
          const("0") -> NumberResult(0),
          const("1") -> NumberResult(1)
        )),
      (
        mkDataManyOutOfDate("a" -> "0", "b" -> "1", "c" -> "100", "d" -> "200"),
        mkDataManyCurrent("a"   -> "0", "b" -> "1", "c" -> "100", "d" -> "200"),
        Map(
          ctx("a")   -> OptionResult(List(0)),
          ctx("b")   -> OptionResult(List(1)),
          ctx("c")   -> Hidden,
          const("0") -> NumberResult(0),
          const("1") -> NumberResult(1)
        )),
      (
        mkDataManyOutOfDate("a" -> "1", "b" -> "0", "c" -> "100", "d" -> "200"),
        mkDataManyCurrent("a"   -> "1", "b" -> "0", "c" -> "100", "d" -> "200"),
        Map(
          ctx("a")   -> OptionResult(List(1)),
          ctx("b")   -> Hidden,
          ctx("c")   -> Hidden,
          ctx("d")   -> Hidden,
          const("0") -> NumberResult(0),
          const("1") -> NumberResult(1)
        )),
      (
        mkDataManyOutOfDate("a" -> "1", "b" -> "1", "c" -> "100", "d" -> "200"),
        mkDataManyCurrent("a"   -> "1", "b" -> "1", "c" -> "100", "d" -> "200"),
        Map(
          ctx("a")   -> OptionResult(List(1)),
          ctx("b")   -> Hidden,
          ctx("c")   -> Hidden,
          ctx("d")   -> Hidden,
          const("0") -> NumberResult(0),
          const("1") -> NumberResult(1)
        ))
    )

    val includeIf1 = IncludeIf(Contains(FormCtx(FormComponentId("a")), Constant("0")))
    val includeIf2 = IncludeIf(Contains(FormCtx(FormComponentId("b")), Constant("0")))
    val includeIf3 = IncludeIf(Contains(FormCtx(FormComponentId("b")), Constant("1")))

    forAll(formComponentIds) { (input, expectedOutput, expectedExprMap) ⇒
      val sections = List(
        mkSection(List(mkFormComponent("a", choice))),
        mkSectionIncludeIf(List(mkFormComponent("b", choice)), includeIf1),
        mkSectionIncludeIf(
          List(mkFormComponent("c", Add(FormCtx(FormComponentId("a")), FormCtx(FormComponentId("b"))))),
          includeIf2),
        mkSectionIncludeIf(
          List(mkFormComponent("d", Add(FormCtx(FormComponentId("a")), FormCtx(FormComponentId("b"))))),
          includeIf3)
      )

      verify(input, expectedOutput, expectedExprMap, sections)
    }
  }

  it should "handle Else expression (in SmartString label) depending on IncludeIf page" in {

    val choice =
      Choice(YesNo, NonEmptyList.of(toSmartString("yes"), toSmartString("no")), Vertical, List.empty, None)

    val formComponentIds = Table(
      ("input", "output", "expectedExprMap"),
      (
        mkDataManyOutOfDate("a" -> "0", "c" -> "0") ++ mkDataOutOfDate("b" -> "Your class"),
        mkDataManyCurrent("a"   -> "0", "c" -> "0") ++ mkDataCurrent("b"   -> "your client"),
        Map(
          ctx("a")             -> OptionResult(List(0)),
          ctx("b")             -> StringResult("your client"),
          const("0")           -> NumberResult(0),
          const("you")         -> StringResult("you"),
          const("your client") -> StringResult("your client"),
        )),
      (
        mkDataManyOutOfDate("a" -> "1", "c" -> "0") ++ mkDataOutOfDate("b" -> "Your class"),
        mkDataManyCurrent("a"   -> "1", "c" -> "0") ++ mkDataCurrent("b"   -> "Your class"),
        Map(
          ctx("a")             -> OptionResult(List(1)),
          ctx("b")             -> Hidden,
          const("0")           -> NumberResult(0),
          const("you")         -> StringResult("you"),
          const("your client") -> StringResult("your client"),
        )),
    )

    val includeIf1 = IncludeIf(Contains(ctx("a"), Constant("0")))

    for (i <- 0 to 100) {
      forAll(formComponentIds) { (input, expectedOutput, expectedExprMap) ⇒
        val sections = List(
          mkSection(List(mkFormComponent("a", choice))),
          mkSectionIncludeIf(List(mkFormComponent("b", const("your client"))), includeIf1),
          mkSection(
            List(mkFormComponent("c", choice).copy(
              label = SmartString(LocalisedString.empty, List(Else(ctx("b"), const("you"))))))),
        )

        verify(input, expectedOutput, expectedExprMap, sections)
      }
    }
  }

  it should "handle String addition" in {

    val inputData = mkDataOutOfDate(
      "a" -> "1",
      "b" -> "10"
    )

    val expectedOutputData = mkDataCurrent(
      "a" -> "1",
      "b" -> "10",
      "c" -> "110"
    )

    val expectedExprMap: Map[Expr, ExpressionResult] = Map(
      ctx("a") -> StringResult("1"),
      ctx("b") -> StringResult("10"),
    )

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSection(List(mkFormComponent("b", Value))) ::
        mkSection(List(mkFormComponent("c", Add(ctx("a"), ctx("b"))))) :: Nil

    verify(inputData, expectedOutputData, expectedExprMap, sections)

  }

  it should "recalculate selected fields on revealing choice" in {

    val data = Table(
      ("input", "output"),
      (
        mkDataManyOutOfDate("c" -> "0") ++ mkDataOutOfDate("a" -> "1", "b" -> "1", "d" -> "0", "e"    -> "0"),
        mkDataManyCurrent("c"   -> "0") ++ mkDataCurrent("a"   -> "1", "b" -> "1", "d" -> "2.00", "e" -> "0")),
      (
        mkDataManyOutOfDate("c" -> "1") ++ mkDataOutOfDate("a" -> "1", "b" -> "1", "d" -> "0", "e" -> "0"),
        mkDataManyCurrent("c"   -> "1") ++ mkDataCurrent("a"   -> "1", "b" -> "1", "d" -> "0", "e" -> "2.00"))
    )

    val expectedExprMap: Map[Expr, ExpressionResult] = Map(
      ctx("a") -> NumberResult(1.00),
      ctx("b") -> NumberResult(1.00),
    )

    val sections =
      mkSection(List(mkFormComponent("a", Value, sterling))) ::
        mkSection(List(mkFormComponent("b", Value, sterling))) ::
        mkSection(
        List(mkFormComponent(
          "c",
          RevealingChoice(
            List(
              RevealingChoiceElement(
                toSmartString("Yes"),
                mkFormComponent("d", Add(FormCtx(FormComponentId("a")), FormCtx(FormComponentId("b"))), sterling) :: Nil,
                false),
              RevealingChoiceElement(
                toSmartString("No"),
                mkFormComponent("e", Add(FormCtx(FormComponentId("a")), FormCtx(FormComponentId("b"))), sterling) :: Nil,
                false)
            ),
            true
          )
        ))) :: Nil

    forAll(data) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, expectedExprMap, sections)
    }

  }

  it should "detect non-selected fields on revealing choice component" in {
    val data: TableFor3[
      VariadicFormData[SourceOrigin.OutOfDate],
      VariadicFormData[SourceOrigin.Current],
      Map[Expr, ExpressionResult]] = Table(
      ("input", "output", "expectedExprMap"),
      (
        mkDataManyOutOfDate("rc" -> "0") ++ mkDataOutOfDate("a" -> "10", "b" -> "11"),
        mkDataManyCurrent("rc"   -> "0") ++ mkDataCurrent("a"   -> "10", "b" -> "11", "res" -> "10.00"),
        Map(
          ctx("a") -> NumberResult(10.00),
          ctx("b") -> Hidden,
        )),
      (
        mkDataManyOutOfDate("rc" -> "1") ++ mkDataOutOfDate("a" -> "10", "b" -> "11"),
        mkDataManyCurrent("rc"   -> "1") ++ mkDataCurrent("a"   -> "10", "b" -> "11", "res" -> "11.00"),
        Map(
          ctx("a") -> Hidden,
          ctx("b") -> NumberResult(11.00),
        )),
      (
        mkDataManyOutOfDate("rc" -> "0,1") ++ mkDataOutOfDate("a" -> "10", "b" -> "11"),
        mkDataManyCurrent("rc"   -> "0,1") ++ mkDataCurrent("a"   -> "10", "b" -> "11", "res" -> "21.00"),
        Map(
          ctx("a") -> NumberResult(10.00),
          ctx("b") -> NumberResult(11.00),
        ))
    )

    val sections =
      mkSection(
        List(mkFormComponent(
          "rc",
          RevealingChoice(
            List(
              RevealingChoiceElement(toSmartString("Yes"), mkFormComponent("a", Value, sterling) :: Nil, false),
              RevealingChoiceElement(toSmartString("No"), mkFormComponent("b", Value, sterling) :: Nil, false)
            ),
            true
          )
        ))) ::
        mkSection(List(mkFormComponent("res", Add(ctx("a"), ctx("b")), sterling))) ::
        Nil

    forAll(data) { (input, expectedOutput, expectedExprMap) ⇒
      verify(input, expectedOutput, expectedExprMap, sections)
    }
  }

  it should "not recalculate editable field" in {

    val formComponentIds: TableFor3[
      VariadicFormData[SourceOrigin.OutOfDate],
      VariadicFormData[SourceOrigin.Current],
      Map[Expr, ExpressionResult]] = Table(
      ("input", "output", "expectedExprMap"),
      (
        mkDataOutOfDate("a" -> "1"),
        mkDataCurrent("a"   -> "1"),
        Map(
          ctx("a") -> NumberResult(1.00),
          ctx("b") -> Empty,
        )),
      (
        mkDataOutOfDate("a" -> "1", "b" -> ""),
        mkDataCurrent("a"   -> "1", "b" -> ""),
        Map(
          ctx("a") -> NumberResult(1.00),
          ctx("b") -> Invalid("Number - cannot convert '' to number"),
        )),
      (
        mkDataOutOfDate("a" -> "1", "b" -> "10"),
        mkDataCurrent("a"   -> "1", "b" -> "10"),
        Map(
          ctx("a") -> NumberResult(1.00),
          ctx("b") -> NumberResult(10.00),
        )),
      (
        mkDataOutOfDate("a" -> "1", "b" -> "10", "c" -> "12"),
        mkDataCurrent("a"   -> "1", "b" -> "10", "c" -> "12"),
        Map(
          ctx("a") -> NumberResult(1.00),
          ctx("b") -> NumberResult(10.00),
        ))
    )

    val sections =
      mkSection(List(mkFormComponentEditable("a", Value, sterling))) ::
        mkSection(List(mkFormComponentEditable("b", Value, sterling))) ::
        mkSection(List(mkFormComponentEditable("c", Add(ctx("a"), ctx("b")), sterling))) :: Nil

    forAll(formComponentIds) { (input, expectedOutput, expectedExprMap) ⇒
      verify(input, expectedOutput, expectedExprMap, sections)
    }
  }

  it should "complex recalculation with 2 simple sections containing includeIf" in {
    val formComponentIds = Table(
      ("input", "output", "expectedExprMap"),
      (
        mkDataManyOutOfDate("a" -> "1"),
        mkDataManyCurrent("a"   -> "1"),
        Map(
          const("0") -> NumberResult(0),
          ctx("a")   -> OptionResult(List(1)),
          ctx("b")   -> Hidden,
          ctx("c")   -> Hidden,
          ctx("d")   -> Hidden,
        )),
      (
        mkDataManyOutOfDate("a" -> "0"),
        mkDataManyCurrent("a"   -> "0") ++ mkDataCurrent("e" -> ""),
        Map(
          const("0") -> NumberResult(0),
          ctx("a")   -> OptionResult(List(0)),
          ctx("b")   -> Empty,
          ctx("c")   -> Empty,
          ctx("d")   -> Hidden,
        )),
      (
        mkDataManyOutOfDate("a" -> "0", "b" -> "1"),
        mkDataManyCurrent("a"   -> "0", "b" -> "1") ++ mkDataCurrent("e" -> ""),
        Map(
          const("0") -> NumberResult(0),
          ctx("a")   -> OptionResult(List(0)),
          ctx("b")   -> OptionResult(List(1)),
          ctx("c")   -> Empty,
          ctx("d")   -> Hidden,
        )),
      (
        mkDataManyOutOfDate("a" -> "0", "b" -> "1") ++ mkDataOutOfDate("c" -> "10"),
        mkDataManyCurrent("a"   -> "0", "b" -> "1") ++ mkDataCurrent("c"   -> "10", "e" -> "10.00"),
        Map(
          const("0") -> NumberResult(0),
          ctx("a")   -> OptionResult(List(0)),
          ctx("b")   -> OptionResult(List(1)),
          ctx("c")   -> NumberResult(10.00),
          ctx("d")   -> Hidden,
        )),
      (
        mkDataManyOutOfDate("a" -> "0", "b" -> "0"),
        mkDataManyCurrent("a"   -> "0", "b" -> "0") ++ mkDataCurrent("e" -> ""),
        Map(
          const("0") -> NumberResult(0),
          ctx("a")   -> OptionResult(List(0)),
          ctx("b")   -> OptionResult(List(0)),
          ctx("c")   -> Empty,
          ctx("d")   -> Empty,
        )),
      (
        mkDataManyOutOfDate("a" -> "0", "b" -> "0") ++ mkDataOutOfDate("c" -> "10", "d" -> "11"),
        mkDataManyCurrent("a"   -> "0", "b" -> "0") ++ mkDataCurrent("c"   -> "10", "d" -> "11", "e" -> "21.00"),
        Map(
          const("0") -> NumberResult(0),
          ctx("a")   -> OptionResult(List(0)),
          ctx("b")   -> OptionResult(List(0)),
          ctx("c")   -> NumberResult(10.00),
          ctx("d")   -> NumberResult(11.00),
        ))
    )

    val choice =
      Choice(YesNo, NonEmptyList.of(toSmartString("yes"), toSmartString("no")), Vertical, List.empty, None)

    val includeIf1 = IncludeIf(Contains(ctx("a"), Constant("0")))
    val includeIf2 = IncludeIf(Contains(ctx("b"), Constant("0")))

    val sections =
      mkSection(List(mkFormComponent("a", choice))) ::
        mkSectionIncludeIf(List(mkFormComponent("b", choice), mkFormComponent("c", Value, sterling)), includeIf1) ::
        mkSectionIncludeIf(List(mkFormComponent("d", Value, sterling)), includeIf2) ::
        mkSection(List(mkFormComponent("e", Add(ctx("c"), ctx("d")), sterling))) :: Nil

    forAll(formComponentIds) { (input, expectedOutput, expectedExprMap) ⇒
      verify(input, expectedOutput, expectedExprMap, sections)
    }

  }

  it should "complex recalculation with 3 complex sections containing includeIf" in {
    val formComponentIds = Table(
      ("input", "output", "expectedExprMap"),
      (
        mkDataManyOutOfDate("a" -> "1", "b" -> "1") ++ mkDataOutOfDate("bb" -> "10", "d" -> "10"),
        mkDataManyCurrent("a"   -> "1", "b" -> "1") ++ mkDataCurrent("bb"   -> "10", "d" -> "10", "e" -> ""),
        Map(
          const("1") -> NumberResult(1),
          ctx("a")   -> OptionResult(List(1)),
          ctx("b")   -> OptionResult(List(1)),
          ctx("c")   -> Empty,
          ctx("cc")  -> Empty,
          ctx("d")   -> Hidden,
        )),
      (
        mkDataManyOutOfDate("a" -> "1", "b" -> "1") ++ mkDataOutOfDate("bb" -> "10", "cc" -> "10", "d" -> "10"),
        mkDataManyCurrent("a"   -> "1", "b" -> "1") ++ mkDataCurrent(
          "bb"                  -> "10",
          "cc"                  -> "10",
          "d"                   -> "10",
          "e"                   -> "10.00"),
        Map(
          const("1") -> NumberResult(1),
          ctx("a")   -> OptionResult(List(1)),
          ctx("b")   -> OptionResult(List(1)),
          ctx("c")   -> Empty,
          ctx("cc")  -> NumberResult(10.00),
          ctx("d")   -> Hidden,
        )),
      (
        mkDataManyOutOfDate("a" -> "1", "b" -> "0") ++ mkDataOutOfDate("bb" -> "10", "d" -> "10"),
        mkDataManyCurrent("a"   -> "1", "b" -> "0") ++ mkDataCurrent("bb"   -> "10", "d" -> "10"),
        Map(
          const("1") -> NumberResult(1),
          ctx("a")   -> OptionResult(List(1)),
          ctx("b")   -> OptionResult(List(0)),
          ctx("c")   -> Hidden,
          ctx("cc")  -> Hidden,
          ctx("d")   -> Hidden,
        )),
      (
        mkDataManyOutOfDate("a" -> "1", "b" -> "1", "c" -> "0") ++ mkDataOutOfDate(
          "bb"                  -> "10",
          "cc"                  -> "10",
          "d"                   -> "10"),
        mkDataManyCurrent("a"   -> "1", "b" -> "1", "c" -> "0") ++ mkDataCurrent(
          "bb"                  -> "10",
          "cc"                  -> "10",
          "d"                   -> "10",
          "e"                   -> "10.00"),
        Map(
          const("1") -> NumberResult(1),
          ctx("a")   -> OptionResult(List(1)),
          ctx("b")   -> OptionResult(List(1)),
          ctx("c")   -> OptionResult(List(0)),
          ctx("cc")  -> NumberResult(10.00),
          ctx("d")   -> Hidden,
        )),
      (
        mkDataManyOutOfDate("a" -> "1", "b" -> "1", "c" -> "1") ++ mkDataOutOfDate(
          "bb"                  -> "10",
          "cc"                  -> "10",
          "d"                   -> "10"),
        mkDataManyCurrent("a"   -> "1", "b" -> "1", "c" -> "1") ++ mkDataCurrent(
          "bb"                  -> "10",
          "cc"                  -> "10",
          "d"                   -> "10",
          "e"                   -> "20.00"),
        Map(
          const("1") -> NumberResult(1),
          ctx("a")   -> OptionResult(List(1)),
          ctx("b")   -> OptionResult(List(1)),
          ctx("c")   -> OptionResult(List(1)),
          ctx("cc")  -> NumberResult(10.00),
          ctx("d")   -> NumberResult(10.00),
        )),
      (
        mkDataManyOutOfDate("a" -> "0", "b" -> "1", "c" -> "1") ++ mkDataOutOfDate(
          "bb"                  -> "10",
          "cc"                  -> "10",
          "d"                   -> "10"),
        mkDataManyCurrent("a"   -> "0", "b" -> "1", "c" -> "1") ++ mkDataCurrent("bb" -> "10", "cc" -> "10", "d" -> "10"),
        Map(
          const("1") -> NumberResult(1),
          ctx("a")   -> OptionResult(List(0)),
          ctx("b")   -> Hidden,
          ctx("bb")  -> Hidden,
          ctx("c")   -> Hidden,
          ctx("cc")  -> Hidden,
          ctx("d")   -> Hidden,
        ))
    )

    val choice =
      Choice(YesNo, NonEmptyList.of(toSmartString("yes"), toSmartString("no")), Vertical, List.empty, None)

    val includeIf1 = IncludeIf(Contains(ctx("a"), Constant("1")))
    val includeIf2 = IncludeIf(Contains(ctx("b"), Constant("1")))
    val includeIf3 = IncludeIf(Contains(ctx("c"), Constant("1")))

    val sections =
      mkSection(List(mkFormComponent("a", choice))) ::
        mkSectionIncludeIf(List(mkFormComponent("b", choice), mkFormComponent("bb", Value)), includeIf1) ::
        mkSectionIncludeIf(List(mkFormComponent("c", choice), mkFormComponent("cc", Value, sterling)), includeIf2) ::
        mkSectionIncludeIf(List(mkFormComponent("d", Value, sterling)), includeIf3) ::
        mkSection(List(mkFormComponent("e", Add(ctx("cc"), ctx("d")), sterling))) :: Nil

    forAll(formComponentIds) { (input, expectedOutput, expectedExprMap) ⇒
      verify(input, expectedOutput, expectedExprMap, sections)
    }
  }

  it should "handle else expression" in {
    val formComponentIds: TableFor3[
      VariadicFormData[SourceOrigin.OutOfDate],
      VariadicFormData[SourceOrigin.Current],
      Map[Expr, ExpressionResult]] = Table(
      ("input", "output", "expectedExprMap"),
      (
        mkDataOutOfDate("a" -> "A", "b" -> "B", "c" -> "C"),
        mkDataCurrent("a"   -> "A", "b" -> "B", "c" -> "C", "z" -> "A"),
        Map(
          ctx("a") -> StringResult("A"),
          ctx("b") -> StringResult("B"),
          ctx("c") -> StringResult("C"),
        )),
      (
        mkDataOutOfDate("b" -> "B", "c" -> "C"),
        mkDataCurrent("b"   -> "B", "c" -> "C", "z" -> "B"),
        Map(
          ctx("a") -> Empty,
          ctx("b") -> StringResult("B"),
          ctx("c") -> StringResult("C"),
        )),
      (
        mkDataOutOfDate("c" -> "C"),
        mkDataCurrent("c"   -> "C", "z" -> "C"),
        Map(
          ctx("a") -> Empty,
          ctx("b") -> Empty,
          ctx("c") -> StringResult("C"),
        )),
      (
        mkDataOutOfDate("a" -> "A", "b" -> "B", "c" -> "C"),
        mkDataCurrent("a"   -> "A", "b" -> "B", "c" -> "C", "z" -> "A"),
        Map(
          ctx("a") -> StringResult("A"),
          ctx("b") -> StringResult("B"),
          ctx("c") -> StringResult("C"),
        )),
      (
        mkDataOutOfDate("a" -> "", "b" -> "B", "c" -> "C"),
        mkDataCurrent("a"   -> "", "b" -> "B", "c" -> "C", "z" -> "B"),
        Map(
          ctx("a") -> Empty,
          ctx("b") -> StringResult("B"),
          ctx("c") -> StringResult("C"),
        )),
      (
        mkDataOutOfDate("a" -> "", "b" -> "", "c" -> "C"),
        mkDataCurrent("a"   -> "", "b" -> "", "c" -> "C", "z" -> "C"),
        Map(
          ctx("a") -> Empty,
          ctx("b") -> Empty,
          ctx("c") -> StringResult("C"),
        ))
    )

    val sections =
      mkSection(List(mkFormComponent("a", Value), mkFormComponent("b", Value), mkFormComponent("c", Value))) ::
        mkSection(List(mkFormComponent("z", Else(ctx("a"), Else(ctx("b"), ctx("c")))))) :: Nil

    forAll(formComponentIds) { (input, expectedOutput, expectedExprMap) ⇒
      verify(input, expectedOutput, expectedExprMap, sections)
    }
  }

  it should "handle invisible fields in else expression" in {
    val formComponentIds: TableFor3[
      VariadicFormData[SourceOrigin.OutOfDate],
      VariadicFormData[SourceOrigin.Current],
      Map[Expr, ExpressionResult]] = Table(
      ("input", "output", "expectedExprMap"),
      (
        mkDataOutOfDate("a" -> "A", "b" -> "B", "c" -> "C"),
        mkDataCurrent("a"   -> "A", "b" -> "B", "c" -> "C", "z" -> "B"),
        Map(
          const("A") -> StringResult("A"),
          ctx("a")   -> StringResult("A"),
          ctx("b")   -> StringResult("B"),
          ctx("c")   -> StringResult("C"))),
      (
        mkDataOutOfDate("b" -> "B", "c"                    -> "C"),
        mkDataCurrent("b"   -> "B", "c"                    -> "C", "z" -> "C"),
        Map(const("A")      -> StringResult("A"), ctx("a") -> Empty, ctx("b") -> Hidden, ctx("c") -> StringResult("C")))
    )

    val includeIf1 = IncludeIf(Equals(FormCtx(FormComponentId("a")), Constant("A")))

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSectionIncludeIf(List(mkFormComponent("b", Value)), includeIf1) ::
        mkSection(List(mkFormComponent("c", Value))) ::
        mkSection(List(mkFormComponent("z", Else(FormCtx(FormComponentId("b")), FormCtx(FormComponentId("c")))))) :: Nil

    forAll(formComponentIds) { (input, expectedOutput, expectedExprMap) ⇒
      verify(input, expectedOutput, expectedExprMap, sections)
    }
  }

  private def verify(
    input: VariadicFormData[SourceOrigin.OutOfDate],
    expectedOutput: VariadicFormData[SourceOrigin.Current],
    expectedExprMap: Map[Expr, ExpressionResult],
    sections: List[Section]
  )(
    implicit position: Position
  ) = {
    val formTemplate: FormTemplate = mkFormTemplate(sections)
    val formModelOptics = mkFormModelOptics(formTemplate, input)
    val outputRecData: RecData[SourceOrigin.Current] = formModelOptics.formModelVisibilityOptics.recData
    val output: EvaluationResults = formModelOptics.formModelVisibilityOptics.recalculationResult.evaluationResults

    outputRecData.variadicFormData shouldBe expectedOutput
    output.exprMap shouldBe expectedExprMap
  }
}
