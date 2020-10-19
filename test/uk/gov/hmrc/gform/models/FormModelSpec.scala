/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.data.NonEmptyList
import org.scalatest.{ FlatSpec, Matchers }
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import scala.language.implicitConversions
import uk.gov.hmrc.gform.Helpers.{ toSmartString, toSmartStringExpression }
import uk.gov.hmrc.gform.eval.{ ExprType, TypedExpr }
import uk.gov.hmrc.gform.eval.ExpressionResult._
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class FormModelSpec extends FlatSpec with Matchers with FormModelSupport with VariadicFormDataSupport {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)

  "FormModel" should "handle revealing choice" in {

    val fcA = mkFormComponent("a", Value)
    val fcB = mkFormComponent("b", Value)

    val rcElementA = RevealingChoiceElement(toSmartString("Yes"), fcA :: Nil, false)
    val rcElementB = RevealingChoiceElement(toSmartString("No"), fcB :: Nil, false)

    val sliceA = mkFormComponent("rc", RevealingChoice(Nil, true))
    val sliceB = mkFormComponent("rc", RevealingChoice(List(rcElementA), true))
    val sliceC = mkFormComponent("rc", RevealingChoice(List(rcElementB), true))

    val rc = mkFormComponent("rc", RevealingChoice(List(rcElementA, rcElementB), true))

    val section1 = mkSection(List(rc))

    val sections = List(
      section1
    )

    val fmb = mkFormModelFromSections(sections)

    val table = Table(
      ("data", "expected"),
      (variadicFormDataMany(), sliceA :: Nil),
      (variadicFormDataMany("rc" -> List(0)), sliceB :: Nil),
      (variadicFormDataMany("rc" -> List(1)), sliceC :: Nil),
      (variadicFormDataMany("rc" -> List(0, 1)), rc :: Nil)
    )

    forAll(table) {
      case (data, expectedFcs) =>
        val expectedPage = Page[Visibility](
          toSmartString("Section Name"),
          None,
          None,
          None,
          None,
          None,
          expectedFcs,
          None,
          None
        )

        val expected: FormModel[Visibility] = FormModel.fromPages(List(Singleton(expectedPage, section1)))

        val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
          fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data)

        res.formModel shouldBe expected
    }
  }

  it should "use recalculate and assign type to the expressions" in {

    val fcA = mkFormComponent("a", Text(PositiveNumber(), Value))
    val fcB = mkFormComponent("b", Text(PositiveNumber(), Value))
    val fcC = mkFormComponent("c", Text(Sterling(RoundingMode.Up, true), Add(FormCtx("a"), FormCtx("b"))))
      .copy(label = toSmartStringExpression("", Add(FormCtx("a"), FormCtx("b"))))

    val section1 = mkSection(List(fcA))
    val section2 = mkSection(List(fcB))
    val section3 = mkSection(List(fcC))

    val sections = List(
      section1,
      section2,
      section3
    )

    val fmb = mkFormModelFromSections(sections)

    val table = Table(
      ("data", "expected"),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "10", "b" -> "20", "c" -> "5"),
        Map(
          TypedExpr.number(FormCtx("a"))                    -> NumberResult(10),
          TypedExpr.number(FormCtx("b"))                    -> NumberResult(20),
          TypedExpr.number(Add(FormCtx("a"), FormCtx("b"))) -> NumberResult(30),
          TypedExpr.number(Add(FormCtx("a"), FormCtx("b"))) -> NumberResult(30)
        )
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "dssd", "b" -> "20", "c" -> "5"),
        Map(
          TypedExpr.number(FormCtx("a"))                    -> Invalid("Number - cannot convert 'dssd' to number"),
          TypedExpr.number(FormCtx("b"))                    -> NumberResult(20),
          TypedExpr.number(Add(FormCtx("a"), FormCtx("b"))) -> Invalid("Number - cannot convert 'dssd' to number"),
          TypedExpr.number(Add(FormCtx("a"), FormCtx("b"))) -> Invalid("Number - cannot convert 'dssd' to number")
        )
      )
    )

    forAll(table) {
      case (data, expected) =>
        val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
          fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data)
        res.evaluationResults.exprMap shouldBe expected
    }
  }

  it should "use recalculate and assign type to the expressions 2" in {

    val fcA = mkFormComponent("a", Text(BasicText, Value))
    val fcB = mkFormComponent("b", Text(BasicText, Value))
    val fcC = mkFormComponent("c", Text(Sterling(RoundingMode.Up, true), Add(FormCtx("a"), FormCtx("b"))))
      .copy(label = toSmartStringExpression("", Add(FormCtx("a"), FormCtx("b"))))

    val section1 = mkSection(List(fcA))
    val section2 = mkSection(List(fcB))
    val section3 = mkSection(List(fcC))

    val sections = List(
      section1,
      section2,
      section3
    )

    val fmb = mkFormModelFromSections(sections)

    val table = Table(
      ("data", "expected"),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "10", "b" -> "20", "c" -> "5"),
        Map(
          TypedExpr.string(FormCtx("a"))                    -> StringResult("10"),
          TypedExpr.string(FormCtx("b"))                    -> StringResult("20"),
          TypedExpr.string(Add(FormCtx("a"), FormCtx("b"))) -> StringResult("1020"),
          TypedExpr.number(Add(FormCtx("a"), FormCtx("b"))) -> NumberResult(30)
        )
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "Hello", "b" -> "World", "c" -> "5"),
        Map(
          TypedExpr.string(FormCtx("a"))                    -> StringResult("Hello"),
          TypedExpr.string(FormCtx("b"))                    -> StringResult("World"),
          TypedExpr.string(Add(FormCtx("a"), FormCtx("b"))) -> StringResult("HelloWorld"),
          TypedExpr.number(Add(FormCtx("a"), FormCtx("b"))) -> Invalid("Number - cannot convert 'World' to number")
        )
      )
    )

    forAll(table) {
      case (data, expected) =>
        val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
          fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data)
        res.evaluationResults.exprMap shouldBe expected
    }
  }

  it should "handle Constant expression according to data it wraps" in {
    val table = Table(
      ("expression", "expected"),
      (
        Constant("A"),
        Map(
          TypedExpr.string(Constant("A")) -> StringResult("A")
        )
      ),
      (
        Constant("1"),
        Map(
          TypedExpr.number(Constant("1")) -> NumberResult(1)
        )
      ),
      (
        Constant("1.2"),
        Map(
          TypedExpr.number(Constant("1.2")) -> NumberResult(1.2)
        )
      ),
      (
        Add(Constant("1.2"), Constant("2")),
        Map(
          TypedExpr.number(Constant("1.2"))                     -> NumberResult(1.2),
          TypedExpr.number(Constant("2"))                       -> NumberResult(2),
          TypedExpr.number(Add(Constant("1.2"), Constant("2"))) -> NumberResult(3.2)
        )
      ),
      /* (
       *   Add(Constant("1"), Constant("2.2")),
       *   Map(
       *     TypedExpr.number(Constant("2.2")) -> NumberResult(2.2),
       *     TypedExpr.number(Constant("1"))   -> NumberResult(1),
       *     TypedExpr.number(Add(Constant("1"), Constant("2.2"))) -> Invalid(
       *       "WholeNumber - cannot convert '2.2' to wholeNumber")
       *   )
       * ), */
      (
        Add(Constant("1"), Constant("2")),
        Map(
          TypedExpr.number(Constant("1"))                     -> NumberResult(1),
          TypedExpr.number(Constant("2"))                     -> NumberResult(2),
          TypedExpr.number(Add(Constant("1"), Constant("2"))) -> NumberResult(3)
        )
      ),
      (
        Add(Constant("A"), Constant("B")),
        Map(
          TypedExpr.string(Constant("A"))                     -> StringResult("A"),
          TypedExpr.string(Constant("B"))                     -> StringResult("B"),
          TypedExpr.string(Add(Constant("A"), Constant("B"))) -> StringResult("AB")
        )
      ),
      (
        Add(Constant("1"), Constant("A")),
        Map(
          TypedExpr.number(Constant("1"))                     -> NumberResult(1),
          TypedExpr.string(Constant("A"))                     -> StringResult("A"),
          TypedExpr.number(Add(Constant("1"), Constant("A"))) -> Invalid("Number - cannot convert 'A' to number")
        )
      ),
      (
        Add(Constant(""), Add(Constant("1"), Constant("A"))),
        Map(
          TypedExpr.number(Constant("1"))                                        -> NumberResult(1),
          TypedExpr.string(Constant(""))                                         -> StringResult(""),
          TypedExpr.string(Constant("A"))                                        -> StringResult("A"),
          TypedExpr.string(Add(Constant(""), Add(Constant("1"), Constant("A")))) -> StringResult("1A")
        )
      ),
      (
        Add(Constant("A"), Constant("1")),
        Map(
          TypedExpr.number(Constant("1"))                     -> NumberResult(1),
          TypedExpr.string(Constant("A"))                     -> StringResult("A"),
          TypedExpr.string(Add(Constant("A"), Constant("1"))) -> StringResult("A1")
        )
      )
    )

    forAll(table) {
      case (expression, expected) =>
        val fcA = mkFormComponent("a", Value).copy(label = toSmartStringExpression("", expression))
        val section1 = mkSection(fcA)
        val sections = List(
          section1
        )
        val fmb = mkFormModelFromSections(sections)
        val data = variadicFormData[SourceOrigin.OutOfDate]()
        val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
          fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data)

        res.evaluationResults.exprMap shouldBe expected
    }
  }

  it should "handle Constant expression respecting format of component" in {
    val table = Table(
      ("textConstraint", "expected", "expectedVariadicData"),
      (
        PositiveNumber(maxFractionalDigits = 0),
        Map(
          TypedExpr.number(Constant("123"))                    -> NumberResult(123),
          TypedExpr.number(Constant("456"))                    -> NumberResult(456),
          TypedExpr.number(FormCtx("a"))                       -> NumberResult(123),
          TypedExpr.number(Add(FormCtx("a"), Constant("456"))) -> NumberResult(579)
        ),
        variadicFormData[SourceOrigin.Current]("a" -> "123")
      ),
      (
        BasicText,
        Map(
          TypedExpr.string(Constant("123"))                    -> StringResult("123"),
          TypedExpr.number(Constant("123"))                    -> NumberResult(123),
          TypedExpr.number(Constant("456"))                    -> NumberResult(456),
          TypedExpr.string(FormCtx("a"))                       -> StringResult("123"),
          TypedExpr.string(Add(FormCtx("a"), Constant("456"))) -> StringResult("123456")
        ),
        variadicFormData[SourceOrigin.Current]("a" -> "123")
      )
    )

    forAll(table) {
      case (textConstraint, expected, expectedVariadicData) =>
        val fcA = mkFormComponent("a", Text(textConstraint, Constant("123")))
        val fcB =
          mkFormComponent("b", Value).copy(label = toSmartStringExpression("", Add(FormCtx("a"), Constant("456"))))
        val section1 = mkSection(fcA)
        val section2 = mkSection(fcB)
        val sections = List(
          section1,
          section2
        )
        val fmb = mkFormModelFromSections(sections)
        val data = variadicFormData[SourceOrigin.OutOfDate]()

        val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
          fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data)

        res.evaluationResults.exprMap shouldBe expected
        res.recData.variadicFormData shouldBe expectedVariadicData

    }

  }

  it should "handle includeIf" in {

    val fcA = mkFormComponent("a", Text(PositiveNumber(maxFractionalDigits = 0), Constant("123")))
    val fcA2 =
      mkFormComponent("a2", Choice(Radio, NonEmptyList.one(toSmartString("Option A")), Vertical, List.empty[Int], None))
    val fcB = mkFormComponent("b", Text(BasicText, Constant("456")))
    val fcC = mkFormComponent("c", Text(BasicText, Value))
    val fcD = mkFormComponent("d", Text(BasicText, FormCtx("c")))

    val includeIf = IncludeIf(Contains(FormCtx("a2"), Constant("0")))
    val section1 = mkSection(fcA, fcA2)
    val section2 = mkSection(fcB)
    val section3 = mkSectionIncludeIf(List(fcC), includeIf)
    val section4 = mkSection(fcD)

    val sections = List(
      section1,
      section2,
      section3,
      section4
    )

    val expectedPageA = mkPage(fcA :: fcA2 :: Nil)
    val expectedPageB = mkPage(fcB :: Nil)
    val expectedPageC = mkPageIncludeIf(fcC :: Nil, includeIf)
    val expectedPageD = mkPage(fcD :: Nil)

    val fmb = mkFormModelFromSections(sections)

    val table = Table(
      ("data", "expected", "expectedFormModel"),
      (
        variadicFormData[SourceOrigin.OutOfDate](),
        Map(
          TypedExpr.number(Constant("123"))        -> NumberResult(123),
          TypedExpr.string(Constant("456"))        -> StringResult("456"),
          TypedExpr.number(Constant("456"))        -> NumberResult(456),
          TypedExpr.number(Constant("0"))          -> NumberResult(0),
          TypedExpr.choiceSelection(FormCtx("a2")) -> Empty,
          TypedExpr.string(FormCtx("c"))           -> Hidden
        ),
        List(
          (Singleton(expectedPageA, section1), 0),
          (Singleton(expectedPageB, section2), 1),
          (Singleton(expectedPageD, section4), 3)
        )
      ),
      (
        variadicFormDataMany("a2" -> List(0)) ++ variadicFormData[SourceOrigin.OutOfDate]("c" -> "X"),
        Map(
          TypedExpr.number(Constant("123"))        -> NumberResult(123),
          TypedExpr.string(Constant("456"))        -> StringResult("456"),
          TypedExpr.number(Constant("456"))        -> NumberResult(456),
          TypedExpr.number(Constant("0"))          -> NumberResult(0),
          TypedExpr.choiceSelection(FormCtx("a2")) -> OptionResult(List(0)),
          TypedExpr.string(FormCtx("c"))           -> StringResult("X")
        ),
        List(
          (Singleton(expectedPageA, section1), 0),
          (Singleton(expectedPageB, section2), 1),
          (Singleton(expectedPageC, section3), 2),
          (Singleton(expectedPageD, section4), 3)
        )
      )
    )

    forAll(table) {
      case (data, expected, expectedPages) =>
        val expectedFormModel: FormModel[Visibility] = fromPagesWithIndex(expectedPages)

        val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
          fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data)

        res.evaluationResults.exprMap shouldBe expected
        res.formModel shouldBe expectedFormModel
    }
  }

  it should "handle includeIf with reference to formComponent with WholeNumber format" in {

    val fcA = mkFormComponent("a", Text(PositiveNumber(maxFractionalDigits = 0), Constant("123")))
    val fcB =
      mkFormComponent("b", Choice(Radio, NonEmptyList.one(toSmartString("Option A")), Vertical, List.empty[Int], None))
    val fcC = mkFormComponent("c", Text(BasicText, Value))
    val fcD = mkFormComponent("d", Text(BasicText, Value))

    val includeIf = IncludeIf(Contains(FormCtx("b"), FormCtx("a")))
    val section1 = mkSection(fcA, fcB)
    val section2 = mkSectionIncludeIf(List(fcC), includeIf)
    val section3 = mkSection(fcD)

    val sections = List(
      section1,
      section2,
      section3
    )

    val expectedPage1 = mkPage(fcA :: fcB :: Nil)
    val expectedPage2 = mkPageIncludeIf(fcC :: Nil, includeIf)
    val expectedPage3 = mkPage(fcD :: Nil)

    val fmb = mkFormModelFromSections(sections)

    val table = Table(
      ("data", "expected", "expectedFormModel"),
      (
        variadicFormDataMany("b" -> List(123)),
        Map(
          TypedExpr.number(Constant("123"))       -> NumberResult(123),
          TypedExpr.number(FormCtx("a"))          -> NumberResult(123),
          TypedExpr.choiceSelection(FormCtx("b")) -> OptionResult(List(123))
        ),
        List(
          (Singleton(expectedPage1, section1), 0),
          (Singleton(expectedPage2, section2), 1),
          (Singleton(expectedPage3, section3), 2)
        )
      ),
      (
        variadicFormDataMany("b" -> List(124)),
        Map(
          TypedExpr.number(Constant("123"))       -> NumberResult(123),
          TypedExpr.number(FormCtx("a"))          -> NumberResult(123),
          TypedExpr.choiceSelection(FormCtx("b")) -> OptionResult(List(124)),
          TypedExpr.string(FormCtx("c"))          -> Hidden
        ),
        List(
          (Singleton(expectedPage1, section1), 0),
          (Singleton(expectedPage3, section3), 2)
        )
      )
    )

    forAll(table) {
      case (data, expected, expectedPages) =>
        val expectedFormModel: FormModel[Visibility] = fromPagesWithIndex(expectedPages)

        val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
          fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data)

        res.evaluationResults.exprMap shouldBe expected
        res.formModel shouldBe expectedFormModel
    }
  }

  it should "handle contains in includeIf with reference to formComponent with NonWholeNumber format" in {

    val fcA = mkFormComponent("a", Text(PositiveNumber(maxFractionalDigits = 2), Constant("123.45")))
    val fcB =
      mkFormComponent("b", Choice(Radio, NonEmptyList.one(toSmartString("Option A")), Vertical, List.empty[Int], None))
    val fcC = mkFormComponent("c", Text(BasicText, Value))
    val fcD = mkFormComponent("d", Text(BasicText, Value))

    val includeIf = IncludeIf(Contains(FormCtx("b"), FormCtx("a")))
    val section1 = mkSection(fcA, fcB)
    val section2 = mkSectionIncludeIf(List(fcC), includeIf)
    val section3 = mkSection(fcD)

    val sections = List(
      section1,
      section2,
      section3
    )

    val expectedPage1 = mkPage(fcA :: fcB :: Nil)
    val expectedPage2 = mkPageIncludeIf(fcC :: Nil, includeIf)
    val expectedPage3 = mkPage(fcD :: Nil)

    val fmb = mkFormModelFromSections(sections)

    val table = Table(
      ("data", "expected", "expectedFormModel"),
      (
        variadicFormDataMany("b" -> List(123)),
        Map(
          TypedExpr.number(Constant("123.45"))    -> NumberResult(123.45),
          TypedExpr.number(FormCtx("a"))          -> NumberResult(123.45),
          TypedExpr.choiceSelection(FormCtx("b")) -> OptionResult(List(123))
        ),
        List(
          (Singleton(expectedPage1, section1), 0),
          (Singleton(expectedPage2, section2), 1),
          (Singleton(expectedPage3, section3), 2)
        )
      )
    )

    forAll(table) {
      case (data, expected, expectedPages) =>
        val expectedFormModel: FormModel[Visibility] = fromPagesWithIndex(expectedPages)

        val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
          fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data)

        res.evaluationResults.exprMap shouldBe expected
        res.formModel shouldBe expectedFormModel
    }
  }

  it should "use recalculate graph bla bla" in {

    val fcA = mkFormComponent("a", Value)
    val fcB = mkFormComponent("b", Value)
    val fcC = mkFormComponent("c", Value)
    val fcD = mkFormComponent("d", Value)
    val fcE = mkFormComponent("e", Value)
    val fcF = mkFormComponent("f", Else(FormCtx("c"), FormCtx("d")))
    val fcG = mkFormComponent("g", Value).copy(label = toSmartStringExpression("", Else(FormCtx("a"), FormCtx("b"))))

    val includeIf1 = IncludeIf(Equals(FormCtx("a"), FormCtx("b")))
    val includeIf2 = IncludeIf(Equals(FormCtx("b"), Constant("WORLD")))

    val section1 = mkSection(List(fcA))
    val section2 = mkSection(List(fcB))
    val section3 = mkSectionIncludeIf(fcC :: Nil, includeIf1)
    val section4 = mkSectionIncludeIf(fcD :: fcE :: Nil, includeIf2)
    val section5 = mkSection(List(fcF))
    val section6 = mkSection(List(fcG))

    val sections = List(
      section1,
      section2,
      section3,
      section4,
      section5,
      section6
    )

    val fmb = mkFormModelFromSections(sections)

    val table = Table(
      ("data", "expected"),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "HELLO", "b" -> "WORLD2", "c" -> "C", "e" -> "E", "d" -> "D"),
        Map(
          TypedExpr.string(FormCtx("a"))                     -> StringResult("HELLO"),
          TypedExpr.string(FormCtx("b"))                     -> StringResult("WORLD2"),
          TypedExpr.string(FormCtx("c"))                     -> Hidden,
          TypedExpr.string(FormCtx("d"))                     -> Hidden,
          TypedExpr.string(FormCtx("e"))                     -> Hidden,
          TypedExpr.string(Constant("WORLD"))                -> StringResult("WORLD"),
          TypedExpr.string(Else(FormCtx("a"), FormCtx("b"))) -> StringResult("HELLO"),
          TypedExpr.string(Else(FormCtx("c"), FormCtx("d"))) -> Hidden
        )),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "HELLO", "b" -> "HELLO", "c" -> "C", "e" -> "E", "d" -> "D"),
        Map(
          TypedExpr.string(FormCtx("a"))                     -> StringResult("HELLO"),
          TypedExpr.string(FormCtx("b"))                     -> StringResult("HELLO"),
          TypedExpr.string(FormCtx("c"))                     -> StringResult("C"),
          TypedExpr.string(FormCtx("d"))                     -> Hidden,
          TypedExpr.string(FormCtx("e"))                     -> Hidden,
          TypedExpr.string(Constant("WORLD"))                -> StringResult("WORLD"),
          TypedExpr.string(Else(FormCtx("a"), FormCtx("b"))) -> StringResult("HELLO"),
          TypedExpr.string(Else(FormCtx("c"), FormCtx("d"))) -> StringResult("C")
        )
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "HELLO", "b" -> "WORLD", "c" -> "C", "e" -> "E", "d" -> "D"),
        Map(
          TypedExpr.string(FormCtx("a"))                     -> StringResult("HELLO"),
          TypedExpr.string(FormCtx("b"))                     -> StringResult("WORLD"),
          TypedExpr.string(FormCtx("c"))                     -> Hidden,
          TypedExpr.string(FormCtx("d"))                     -> StringResult("D"),
          TypedExpr.string(Constant("WORLD"))                -> StringResult("WORLD"),
          TypedExpr.string(Else(FormCtx("a"), FormCtx("b"))) -> StringResult("HELLO"),
          TypedExpr.string(Else(FormCtx("c"), FormCtx("d"))) -> StringResult("D")
        )
      )
    )

    forAll(table) {
      case (data, expected) =>
        val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
          fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data)
        res.evaluationResults.exprMap shouldBe expected
    }
  }

  "toTypedExpr" should "infer type of an expresion" in {
    val fcA = mkFormComponent("a", Text(BasicText, Value))
    val fcB = mkFormComponent("b", Text(PositiveNumber(), Value))
    val fcC = mkFormComponent("c", Else(FormCtx("b"), FormCtx("a")))
    val fcD = mkFormComponent("d", Value).copy(label = toSmartStringExpression("", Else(FormCtx("a"), FormCtx("b"))))

    val section1 = mkSection(List(fcA))
    val section2 = mkSection(List(fcB))
    val section3 = mkSection(List(fcC))
    val section4 = mkSection(List(fcD))

    val sections = List(
      section1,
      section2,
      section3,
      section4
    )

    val fmb = mkFormModelFromSections(sections)

    val data: VariadicFormData[SourceOrigin.OutOfDate] = variadicFormDataMany()

    val fm: FormModel[Visibility] =
      fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data).formModel

    val expr1 = Add(FormCtx("a"), FormCtx("b"))
    val expr2 = Add(FormCtx("b"), FormCtx("a"))

    val table = Table(
      ("expression", "expected"),
      (expr1, TypedExpr(expr1, ExprType.String)),
      (expr2, TypedExpr(expr2, ExprType.Number))
    )

    forAll(table) {
      case (expression, expected) =>
        val typedExpr = fm.toTypedExpr(expression)
        typedExpr shouldBe expected
    }

    val table2 = Table(
      ("expression", "formComponentId", "expected"),
      (expr1, FormComponentId("a"), TypedExpr(expr1, ExprType.String)),
      (expr1, FormComponentId("b"), TypedExpr(expr1, ExprType.Number))
    )

    forAll(table2) {
      case (expression, fcId, expected) =>
        val explicitTypedExpr = fm.explicitTypedExpr(expression, fcId)
        explicitTypedExpr shouldBe expected
    }

  }

  "visibilityModel" should "return visibility model" in {
    val fcA = mkFormComponent("a", Value)
    val fcB = mkFormComponent("b", Value)
    val fcC = mkFormComponent("c", Value)
    val fcD = mkFormComponent("d", Value)
    val fcE = mkFormComponent("e", Value)
    val fcF = mkFormComponent("f", Else(FormCtx("c"), FormCtx("d")))
    val fcG = mkFormComponent("g", Value).copy(label = toSmartStringExpression("", Else(FormCtx("a"), FormCtx("b"))))

    val includeIf1 = IncludeIf(Equals(FormCtx("a"), FormCtx("b")))
    val includeIf2 = IncludeIf(Equals(FormCtx("b"), Constant("WORLD")))

    val section1 = mkSection(List(fcA))
    val section2 = mkSection(List(fcB))
    val section3 = mkSectionIncludeIf(fcC :: Nil, includeIf1)
    val section4 = mkSectionIncludeIf(fcD :: fcE :: Nil, includeIf2)
    val section5 = mkSection(List(fcF))
    val section6 = mkSection(List(fcG))

    val sections = List(
      section1,
      section2,
      section3,
      section4,
      section5,
      section6
    )
    val fmb = mkFormModelFromSections(sections)

    val expectedPageA = mkPage(fcA :: Nil)
    val expectedPageB = mkPage(fcB :: Nil)
    val expectedPageC = mkPageIncludeIf(fcC :: Nil, includeIf1)
    val expectedPageD = mkPageIncludeIf(fcD :: fcE :: Nil, includeIf2)
    val expectedPageF = mkPage(fcF :: Nil)
    val expectedPageG = mkPage(fcG :: Nil)

    val table = Table(
      ("data", "expectedData", "expectedPages"),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "HELLO", "b" -> "WORLD2", "c" -> "C", "e" -> "E", "d" -> "D"),
        variadicFormData[SourceOrigin.Current]("a"   -> "HELLO", "b" -> "WORLD2", "c" -> "C", "e" -> "E", "d" -> "D"),
        List(
          (Singleton(expectedPageA, section1), 0),
          (Singleton(expectedPageB, section2), 1),
          (Singleton(expectedPageF, section5), 4),
          (Singleton(expectedPageG, section6), 5)
        )
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate](
          "a" -> "HELLO",
          "b" -> "HELLO",
          "c" -> "C",
          "d" -> "D",
          "e" -> "E"
        ),
        variadicFormData[SourceOrigin.Current](
          "a" -> "HELLO",
          "b" -> "HELLO",
          "c" -> "C",
          "d" -> "D",
          "e" -> "E",
          "f" -> "C"
        ),
        List(
          (Singleton(expectedPageA, section1), 0),
          (Singleton(expectedPageB, section2), 1),
          (Singleton(expectedPageC, section3), 2),
          (Singleton(expectedPageF, section5), 4),
          (Singleton(expectedPageG, section6), 5)
        )
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate](
          "a" -> "HELLO",
          "b" -> "WORLD",
          "c" -> "C",
          "d" -> "D",
          "e" -> "E"
        ),
        variadicFormData[SourceOrigin.Current](
          "a" -> "HELLO",
          "b" -> "WORLD",
          "c" -> "C",
          "d" -> "D",
          "e" -> "E",
          "f" -> "D"
        ),
        List(
          (Singleton(expectedPageA, section1), 0),
          (Singleton(expectedPageB, section2), 1),
          (Singleton(expectedPageD, section4), 3),
          (Singleton(expectedPageF, section5), 4),
          (Singleton(expectedPageG, section6), 5)
        )
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate](
          "a" -> "WORLD",
          "b" -> "WORLD",
          "c" -> "C",
          "d" -> "D",
          "e" -> "E"
        ),
        variadicFormData[SourceOrigin.Current](
          "a" -> "WORLD",
          "b" -> "WORLD",
          "c" -> "C",
          "d" -> "D",
          "e" -> "E",
          "f" -> "C"
        ),
        List(
          (Singleton(expectedPageA, section1), 0),
          (Singleton(expectedPageB, section2), 1),
          (Singleton(expectedPageC, section3), 2),
          (Singleton(expectedPageD, section4), 3),
          (Singleton(expectedPageF, section5), 4),
          (Singleton(expectedPageG, section6), 5)
        )
      )
    )

    forAll(table) {
      case (data, expectedData, expectedPages) =>
        val visibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo] =
          fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data)

        val expected: FormModel[Visibility] = fromPagesWithIndex(expectedPages)

        visibilityOptics.formModel shouldBe expected
        visibilityOptics.recData.variadicFormData shouldBe expectedData
    }
  }

  private def fromPagesWithIndex[A <: PageMode](pages: List[(PageModel[A], Int)]): FormModel[A] =
    FormModel(pages.map { case (page, index) => page -> SectionNumber(index) })

  private def mkPage(formComponents: List[FormComponent]): Page[Visibility] = Page[Visibility](
    toSmartString("Section Name"),
    None,
    None,
    None,
    None,
    None,
    formComponents,
    None,
    None
  )

  private def mkPageIncludeIf(formComponents: List[FormComponent], includeIf: IncludeIf): Page[Visibility] =
    Page[Visibility](
      toSmartString("Section Name"),
      None,
      None,
      None,
      Some(includeIf),
      None,
      formComponents,
      None,
      None
    )

}
