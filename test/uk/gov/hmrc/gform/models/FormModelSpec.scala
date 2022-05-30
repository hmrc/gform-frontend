/*
 * Copyright 2022 HM Revenue & Customs
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
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import play.api.i18n.Messages
import play.api.test.Helpers

import scala.language.implicitConversions
import uk.gov.hmrc.gform.Helpers.{ toSmartString, toSmartStringExpression }
import uk.gov.hmrc.gform.eval.{ ExprType, RevealingChoiceData, RevealingChoiceInfo, StaticTypeData, StaticTypeInfo, SumInfo }
import uk.gov.hmrc.gform.eval.ExpressionResult._
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SourceOrigin }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }

class FormModelSpec extends AnyFlatSpecLike with Matchers with FormModelSupport with VariadicFormDataSupport {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)
  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages = Helpers.stubMessages(Helpers.stubMessagesApi(Map.empty))

  "FormModel" should "handle revealing choice" in {

    val fcA = mkFormComponent("a", Value)
    val fcB = mkFormComponent("b", Value)

    val rcElementA = RevealingChoiceElement(toOptionData("Yes"), fcA :: Nil, None, false)
    val rcElementB = RevealingChoiceElement(toOptionData("No"), fcB :: Nil, None, false)

    val sliceA = mkFormComponent("rc", RevealingChoice(Nil, true))
    val sliceB = mkFormComponent("rc", RevealingChoice(List(rcElementA), true))
    val sliceC = mkFormComponent("rc", RevealingChoice(List(rcElementB), true))

    val rc = mkFormComponent("rc", RevealingChoice(List(rcElementA, rcElementB), true))

    val section1 = mkSection(List(rc))

    val sections = List(
      section1
    )

    val fmb = mkFormModelFromSections(sections)

    val expectedStaticTypeInfo = StaticTypeInfoBuilder.simple(
      "rc" -> ExprType.ChoiceSelection,
      "a"  -> ExprType.String,
      "b"  -> ExprType.String
    )

    val expectedRevealinChoiceInfo = RevealingChoiceInfoBuilder(
      "a" -> ("0" -> "rc"),
      "b" -> ("1" -> "rc")
    )

    val table = Table(
      ("data", "expected"),
      (variadicFormDataMany(), sliceA :: Nil),
      (variadicFormDataMany("rc" -> List("0")), sliceB :: Nil),
      (variadicFormDataMany("rc" -> List("1")), sliceC :: Nil),
      (variadicFormDataMany("rc" -> List("0", "1")), rc :: Nil)
    )

    forAll(table) { case (data, expectedFcs) =>
      val expectedPage = Page[Visibility](
        toSmartString("Section Name"),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        expectedFcs,
        None,
        None,
        None,
        None,
        None,
        None
      )

      val expected: FormModel[Visibility] = FormModel.fromPages(
        NonEmptyList.one(BracketPlain.NonRepeatingPage(Singleton(expectedPage), section1)),
        expectedStaticTypeInfo,
        expectedRevealinChoiceInfo,
        SumInfo.empty
      )

      val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
        fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)

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
          FormCtx("a") -> NumberResult(10.00),
          FormCtx("b") -> NumberResult(20.00)
        )
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "dssd", "b" -> "20", "c" -> "5"),
        Map(
          FormCtx("a") -> Invalid("Number - cannot convert 'dssd' to number"),
          FormCtx("b") -> NumberResult(20.00)
        )
      )
    )

    forAll(table) { case (data, expected) =>
      val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
        fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)
      res.evaluationResults.exprMap shouldBe expected
    }
  }

  it should "use recalculate and assign type to the expressions 2" in {

    val fcA = mkFormComponent("a", Text(TextConstraint.default, Value))
    val fcB = mkFormComponent("b", Text(TextConstraint.default, Value))
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
          FormCtx("a") -> StringResult("10"),
          FormCtx("b") -> StringResult("20")
        )
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "Hello", "b" -> "World", "c" -> "5"),
        Map(
          FormCtx("a") -> StringResult("Hello"),
          FormCtx("b") -> StringResult("World")
        )
      )
    )

    forAll(table) { case (data, expected) =>
      val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
        fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)
      res.evaluationResults.exprMap shouldBe expected
    }
  }

  it should "handle Constant expression according to data it wraps" in {
    val table = Table(
      ("expression", "expected"),
      (
        Constant("A"),
        Map(
          Constant("A") -> StringResult("A")
        )
      ),
      (
        Constant("1"),
        Map(
          Constant("1") -> NumberResult(1)
        )
      ),
      (
        Constant("1.2"),
        Map(
          Constant("1.2") -> NumberResult(1.2)
        )
      ),
      (
        Add(Constant("1.2"), Constant("2")),
        Map(
          Constant("1.2") -> NumberResult(1.2),
          Constant("2")   -> NumberResult(2)
        )
      ),
      (
        Add(Constant("1"), Constant("2")),
        Map(
          Constant("1") -> NumberResult(1),
          Constant("2") -> NumberResult(2)
        )
      ),
      (
        Add(Constant("A"), Constant("B")),
        Map(
          Constant("A") -> StringResult("A"),
          Constant("B") -> StringResult("B")
        )
      ),
      (
        Add(Constant("1"), Constant("A")),
        Map(
          Constant("1") -> NumberResult(1),
          Constant("A") -> StringResult("A")
        )
      ),
      (
        Add(Constant(""), Add(Constant("1"), Constant("A"))),
        Map(
          Constant("1") -> NumberResult(1),
          Constant("")  -> Empty,
          Constant("A") -> StringResult("A")
        )
      ),
      (
        Add(Constant("A"), Constant("1")),
        Map(
          Constant("1") -> NumberResult(1),
          Constant("A") -> StringResult("A")
        )
      )
    )

    forAll(table) { case (expression, expected) =>
      val fcA = mkFormComponent("a", Value).copy(label = toSmartStringExpression("", expression))
      val section1 = mkSection(fcA)
      val sections = List(
        section1
      )
      val fmb = mkFormModelFromSections(sections)
      val data = variadicFormData[SourceOrigin.OutOfDate]()
      val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
        fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)

      res.evaluationResults.exprMap shouldBe expected
    }
  }

  it should "handle Constant expression respecting format of component" in {
    val table = Table(
      ("textConstraint", "expected", "expectedVariadicData"),
      (
        PositiveNumber(maxFractionalDigits = 0),
        Map(
          Constant("123") -> NumberResult(123),
          Constant("456") -> NumberResult(456),
          FormCtx("a")    -> NumberResult(123)
        ),
        variadicFormData[SourceOrigin.Current]("a" -> "123")
      ),
      (
        TextConstraint.default,
        Map(
          Constant("123") -> NumberResult(123),
          Constant("456") -> NumberResult(456),
          FormCtx("a")    -> StringResult("123")
        ),
        variadicFormData[SourceOrigin.Current]("a" -> "123")
      )
    )

    forAll(table) { case (textConstraint, expected, expectedVariadicData) =>
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
      val data = variadicFormData[SourceOrigin.OutOfDate]("a" -> "123")

      val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
        fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)

      res.evaluationResults.exprMap shouldBe expected
      res.recData.variadicFormData shouldBe expectedVariadicData

    }

  }

  it should "handle includeIf with simple contains expression" in {

    val fcA = mkFormComponent("a", Text(PositiveNumber(maxFractionalDigits = 0), Constant("123")))
    val fcA2 =
      mkFormComponent(
        "a2",
        Choice(
          Radio,
          NonEmptyList.one(toOptionData("Option A")),
          Vertical,
          List.empty[Int],
          None,
          None,
          None,
          LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
          None,
          None
        )
      )
    val fcB = mkFormComponent("b", Text(TextConstraint.default, Constant("456")))
    val fcC = mkFormComponent("c", Text(TextConstraint.default, Value))
    val fcD = mkFormComponent("d", Text(TextConstraint.default, FormCtx("c")))

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

    val staticTypeInfo = StaticTypeInfoBuilder(
      "a"  -> (ExprType.Number          -> Some(PositiveNumber(11, 0, RoundingMode.Down, None))),
      "b"  -> (ExprType.String          -> None),
      "c"  -> (ExprType.String          -> None),
      "a2" -> (ExprType.ChoiceSelection -> None),
      "d"  -> (ExprType.String          -> None)
    )

    val table = Table(
      ("data", "expected", "expectedFormModel"),
      (
        variadicFormData[SourceOrigin.OutOfDate](),
        Map(
          Constant("123") -> NumberResult(123),
          Constant("456") -> NumberResult(456),
          Constant("0")   -> NumberResult(0),
          FormCtx("a2")   -> Empty,
          FormCtx("c")    -> Hidden
        ),
        NonEmptyList.of(
          Bracket.NonRepeatingPage(Singleton(expectedPageA), SectionNumber(0), section1),
          Bracket.NonRepeatingPage(Singleton(expectedPageB), SectionNumber(1), section2),
          Bracket.NonRepeatingPage(Singleton(expectedPageD), SectionNumber(3), section4)
        )
      ),
      (
        variadicFormDataMany("a2" -> List("0")) ++ variadicFormData[SourceOrigin.OutOfDate]("c" -> "X"),
        Map(
          Constant("123") -> NumberResult(123),
          Constant("456") -> NumberResult(456),
          Constant("0")   -> NumberResult(0),
          FormCtx("a2")   -> OptionResult(List("0")),
          FormCtx("c")    -> StringResult("X")
        ),
        NonEmptyList.of(
          Bracket.NonRepeatingPage(Singleton(expectedPageA), SectionNumber(0), section1),
          Bracket.NonRepeatingPage(Singleton(expectedPageB), SectionNumber(1), section2),
          Bracket.NonRepeatingPage(Singleton(expectedPageC), SectionNumber(2), section3),
          Bracket.NonRepeatingPage(Singleton(expectedPageD), SectionNumber(3), section4)
        )
      )
    )

    forAll(table) { case (data, expected, expectedPages) =>
      val expectedFormModel: FormModel[Visibility] = fromPagesWithIndex(expectedPages, staticTypeInfo)

      val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
        fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)

      res.evaluationResults.exprMap shouldBe expected
      res.formModel shouldBe expectedFormModel
    }
  }

  it should "handle includeIf with reference to formComponent with WholeNumber format" in {

    val fcA = mkFormComponent("a", Text(PositiveNumber(maxFractionalDigits = 0), Constant("123")))
    val fcB =
      mkFormComponent(
        "b",
        Choice(
          Radio,
          NonEmptyList.one(toOptionData("Option A")),
          Vertical,
          List.empty[Int],
          None,
          None,
          None,
          LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
          None,
          None
        )
      )
    val fcC = mkFormComponent("c", Text(TextConstraint.default, Value))
    val fcD = mkFormComponent("d", Text(TextConstraint.default, Value))

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

    val staticTypeInfo = StaticTypeInfoBuilder(
      "a" -> (ExprType.Number          -> Some(PositiveNumber(11, 0, RoundingMode.Down, None))),
      "b" -> (ExprType.ChoiceSelection -> None),
      "c" -> (ExprType.String          -> None),
      "d" -> (ExprType.String          -> None)
    )

    val table = Table(
      ("data", "expected", "expectedFormModel"),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "123") ++
          variadicFormDataMany("b"                   -> List("123")),
        Map(
          Constant("123") -> NumberResult(123),
          FormCtx("a")    -> NumberResult(123),
          FormCtx("b")    -> OptionResult(List("123"))
        ),
        NonEmptyList.of(
          Bracket.NonRepeatingPage(Singleton(expectedPage1), SectionNumber(0), section1),
          Bracket.NonRepeatingPage(Singleton(expectedPage2), SectionNumber(1), section2),
          Bracket.NonRepeatingPage(Singleton(expectedPage3), SectionNumber(2), section3)
        )
      ),
      (
        variadicFormDataMany("b" -> List("124")),
        Map(
          Constant("123") -> NumberResult(123),
          FormCtx("a")    -> NumberResult(123),
          FormCtx("b")    -> OptionResult(List("124")),
          FormCtx("c")    -> Hidden
        ),
        NonEmptyList.of(
          Bracket.NonRepeatingPage(Singleton(expectedPage1), SectionNumber(0), section1),
          Bracket.NonRepeatingPage(Singleton(expectedPage3), SectionNumber(2), section3)
        )
      )
    )

    forAll(table) { case (data, expected, expectedPages) =>
      val expectedFormModel: FormModel[Visibility] = fromPagesWithIndex(expectedPages, staticTypeInfo)

      val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
        fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)

      res.evaluationResults.exprMap shouldBe expected
      res.formModel shouldBe expectedFormModel
    }
  }

  it should "handle contains in includeIf with reference to formComponent with NonWholeNumber format" in {

    val fcA = mkFormComponent("a", Text(PositiveNumber(maxFractionalDigits = 2), Constant("123.45")))
    val fcB =
      mkFormComponent(
        "b",
        Choice(
          Radio,
          toOptionData(NonEmptyList.one("Option A")),
          Vertical,
          List.empty[Int],
          None,
          None,
          None,
          LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
          None,
          None
        )
      )
    val fcC = mkFormComponent("c", Text(TextConstraint.default, Value))
    val fcD = mkFormComponent("d", Text(TextConstraint.default, Value))

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

    val staticTypeInfo = StaticTypeInfoBuilder(
      "a" -> (ExprType.Number          -> Some(PositiveNumber(11, 2, RoundingMode.Down, None))),
      "b" -> (ExprType.ChoiceSelection -> None),
      "c" -> (ExprType.String          -> None),
      "d" -> (ExprType.String          -> None)
    )

    val table = Table(
      ("data", "expected", "expectedFormModel"),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "123.45") ++
          variadicFormDataMany("b"                   -> List("123")),
        Map(
          Constant("123.45") -> NumberResult(123.45),
          FormCtx("a")       -> NumberResult(123.45),
          FormCtx("b")       -> OptionResult(List("123"))
        ),
        NonEmptyList.of(
          Bracket.NonRepeatingPage(Singleton(expectedPage1), SectionNumber(0), section1),
          Bracket.NonRepeatingPage(Singleton(expectedPage2), SectionNumber(1), section2),
          Bracket.NonRepeatingPage(Singleton(expectedPage3), SectionNumber(2), section3)
        )
      )
    )

    forAll(table) { case (data, expected, expectedPages) =>
      val expectedFormModel: FormModel[Visibility] = fromPagesWithIndex(expectedPages, staticTypeInfo)

      val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
        fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)

      res.evaluationResults.exprMap shouldBe expected
      res.formModel shouldBe expectedFormModel
    }
  }

  it should "use recalculate graph to correctly mark hidden fields" in {

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
          FormCtx("a")      -> StringResult("HELLO"),
          FormCtx("b")      -> StringResult("WORLD2"),
          FormCtx("c")      -> Hidden,
          FormCtx("d")      -> Hidden,
          FormCtx("e")      -> Hidden,
          Constant("WORLD") -> StringResult("WORLD")
        )
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "HELLO", "b" -> "HELLO", "c" -> "C", "e" -> "E", "d" -> "D"),
        Map(
          FormCtx("a")      -> StringResult("HELLO"),
          FormCtx("b")      -> StringResult("HELLO"),
          FormCtx("c")      -> StringResult("C"),
          FormCtx("d")      -> Hidden,
          FormCtx("e")      -> Hidden,
          Constant("WORLD") -> StringResult("WORLD")
        )
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "HELLO", "b" -> "WORLD", "c" -> "C", "e" -> "E", "d" -> "D"),
        Map(
          FormCtx("a")      -> StringResult("HELLO"),
          FormCtx("b")      -> StringResult("WORLD"),
          FormCtx("c")      -> Hidden,
          FormCtx("d")      -> StringResult("D"),
          Constant("WORLD") -> StringResult("WORLD")
        )
      )
    )

    forAll(table) { case (data, expected) =>
      val res: FormModelVisibilityOptics[DataOrigin.Mongo] =
        fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)
      res.evaluationResults.exprMap shouldBe expected
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

    val staticTypeInfo = StaticTypeInfoBuilder.simple(
      "a" -> ExprType.String,
      "b" -> ExprType.String,
      "c" -> ExprType.String,
      "d" -> ExprType.String,
      "e" -> ExprType.String,
      "f" -> ExprType.String,
      "g" -> ExprType.String
    )

    val table = Table(
      ("data", "expectedData", "expectedPages"),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "HELLO", "b" -> "WORLD2", "c" -> "C", "e" -> "E", "d" -> "D"),
        variadicFormData[SourceOrigin.Current]("a"   -> "HELLO", "b" -> "WORLD2", "c" -> "C", "e" -> "E", "d" -> "D"),
        NonEmptyList.of(
          Bracket.NonRepeatingPage(Singleton(expectedPageA), SectionNumber(0), section1),
          Bracket.NonRepeatingPage(Singleton(expectedPageB), SectionNumber(1), section2),
          Bracket.NonRepeatingPage(Singleton(expectedPageF), SectionNumber(4), section5),
          Bracket.NonRepeatingPage(Singleton(expectedPageG), SectionNumber(5), section6)
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
        NonEmptyList.of(
          Bracket.NonRepeatingPage(Singleton(expectedPageA), SectionNumber(0), section1),
          Bracket.NonRepeatingPage(Singleton(expectedPageB), SectionNumber(1), section2),
          Bracket.NonRepeatingPage(Singleton(expectedPageC), SectionNumber(2), section3),
          Bracket.NonRepeatingPage(Singleton(expectedPageF), SectionNumber(4), section5),
          Bracket.NonRepeatingPage(Singleton(expectedPageG), SectionNumber(5), section6)
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
        NonEmptyList.of(
          Bracket.NonRepeatingPage(Singleton(expectedPageA), SectionNumber(0), section1),
          Bracket.NonRepeatingPage(Singleton(expectedPageB), SectionNumber(1), section2),
          Bracket.NonRepeatingPage(Singleton(expectedPageD), SectionNumber(3), section4),
          Bracket.NonRepeatingPage(Singleton(expectedPageF), SectionNumber(4), section5),
          Bracket.NonRepeatingPage(Singleton(expectedPageG), SectionNumber(5), section6)
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
        NonEmptyList.of(
          Bracket.NonRepeatingPage(Singleton(expectedPageA), SectionNumber(0), section1),
          Bracket.NonRepeatingPage(Singleton(expectedPageB), SectionNumber(1), section2),
          Bracket.NonRepeatingPage(Singleton(expectedPageC), SectionNumber(2), section3),
          Bracket.NonRepeatingPage(Singleton(expectedPageD), SectionNumber(3), section4),
          Bracket.NonRepeatingPage(Singleton(expectedPageF), SectionNumber(4), section5),
          Bracket.NonRepeatingPage(Singleton(expectedPageG), SectionNumber(5), section6)
        )
      )
    )

    forAll(table) { case (data, expectedData, expectedPages) =>
      val visibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo] =
        fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)

      val expected: FormModel[Visibility] = fromPagesWithIndex(expectedPages, staticTypeInfo)

      visibilityOptics.formModel shouldBe expected
      visibilityOptics.recData.variadicFormData shouldBe expectedData
    }
  }

}

object StaticTypeInfoBuilder {
  def simple(ts: (String, ExprType)*): StaticTypeInfo = StaticTypeInfo {
    ts.map { case (baseComponentId, exprType) =>
      BaseComponentId(baseComponentId) -> StaticTypeData(exprType, None)
    }.toMap
  }
  def apply(ts: (String, (ExprType, Option[TextConstraint]))*): StaticTypeInfo = StaticTypeInfo {
    ts.map { case (baseComponentId, (exprType, textConstraint)) =>
      BaseComponentId(baseComponentId) -> StaticTypeData(exprType, textConstraint)
    }.toMap
  }
}

object RevealingChoiceInfoBuilder {
  def apply(ts: (String, (String, String))*): RevealingChoiceInfo = RevealingChoiceInfo {
    ts.map { case (baseComponentId, (index, parent)) =>
      BaseComponentId(baseComponentId) -> RevealingChoiceData(index, BaseComponentId(parent))
    }.toMap
  }
}
