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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import play.api.i18n.Messages
import uk.gov.hmrc.gform.Helpers.toSmartString

import scala.language.implicitConversions
import uk.gov.hmrc.gform.eval.ExprType
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SourceOrigin }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import java.time.Instant

class FormModelBuilderSpec extends AnyFlatSpecLike with Matchers with FormModelSupport with VariadicFormDataSupport {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)
  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages = play.api.test.Helpers.stubMessages(play.api.test.Helpers.stubMessagesApi(Map.empty))

  "FormModelBuilder.renderPageModel" should "return visibility model without components whose includeIf evaluates to false" in {

    val includeIf = IncludeIf(Equals(FormCtx("a"), Constant("X")))

    val fcA = mkFormComponent("a", Value)
    val fcB = mkFormComponent("b", Value).copy(includeIf = Some(includeIf))
    val fcC = mkFormComponent("c", Value)

    val section1 = mkSection(List(fcA))
    val section2 = mkSection(List(fcB, fcC))

    val sections = List(
      section1,
      section2
    )
    val fmb = mkFormModelFromSections(sections)

    val expectedPageA = mkPage(fcA :: Nil)

    val expectedPageB1 = mkPage(fcC :: Nil)
    val expectedPageB2 = mkPage(fcB :: fcC :: Nil)

    val staticTypeInfo = StaticTypeInfoBuilder.simple(
      "a" -> ExprType.String,
      "b" -> ExprType.String,
      "c" -> ExprType.String
    )

    val table = Table(
      ("data", "expectedData", "expectedPages"),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "NotX", "b" -> "B", "c" -> "C"),
        variadicFormData[SourceOrigin.Current]("a"   -> "NotX", "b" -> "B", "c" -> "C"),
        NonEmptyList.of(
          Bracket.NonRepeatingPage(
            SingletonWithNumber(Singleton(expectedPageA), SectionNumber.Classic.NormalPage(TemplateSectionIndex(0))),
            section1
          ),
          Bracket.NonRepeatingPage(
            SingletonWithNumber(Singleton(expectedPageB1), SectionNumber.Classic.NormalPage(TemplateSectionIndex(1))),
            section2
          )
        )
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "X", "b" -> "B", "c" -> "C"),
        variadicFormData[SourceOrigin.Current]("a"   -> "X", "b" -> "B", "c" -> "C"),
        NonEmptyList.of(
          Bracket.NonRepeatingPage(
            SingletonWithNumber(Singleton(expectedPageA), SectionNumber.Classic.NormalPage(TemplateSectionIndex(0))),
            section1
          ),
          Bracket.NonRepeatingPage(
            SingletonWithNumber(Singleton(expectedPageB2), SectionNumber.Classic.NormalPage(TemplateSectionIndex(1))),
            section2
          )
        )
      )
    )

    forAll(table) { case (data, expectedData, expectedPages) =>
      val visibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo] =
        fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None, Instant.now)

      val formModelOptics: FormModelOptics[DataOrigin.Mongo] =
        fmb.renderPageModel[DataOrigin.Mongo, SectionSelectorType.Normal](visibilityOptics, None)

      val expected: FormModel[Visibility] = fromPagesWithIndex(expectedPages, staticTypeInfo)

    //TODO: Change test, visibility optics should not hide pages that are not part of expr tree associated with current section.
    //formModelOptics.formModelVisibilityOptics.formModel shouldBe expected
    //visibilityOptics.recData.variadicFormData shouldBe expectedData
    }

  }

  it should "expand repeated sections" in {
    val fcA = mkFormComponent("a", Value, Number())
    val fcB = mkFormComponent("b", Value)
    val fcC = mkFormComponent("c", Date(AnyDate, Offset(0), None))

    val section1 = mkSection(List(fcA))
    val section2 = mkRepeatingPageSection(List(fcB, fcC), FormCtx(FormComponentId("a")))

    val sections = List(
      section1,
      section2
    )
    val fmb = mkFormModelFromSections(sections)
    val variadicData = variadicFormData[SourceOrigin.OutOfDate]("a" -> "2")
    val visibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo] =
      fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](variadicData, None, Instant.now)
    val formModelOptics = fmb.renderPageModel[DataOrigin.Mongo, SectionSelectorType.Normal](visibilityOptics, None)

    formModelOptics.formModelRenderPageOptics.formModel.allFormComponentIds shouldBe List(
      FormComponentId("a"),
      FormComponentId("1_b"),
      FormComponentId("1_c"),
      FormComponentId("2_b"),
      FormComponentId("2_c")
    )

    formModelOptics.formModelRenderPageOptics.formModel.fcIdRepeatsExprLookup shouldBe Map(
      FormComponentId("1_b") -> FormCtx(FormComponentId("a")),
      FormComponentId("2_b") -> FormCtx(FormComponentId("a")),
      FormComponentId("1_c") -> FormCtx(FormComponentId("a")),
      FormComponentId("2_c") -> FormCtx(FormComponentId("a"))
    )
  }

  it should "create CheckoutYourAnswers page model when AddToList configured with CheckYourAnswersPage" in {

    val fcA = mkFormComponent("a", Value)

    val section1 = mkAddToListSection(
      "someQuestion",
      Some(
        CheckYourAnswersPage(
          Some(toSmartString("Title")),
          None,
          toSmartString("Update Title"),
          Some(toSmartString("No PII Title")),
          Some(toSmartString("No PII Update Title")),
          Some(toSmartString("Header")),
          Some(toSmartString("Footer")),
          Some(toSmartString("Continue")),
          None,
          None,
          None,
          None,
          None
        )
      ),
      List(fcA)
    )

    val sections = List(
      section1
    )
    val fmb = mkFormModelFromSections(sections)
    val variadicData = variadicFormData[SourceOrigin.OutOfDate]("a" -> "1")

    val fm = fmb.expand[Interim, SectionSelectorType.Normal](variadicData)
    val addToListIterations = fm.brackets.addToListBracket(AddToListId(FormComponentId("someQuestion"))).iterations
    addToListIterations.size shouldBe 1
    addToListIterations.head.checkYourAnswers.isDefined shouldBe true
    addToListIterations.head.checkYourAnswers.get.checkYourAnswers.expandedUpdateTitle shouldBe toSmartString(
      "Update Title"
    )
    addToListIterations.head.checkYourAnswers.get.checkYourAnswers.expandedNoPIIUpdateTitle shouldBe Some(
      toSmartString("No PII Update Title")
    )
    addToListIterations.head.checkYourAnswers.get.checkYourAnswers.index shouldBe 1
    addToListIterations.head.checkYourAnswers.get.checkYourAnswers.expandedId shouldBe PageId("1_someQuestionCYA")
    addToListIterations.head.checkYourAnswers.get.checkYourAnswers.formComponent.id shouldBe FormComponentId(
      "1_someQuestionCYA"
    )
  }

  it should "not create CheckoutYourAnswers page model when AddToList isn't configured with CheckYourAnswersPage" in {

    val fcA = mkFormComponent("a", Value)

    val section1 = mkAddToListSection(
      "someQuestion",
      None,
      List(fcA)
    )

    val sections = List(
      section1
    )
    val fmb = mkFormModelFromSections(sections)
    val variadicData = variadicFormData[SourceOrigin.OutOfDate]("a" -> "1")

    val fm = fmb.expand[Interim, SectionSelectorType.Normal](variadicData)
    val addToListIterations = fm.brackets.addToListBracket(AddToListId(FormComponentId("someQuestion"))).iterations
    addToListIterations.size shouldBe 1
    addToListIterations.head.checkYourAnswers shouldBe empty
  }
}
