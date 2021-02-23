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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import org.scalatest.{ FlatSpec, Matchers }
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import scala.language.implicitConversions
import uk.gov.hmrc.gform.eval.ExprType
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class FormModelBuilderSpec extends FlatSpec with Matchers with FormModelSupport with VariadicFormDataSupport {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)

  "FormModelBuilder.renderPageModel" should "return visibility model without components whose includeIf evaluates to false" in {

    val includeIf = IncludeIf(Equals(FormCtx("a"), Constant("X")))

    val fcA = mkFormComponent("a", Value)
    val fcB = mkFormComponent("b", Value).copy(includeIf = Some(includeIf))
    val fcC = mkFormComponent("c", Value)

    val section1 = mkSection(List(fcA))
    val section2 = mkSection(List(fcB, fcC))

    val sections = List(
      section1,
      section2,
    )
    val fmb = mkFormModelFromSections(sections)

    val expectedPageA = mkPage(fcA :: Nil)

    val expectedPageB1 = mkPage(fcC :: Nil)
    val expectedPageB2 = mkPage(fcB :: fcC :: Nil)

    val staticTypeInfo = StaticTypeInfoBuilder.simple(
      "a" -> ExprType.String,
      "b" -> ExprType.String,
      "c" -> ExprType.String,
    )

    val table = Table(
      ("data", "expectedData", "expectedPages"),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "NotX", "b" -> "B", "c" -> "C"),
        variadicFormData[SourceOrigin.Current]("a"   -> "NotX", "b" -> "B", "c" -> "C"),
        NonEmptyList.of(
          Bracket.NonRepeatingPage(Singleton(expectedPageA), SectionNumber(0), section1),
          Bracket.NonRepeatingPage(Singleton(expectedPageB1), SectionNumber(1), section2),
        )
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate]("a" -> "X", "b" -> "B", "c" -> "C"),
        variadicFormData[SourceOrigin.Current]("a"   -> "X", "b" -> "B", "c" -> "C"),
        NonEmptyList.of(
          Bracket.NonRepeatingPage(Singleton(expectedPageA), SectionNumber(0), section1),
          Bracket.NonRepeatingPage(Singleton(expectedPageB2), SectionNumber(1), section2),
        )
      )
    )

    forAll(table) {
      case (data, expectedData, expectedPages) =>
        val visibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo] =
          fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](data, None)

        val formModelOptics: FormModelOptics[DataOrigin.Mongo] =
          fmb.renderPageModel[DataOrigin.Mongo, SectionSelectorType.Normal](visibilityOptics, None)

        val expected: FormModel[Visibility] = fromPagesWithIndex(expectedPages, staticTypeInfo)

        formModelOptics.formModelVisibilityOptics.formModel shouldBe expected
        visibilityOptics.recData.variadicFormData shouldBe expectedData
    }

  }
}
