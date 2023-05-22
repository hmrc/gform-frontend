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

package uk.gov.hmrc.gform.validation

import cats.data.Validated.Invalid
import munit.FunSuite
import play.api.i18n._
import play.api.http.HttpConfiguration
import play.api.{ Configuration, Environment }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkFormComponent, mkFormTemplate }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ FormModelSupport, VariadicFormDataSupport }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ CalendarDate, FormComponent, FormComponentId, FormTemplate }
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.validationSuccess

class CalendarDateValidationSpec extends FunSuite with FormModelSupport with VariadicFormDataSupport {
  implicit val lang: LangADT = LangADT.En

  private val dateComponent: FormComponent = mkFormComponent("date", CalendarDate)
  private val dateDayAtom = FormComponentId("date-day").modelComponentId
  private val dateMonthAtom = FormComponentId("date-month").modelComponentId
  private val formTemplate: FormTemplate = mkFormTemplate(
    ExampleData.nonRepeatingPageSection(fields = List(dateComponent))
  )

  implicit val smartStringEvaluator: SmartStringEvaluator = (s: SmartString, _: Boolean) => s.rawValue(LangADT.En)

  val environment = Environment.simple()
  val configuration = Configuration.load(environment)
  val langs = new DefaultLangs()
  val httpConfiguration = HttpConfiguration.fromConfiguration(configuration, environment)

  val messagesApi: MessagesApi =
    new DefaultMessagesApiProvider(environment, configuration, langs, httpConfiguration).get
  implicit val messages: Messages = messagesApi.preferred(Seq(langs.availables.head))

  test("validate should return valid when calendarDate atoms are correct") {

    val data = VariadicFormData[OutOfDate](
      Map(
        dateDayAtom   -> VariadicValue.One("1"),
        dateMonthAtom -> VariadicValue.One("1")
      )
    )
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)
    val calendarDateValidation = new CalendarDateValidation(formModelOptics.formModelVisibilityOptics)

    assertEquals(calendarDateValidation.validate(dateComponent), validationSuccess)
  }

  test("validate should return invalid when all calendarDate atoms are missing") {
    val data = VariadicFormData[OutOfDate](Map.empty)
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)

    assertEquals(
      new CalendarDateValidation(formModelOptics.formModelVisibilityOptics).validate(dateComponent),
      Invalid(Map(dateMonthAtom -> Set("Enter a date"), dateDayAtom -> Set("Enter a date")))
    )
  }

  test("validate should return invalid when some calendarDate atoms are missing") {
    val data = VariadicFormData[OutOfDate](
      Map(
        dateDayAtom -> VariadicValue.One("1")
      )
    )
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)

    assertEquals(
      new CalendarDateValidation(formModelOptics.formModelVisibilityOptics).validate(dateComponent),
      Invalid(Map(dateMonthAtom -> Set("Enter a date")))
    )
  }

  test("validate should return invalid when some calendarDate atoms have length > 2") {

    val data = VariadicFormData[OutOfDate](
      Map(
        dateDayAtom   -> VariadicValue.One("111"),
        dateMonthAtom -> VariadicValue.One("222")
      )
    )
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)

    assertEquals(
      new CalendarDateValidation(formModelOptics.formModelVisibilityOptics).validate(dateComponent),
      Invalid(
        Map(
          dateDayAtom -> Set(
            "Enter a day in the correct format"
          ),
          dateMonthAtom -> Set(
            "Enter a month in the correct format"
          )
        )
      )
    )
  }

  test("validate should return invalid when some calendarDate atoms are not integers") {

    val data = VariadicFormData[OutOfDate](
      Map(
        dateDayAtom   -> VariadicValue.One("a"),
        dateMonthAtom -> VariadicValue.One("b")
      )
    )
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)

    assertEquals(
      new CalendarDateValidation(formModelOptics.formModelVisibilityOptics).validate(dateComponent),
      Invalid(
        Map(
          dateDayAtom -> Set(
            "Enter a day in the correct format"
          ),
          dateMonthAtom -> Set(
            "Enter a month in the correct format"
          )
        )
      )
    )
  }

  val table: List[(VariadicFormData[OutOfDate], ModelComponentId, String, String)] = List(
    (
      VariadicFormData[OutOfDate](
        Map(
          dateDayAtom   -> VariadicValue.One("0"),
          dateMonthAtom -> VariadicValue.One("1")
        )
      ),
      dateDayAtom,
      "Enter a day in the correct format",
      "Day outside range"
    ),
    (
      VariadicFormData[OutOfDate](
        Map(
          dateDayAtom   -> VariadicValue.One("1"),
          dateMonthAtom -> VariadicValue.One("0")
        )
      ),
      dateMonthAtom,
      "Enter a month in the correct format",
      "Month outside range"
    ),
    (
      VariadicFormData[OutOfDate](
        Map(
          dateDayAtom   -> VariadicValue.One("30"),
          dateMonthAtom -> VariadicValue.One("2")
        )
      ),
      dateDayAtom,
      "Enter a day in the correct format",
      "February day outside range"
    )
  )

  table.zipWithIndex.foreach { case ((data, atom, message, description), index) =>
    test(s"$index. $description") {
      val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)
      assertEquals(
        new CalendarDateValidation(formModelOptics.formModelVisibilityOptics).validate(dateComponent),
        Invalid(
          Map(
            atom -> Set(
              message
            )
          )
        )
      )
    }
  }

  val table1: List[(VariadicFormData[OutOfDate], ModelComponentId, ModelComponentId, String, String, String)] = List(
    (
      VariadicFormData[OutOfDate](
        Map(
          dateDayAtom   -> VariadicValue.One("0"),
          dateMonthAtom -> VariadicValue.One("0")
        )
      ),
      dateDayAtom,
      dateMonthAtom,
      "Enter a day in the correct format",
      "Enter a month in the correct format",
      "Both day and month outside range"
    )
  )

  table1.zipWithIndex.foreach { case ((data, atom1, atom2, message1, message2, description), index) =>
    test(s"$index. $description") {
      val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)
      assertEquals(
        new CalendarDateValidation(formModelOptics.formModelVisibilityOptics).validate(dateComponent),
        Invalid(
          Map(
            atom1 -> Set(
              message1
            ),
            atom2 -> Set(
              message2
            )
          )
        )
      )
    }
  }
}
