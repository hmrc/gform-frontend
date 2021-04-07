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

package uk.gov.hmrc.gform.validation

import cats.data.Validated.Invalid
import munit.FunSuite
import play.api.i18n.Messages
import play.api.test.Helpers
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkFormComponent, mkFormTemplate }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ FormModelSupport, VariadicFormDataSupport }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ CalendarDate, FormComponent, FormComponentId, FormTemplate }
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.validationSuccess

class CalendarDateValidationSpec extends FunSuite with FormModelSupport with VariadicFormDataSupport with ExampleData {

  private val dateComponent: FormComponent = mkFormComponent("date", CalendarDate)
  private val dateDayAtom = FormComponentId("date-day").modelComponentId
  private val dateMonthAtom = FormComponentId("date-month").modelComponentId
  private val formTemplate: FormTemplate = mkFormTemplate(nonRepeatingPageSection(fields = List(dateComponent)))

  implicit val smartStringEvaluator: SmartStringEvaluator = (s: SmartString, _: Boolean) => s.rawValue(LangADT.En)

  implicit val messages: Messages = Helpers.stubMessages(
    Helpers.stubMessagesApi(
      Map(
        "en" -> Map(
          "date.day"                   -> "day",
          "date.month"                 -> "month",
          "helper.order"               -> "{0} {1}",
          "field.error.number"         -> "{0} must be a number",
          "field.error.required"       -> "{0} must be entered",
          "date.dayMonthCombo.invalid" -> "{0} must have valid values for day and month"
        )
      )
    )
  )

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
      Invalid(Map(dateMonthAtom -> Set("Label must be entered"), dateDayAtom -> Set("Label must be entered")))
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
      Invalid(Map(dateMonthAtom -> Set("Label must be entered")))
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
          dateComponent.modelComponentId -> Set(
            "Label day must be a number",
            "Label month must be a number"
          )
        )
      )
    )
  }

  val table: List[(VariadicFormData[OutOfDate], String)] = List(
    (
      VariadicFormData[OutOfDate](
        Map(
          dateDayAtom   -> VariadicValue.One("0"),
          dateMonthAtom -> VariadicValue.One("1")
        )
      ),
      "Invalid day value"
    ),
    (
      VariadicFormData[OutOfDate](
        Map(
          dateDayAtom   -> VariadicValue.One("1"),
          dateMonthAtom -> VariadicValue.One("13")
        )
      ),
      "Invalid month value"
    ),
    (
      VariadicFormData[OutOfDate](
        Map(
          dateDayAtom   -> VariadicValue.One("30"),
          dateMonthAtom -> VariadicValue.One("2")
        )
      ),
      "Invalid day for the month"
    )
  )

  table.zipWithIndex.foreach { case ((data, description), index) =>
    test(index + ". " + description) {
      val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)
      assertEquals(
        new CalendarDateValidation(formModelOptics.formModelVisibilityOptics).validate(dateComponent),
        Invalid(
          Map(
            dateComponent.modelComponentId -> Set(
              "Label must have valid values for day and month"
            )
          )
        )
      )
    }
  }
}
