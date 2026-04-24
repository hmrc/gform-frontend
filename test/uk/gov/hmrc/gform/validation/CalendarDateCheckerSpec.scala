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
import play.api.Configuration
import play.api.Environment
import play.api.http.HttpConfiguration
import play.api.i18n._
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.mkFormComponent
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.mkFormTemplate
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.FormModelSupport
import uk.gov.hmrc.gform.models.VariadicFormDataSupport
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.CalendarDate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.validationSuccess

import scala.collection.mutable

class CalendarDateCheckerSpec extends FunSuite with FormModelSupport with VariadicFormDataSupport {

  implicit val checkInterpreter: ComponentChecker.CheckInterpreter = ComponentChecker.NonShortCircuitInterpreter
  implicit val lang: LangADT = LangADT.En

  private val dateComponent: FormComponent = mkFormComponent("date", CalendarDate)
  private val dateDayAtom = FormComponentId("date-day").modelComponentId
  private val dateMonthAtom = FormComponentId("date-month").modelComponentId
  private val formTemplate: FormTemplate = mkFormTemplate(
    ExampleData.nonRepeatingPageSection(fields = List(dateComponent))
  )

  implicit val smartStringEvaluator: SmartStringEvaluator = new SmartStringEvaluator {
    override def apply(s: SmartString, markDown: Boolean): String = s.rawDefaultValue(LangADT.En)
    override def evalEnglish(s: SmartString, markDown: Boolean): String = s.rawDefaultValue(LangADT.En)
  }

  val environment = Environment.simple()
  val configuration = Configuration.load(environment)
  val langs = new DefaultLangs()
  val httpConfiguration = HttpConfiguration.fromConfiguration(configuration, environment)

  def checkerDependency(optics: FormModelVisibilityOptics) =
    new CheckerDependency {
      def formModelVisibilityOptics: FormModelVisibilityOptics = optics
      def formComponent: FormComponent = dateComponent
      def cache: CacheData = ???
      def envelope: EnvelopeWithMapping = ???
      def lookupRegistry: LookupRegistry = ???
      def getEmailCodeFieldMatcher = ???
    }

  val messagesApi: MessagesApi =
    new DefaultMessagesApiProvider(environment, configuration, langs, httpConfiguration).get
  implicit val messages: Messages = messagesApi.preferred(Seq(langs.availables.head))

  test("validate should return valid when calendarDate atoms are correct") {

    val data = VariadicFormData(
      Map(
        dateDayAtom   -> VariadicValue.One("1"),
        dateMonthAtom -> VariadicValue.One("1")
      )
    )
    val formModelOptics: FormModelOptics = mkFormModelOptics(formTemplate, data)
    val calendarDateValidation = new CalendarDateChecker()

    assertEquals(
      calendarDateValidation.runCheck(checkerDependency(formModelOptics.formModelVisibilityOptics)),
      validationSuccess
    )
  }

  test("validate should return invalid when all calendarDate atoms are missing") {
    val data = VariadicFormData(Map.empty)
    val formModelOptics: FormModelOptics = mkFormModelOptics(formTemplate, data)

    assertEquals(
      new CalendarDateChecker()
        .runCheck(checkerDependency(formModelOptics.formModelVisibilityOptics)),
      Invalid(
        Map(
          dateMonthAtom -> mutable.LinkedHashSet("Enter a date"),
          dateDayAtom   -> mutable.LinkedHashSet("Enter a date")
        )
      )
    )
  }

  test("validate should return invalid when calendarDate month atom missing") {
    val data = VariadicFormData(
      Map(
        dateDayAtom   -> VariadicValue.One("1"),
        dateMonthAtom -> VariadicValue.One("")
      )
    )
    val formModelOptics: FormModelOptics = mkFormModelOptics(formTemplate, data)

    assertEquals(
      new CalendarDateChecker()
        .runCheck(checkerDependency(formModelOptics.formModelVisibilityOptics)),
      Invalid(Map(dateMonthAtom -> mutable.LinkedHashSet("Date must include a month", "Enter real month")))
    )
  }

  test("validate should return invalid when calendarDate day atom missing") {
    val data = VariadicFormData(
      Map(
        dateDayAtom   -> VariadicValue.One(""),
        dateMonthAtom -> VariadicValue.One("1")
      )
    )
    val formModelOptics: FormModelOptics = mkFormModelOptics(formTemplate, data)

    assertEquals(
      new CalendarDateChecker()
        .runCheck(checkerDependency(formModelOptics.formModelVisibilityOptics)),
      Invalid(Map(dateDayAtom -> mutable.LinkedHashSet("Date must include a day", "Enter real day")))
    )
  }

  test("validate should return invalid when calendarDate atoms have length > 2") {

    val data = VariadicFormData(
      Map(
        dateDayAtom   -> VariadicValue.One("111"),
        dateMonthAtom -> VariadicValue.One("222")
      )
    )
    val formModelOptics: FormModelOptics = mkFormModelOptics(formTemplate, data)

    assertEquals(
      new CalendarDateChecker()
        .runCheck(checkerDependency(formModelOptics.formModelVisibilityOptics)),
      Invalid(
        Map(
          dateDayAtom   -> mutable.LinkedHashSet("Enter real day"),
          dateMonthAtom -> mutable.LinkedHashSet("Enter real month")
        )
      )
    )
  }

  test("validate should return invalid when some calendarDate atoms are not integers") {

    val data = VariadicFormData(
      Map(
        dateDayAtom   -> VariadicValue.One("a"),
        dateMonthAtom -> VariadicValue.One("b")
      )
    )
    val formModelOptics: FormModelOptics = mkFormModelOptics(formTemplate, data)

    assertEquals(
      new CalendarDateChecker()
        .runCheck(checkerDependency(formModelOptics.formModelVisibilityOptics)),
      Invalid(
        Map(
          dateDayAtom   -> mutable.LinkedHashSet("Enter real day"),
          dateMonthAtom -> mutable.LinkedHashSet("Enter real month")
        )
      )
    )
  }

  val table: List[(VariadicFormData, ModelComponentId, String, String)] = List(
    (
      VariadicFormData(
        Map(
          dateDayAtom   -> VariadicValue.One("0"),
          dateMonthAtom -> VariadicValue.One("1")
        )
      ),
      dateDayAtom,
      "Enter real day",
      "Day outside range"
    ),
    (
      VariadicFormData(
        Map(
          dateDayAtom   -> VariadicValue.One("1"),
          dateMonthAtom -> VariadicValue.One("0")
        )
      ),
      dateMonthAtom,
      "Enter real month",
      "Month outside range"
    ),
    (
      VariadicFormData(
        Map(
          dateDayAtom   -> VariadicValue.One("30"),
          dateMonthAtom -> VariadicValue.One("2")
        )
      ),
      dateDayAtom,
      "Enter real day",
      "February day outside range"
    )
  )

  table.zipWithIndex.foreach { case ((data, atom, message, description), index) =>
    test(s"${index + 1}. $description") {
      val formModelOptics: FormModelOptics = mkFormModelOptics(formTemplate, data)
      assertEquals(
        new CalendarDateChecker()
          .runCheck(checkerDependency(formModelOptics.formModelVisibilityOptics)),
        Invalid(
          Map(
            atom -> mutable.LinkedHashSet(message)
          )
        )
      )
    }
  }

  val table1: List[(VariadicFormData, ModelComponentId, ModelComponentId, String, String, String)] = List(
    (
      VariadicFormData(
        Map(
          dateDayAtom   -> VariadicValue.One("0"),
          dateMonthAtom -> VariadicValue.One("0")
        )
      ),
      dateDayAtom,
      dateMonthAtom,
      "Enter real day",
      "Enter real month",
      "Both day and month outside range"
    )
  )

  table1.zipWithIndex.foreach { case ((data, atom1, atom2, message1, message2, description), index) =>
    test(s"${index + 1}. $description") {
      val formModelOptics: FormModelOptics = mkFormModelOptics(formTemplate, data)
      assertEquals(
        new CalendarDateChecker()
          .runCheck(checkerDependency(formModelOptics.formModelVisibilityOptics)),
        Invalid(
          Map(
            atom1 -> mutable.LinkedHashSet(message1),
            atom2 -> mutable.LinkedHashSet(message2)
          )
        )
      )
    }
  }
}
