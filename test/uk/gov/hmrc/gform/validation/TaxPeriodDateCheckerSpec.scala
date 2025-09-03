/*
 * Copyright 2025 HM Revenue & Customs
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
import play.api.http.HttpConfiguration
import play.api.i18n.{ DefaultLangs, DefaultMessagesApiProvider, Messages, MessagesApi }
import play.api.{ Configuration, Environment }
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkFormComponent, mkFormTemplate }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ FormModelSupport, VariadicFormDataSupport }
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, FormTemplate, TaxPeriodDate }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.validationSuccess

import scala.collection.mutable

class TaxPeriodDateCheckerSpec extends FunSuite with FormModelSupport with VariadicFormDataSupport {
  implicit val checkInterpreter: ComponentChecker.CheckInterpreter = ComponentChecker.NonShortCircuitInterpreter
  implicit val lang: LangADT = LangADT.En

  private val dateComponent: FormComponent = mkFormComponent("date", TaxPeriodDate)
  private val dateMonthAtom = FormComponentId("date-month").modelComponentId
  private val dateYearAtom = FormComponentId("date-year").modelComponentId
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

  def checkerDependency[D <: DataOrigin](optics: FormModelVisibilityOptics[D]) =
    new CheckerDependency[D] {
      def formModelVisibilityOptics: FormModelVisibilityOptics[D] = optics
      def formComponent: FormComponent = dateComponent
      def cache: CacheData = ???
      def envelope: EnvelopeWithMapping = ???
      def lookupRegistry: LookupRegistry = ???
      def getEmailCodeFieldMatcher = ???
    }

  val messagesApi: MessagesApi =
    new DefaultMessagesApiProvider(environment, configuration, langs, httpConfiguration).get
  implicit val messages: Messages = messagesApi.preferred(Seq(langs.availables.head))

  test("validate should return valid when taxPeriodDate atoms are correct") {
    val data = VariadicFormData[OutOfDate](
      Map(
        dateMonthAtom -> VariadicValue.One("1"),
        dateYearAtom  -> VariadicValue.One("2000")
      )
    )
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)
    val taxPeriodDateValidation = new TaxPeriodDateChecker[DataOrigin.Browser]()

    assertEquals(
      taxPeriodDateValidation.runCheck(
        checkerDependency[DataOrigin.Browser](formModelOptics.formModelVisibilityOptics)
      ),
      validationSuccess
    )
  }

  test("validate should return invalid when all taxPeriodDate atoms are missing") {
    val data = VariadicFormData[OutOfDate](Map.empty)
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)
    val taxPeriodDateValidation = new TaxPeriodDateChecker[DataOrigin.Browser]()

    assertEquals(
      taxPeriodDateValidation.runCheck(
        checkerDependency[DataOrigin.Browser](formModelOptics.formModelVisibilityOptics)
      ),
      Invalid(
        Map(
          dateMonthAtom -> mutable.LinkedHashSet("Enter a date"),
          dateYearAtom  -> mutable.LinkedHashSet("Enter a date")
        )
      )
    )
  }

  test("validate should return invalid when taxPeriodDate year atom is missing") {
    val data = VariadicFormData[OutOfDate](
      Map(
        dateMonthAtom -> VariadicValue.One("1"),
        dateYearAtom  -> VariadicValue.One("")
      )
    )
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)
    val taxPeriodDateValidation = new TaxPeriodDateChecker[DataOrigin.Browser]()

    assertEquals(
      taxPeriodDateValidation.runCheck(
        checkerDependency[DataOrigin.Browser](formModelOptics.formModelVisibilityOptics)
      ),
      Invalid(Map(dateYearAtom -> mutable.LinkedHashSet("Date must include a year", "Enter real year")))
    )
  }

  test("validate should return invalid when taxPeriodDate month atom is missing") {
    val data = VariadicFormData[OutOfDate](
      Map(
        dateMonthAtom -> VariadicValue.One(""),
        dateYearAtom  -> VariadicValue.One("2000")
      )
    )
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)
    val taxPeriodDateValidation = new TaxPeriodDateChecker[DataOrigin.Browser]()

    assertEquals(
      taxPeriodDateValidation.runCheck(
        checkerDependency[DataOrigin.Browser](formModelOptics.formModelVisibilityOptics)
      ),
      Invalid(Map(dateMonthAtom -> mutable.LinkedHashSet("Date must include a month", "Enter real month")))
    )
  }

  test("validate should return invalid when taxPeriodDate atoms have abnormal lengths") {
    val data = VariadicFormData[OutOfDate](
      Map(
        dateMonthAtom -> VariadicValue.One("123"),
        dateYearAtom  -> VariadicValue.One("20000")
      )
    )
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)
    val taxPeriodDateValidation = new TaxPeriodDateChecker[DataOrigin.Browser]()

    assertEquals(
      taxPeriodDateValidation.runCheck(
        checkerDependency[DataOrigin.Browser](formModelOptics.formModelVisibilityOptics)
      ),
      Invalid(
        Map(
          dateMonthAtom -> mutable.LinkedHashSet("Enter real month"),
          dateYearAtom  -> mutable.LinkedHashSet("Enter real year")
        )
      )
    )
  }

  test("validate should return invalid when taxPeriodDate atoms are not integers") {
    val data = VariadicFormData[OutOfDate](
      Map(
        dateMonthAtom -> VariadicValue.One("a"),
        dateYearAtom  -> VariadicValue.One("b")
      )
    )
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)
    val taxPeriodDateValidation = new TaxPeriodDateChecker[DataOrigin.Browser]()

    assertEquals(
      taxPeriodDateValidation.runCheck(
        checkerDependency[DataOrigin.Browser](formModelOptics.formModelVisibilityOptics)
      ),
      Invalid(
        Map(
          dateMonthAtom -> mutable.LinkedHashSet("Enter real month"),
          dateYearAtom  -> mutable.LinkedHashSet("Enter real year")
        )
      )
    )
  }

  val table: List[(VariadicFormData[OutOfDate], ModelComponentId, String, String)] = List(
    (
      VariadicFormData[OutOfDate](
        Map(
          dateMonthAtom -> VariadicValue.One("1"),
          dateYearAtom  -> VariadicValue.One("0")
        )
      ),
      dateYearAtom,
      "Enter real year",
      "Year outside range"
    ),
    (
      VariadicFormData[OutOfDate](
        Map(
          dateMonthAtom -> VariadicValue.One("0"),
          dateYearAtom  -> VariadicValue.One("2000")
        )
      ),
      dateMonthAtom,
      "Enter real month",
      "Month outside bottom range"
    ),
    (
      VariadicFormData[OutOfDate](
        Map(
          dateMonthAtom -> VariadicValue.One("13"),
          dateYearAtom  -> VariadicValue.One("2000")
        )
      ),
      dateMonthAtom,
      "Enter real month",
      "Month outside top range"
    )
  )

  table.zipWithIndex.foreach { case ((data, atom, message, description), index) =>
    test(s"${index + 1}. $description") {
      val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)
      assertEquals(
        new TaxPeriodDateChecker[DataOrigin.Browser]()
          .runCheck(checkerDependency(formModelOptics.formModelVisibilityOptics)),
        Invalid(
          Map(
            atom -> mutable.LinkedHashSet(
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
          dateMonthAtom -> VariadicValue.One("0"),
          dateYearAtom  -> VariadicValue.One("0")
        )
      ),
      dateMonthAtom,
      dateYearAtom,
      "Enter real month",
      "Enter real year",
      "Both month and year outside range"
    )
  )

  table1.zipWithIndex.foreach { case ((data, atom1, atom2, message1, message2, description), index) =>
    test(s"${index + 1}. $description") {
      val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(formTemplate, data)
      assertEquals(
        new TaxPeriodDateChecker[DataOrigin.Browser]()
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
