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

package uk.gov.hmrc.gform.validation

/* import cats.instances.future._
 * import cats.scalatest.EitherMatchers
 * import cats.scalatest.ValidatedValues._
 * import java.time.LocalDate
 *
 * import org.scalatest.concurrent.ScalaFutures
 * import org.scalatest.mockito.MockitoSugar.mock
 * import org.scalatest.{ FlatSpec, Matchers }
 * import play.api.i18n.Messages
 * import uk.gov.hmrc.gform.Helpers.toSmartString
 * import uk.gov.hmrc.gform.GraphSpec
 * import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
 * import uk.gov.hmrc.gform.fileupload.Envelope
 * import uk.gov.hmrc.gform.lookup.LookupRegistry
 * import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, SmartString, VariadicFormData }
 * import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormDataRecalculated, ThirdPartyData }
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate._
 * import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
 * import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
 * import uk.gov.hmrc.http.HeaderCarrier
 *
 * import scala.concurrent.ExecutionContext.Implicits.global
 *
 * class DateValidationSpec(implicit messages: Messages, l: LangADT)
 *     extends FlatSpec with Matchers with EitherMatchers with ScalaFutures with GraphSpec {
 *   val retrievals = mock[MaterialisedRetrievals]
 *
 *   private val lookupRegistry = new LookupRegistry(Map.empty)
 *
 *   implicit lazy val hc = HeaderCarrier()
 *
 *   implicit val smartStringEvaluator: SmartStringEvaluator = new SmartStringEvaluator {
 *     override def apply(s: SmartString, markDown: Boolean): String = s.rawValue(LangADT.En)
 *   }
 *
 *   private def mkComponentsValidator(data: FormDataRecalculated): ComponentsValidator =
 *     new ComponentsValidator(
 *       data,
 *       EnvelopeId("whatever"),
 *       Envelope.empty,
 *       retrievals,
 *       booleanExprEval,
 *       ThirdPartyData.empty,
 *       ExampleData.formTemplate,
 *       lookupRegistry)
 *
 *   private def mkFormComponent(date: Date) =
 *     FormComponent(
 *       FormComponentId("accPeriodStartDate"),
 *       date,
 *       toSmartString("sample label"),
 *       None,
 *       None,
 *       None,
 *       true,
 *       false,
 *       false,
 *       true,
 *       false,
 *       None)
 *
 *   "After Today 1" should "accepts dates after tomorrow" in {
 *     val dateConstraint = List(DateConstraint(After, Today, OffsetDate(1)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val speccedDate = mkFormComponent(date)
 *     val speccedDateList = List(speccedDate)
 *
 *     val acceptedAfter = LocalDate.now().plusDays(2)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> acceptedAfter.getDayOfMonth.toString,
 *         FormComponentId("accPeriodStartDate-month") -> acceptedAfter.getMonthValue.toString,
 *         FormComponentId("accPeriodStartDate-year")  -> acceptedAfter.getYear.toString
 *       ))
 *
 *     val result: ValidatedType[Unit] =
 *       mkComponentsValidator(data).validate(speccedDate, speccedDateList, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.value shouldBe (())
 *   }
 *
 *   "After Today 0" should "accepts dates after today" in {
 *     val dateConstraint = List(DateConstraint(After, Today, OffsetDate(0)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val acceptedAfter = LocalDate.now().plusDays(1)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> acceptedAfter.getDayOfMonth.toString,
 *         FormComponentId("accPeriodStartDate-month") -> acceptedAfter.getMonthValue.toString,
 *         FormComponentId("accPeriodStartDate-year")  -> acceptedAfter.getYear.toString
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.toEither should beRight(())
 *   }
 *
 *   "After 2017-06-16 5" should "accepts dates after 2017-06-21" in {
 *     val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(5)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(6)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> acceptedAfter.getDayOfMonth.toString,
 *         FormComponentId("accPeriodStartDate-month") -> acceptedAfter.getMonthValue.toString,
 *         FormComponentId("accPeriodStartDate-year")  -> acceptedAfter.getYear.toString
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.value shouldBe (())
 *   }
 *
 *   "validation for After 2017-06-16 5" should "return invalid for dates that aren't after 2017-06-21" in {
 *     val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(5)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(2)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> acceptedAfter.getDayOfMonth.toString,
 *         FormComponentId("accPeriodStartDate-month") -> acceptedAfter.getMonthValue.toString,
 *         FormComponentId("accPeriodStartDate-year")  -> acceptedAfter.getYear.toString
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.toEither should beLeft(Map(fieldValue.id -> Set("sample label must be after 21 June 2017")))
 *   }
 *
 *   "After Today -1" should "accepts today and dates in future" in {
 *     val dateConstraint = List(DateConstraint(After, Today, OffsetDate(-1)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val acceptedAfter = LocalDate.now()
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> acceptedAfter.getDayOfMonth.toString,
 *         FormComponentId("accPeriodStartDate-month") -> acceptedAfter.getMonthValue.toString,
 *         FormComponentId("accPeriodStartDate-year")  -> acceptedAfter.getYear.toString
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.value shouldBe (())
 *   }
 *
 *   "After 2017-06-16 -5" should "accepts dates after 2017-06-11" in {
 *     val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(-4)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> acceptedAfter.getDayOfMonth.toString,
 *         FormComponentId("accPeriodStartDate-month") -> acceptedAfter.getMonthValue.toString,
 *         FormComponentId("accPeriodStartDate-year")  -> acceptedAfter.getYear.toString
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.value shouldBe (())
 *   }
 *
 *   "Before Today 1" should "accepts today and dates in past" in {
 *     val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(1)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val acceptedAfter = LocalDate.now()
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> acceptedAfter.getDayOfMonth.toString,
 *         FormComponentId("accPeriodStartDate-month") -> acceptedAfter.getMonthValue.toString,
 *         FormComponentId("accPeriodStartDate-year")  -> acceptedAfter.getYear.toString
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.value shouldBe (())
 *   }
 *
 *   "Before Today 0" should "accepts dates before today" in {
 *     val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(0)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val acceptedAfter = LocalDate.now().plusDays(-1)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> acceptedAfter.getDayOfMonth.toString,
 *         FormComponentId("accPeriodStartDate-month") -> acceptedAfter.getMonthValue.toString,
 *         FormComponentId("accPeriodStartDate-year")  -> acceptedAfter.getYear.toString
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.value shouldBe (())
 *   }
 *
 *   "Before Today -1" should "accepts dates before yesterday" in {
 *     val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(-1)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val acceptedAfter = LocalDate.now().plusDays(-2)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> acceptedAfter.getDayOfMonth.toString,
 *         FormComponentId("accPeriodStartDate-month") -> acceptedAfter.getMonthValue.toString,
 *         FormComponentId("accPeriodStartDate-year")  -> acceptedAfter.getYear.toString
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.value shouldBe (())
 *   }
 *
 *   "Before 2017-06-16 -5" should "accepts dates before 2017-06-11" in {
 *     val dateConstraint = List(DateConstraint(Before, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(-6)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> acceptedAfter.getDayOfMonth.toString,
 *         FormComponentId("accPeriodStartDate-month") -> acceptedAfter.getMonthValue.toString,
 *         FormComponentId("accPeriodStartDate-year")  -> acceptedAfter.getYear.toString
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.value shouldBe (())
 *   }
 *
 *   "Precisely YYYY-04-DD" should "accept any date that in April" in {
 *     val dateConstraint = List(DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Exact(4), Day.Any), OffsetDate(0)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val accepted = LocalDate.of(2017, 4, 16)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> accepted.getDayOfMonth.toString,
 *         FormComponentId("accPeriodStartDate-month") -> accepted.getMonthValue.toString,
 *         FormComponentId("accPeriodStartDate-year")  -> accepted.getYear.toString
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.value shouldBe (())
 *   }
 *
 *   "Precisely YYYY-MM-lastDay" should "accept any date that is the last day of the given month" in {
 *     val dateConstraint = List(DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Any, Day.Last), OffsetDate(0)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val accepted = LocalDate.of(2020, 2, 29)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> accepted.getDayOfMonth.toString,
 *         FormComponentId("accPeriodStartDate-month") -> accepted.getMonthValue.toString,
 *         FormComponentId("accPeriodStartDate-year")  -> accepted.getYear.toString
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.value shouldBe (())
 *   }
 *
 *   "Precisely next-MM-DD" should "accept any date that is next year" in {
 *     val dateConstraint = List(DateConstraint(Precisely, ConcreteDate(Next, Month.Any, Day.Any), OffsetDate(0)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val accepted = LocalDate.of(LocalDate.now().getYear + 1, 2, 29)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> accepted.getDayOfMonth.toString,
 *         FormComponentId("accPeriodStartDate-month") -> accepted.getMonthValue.toString,
 *         FormComponentId("accPeriodStartDate-year")  -> accepted.getYear.toString
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.value shouldBe (())
 *   }
 *
 *   "Date 26-02-2020" should "return Is not Valid when lastDay validation is applied" in {
 *     val dateConstraint = List(DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Any, Day.Last), OffsetDate(0)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> "26",
 *         FormComponentId("accPeriodStartDate-month") -> "02",
 *         FormComponentId("accPeriodStartDate-year")  -> "2020"
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.toEither should beLeft(Map(fieldValue.id -> Set(s"sample label must be the last day of the month")))
 *   }
 *
 *   "Date 26-02-2020" should "return Is not Valid when firstDay validation is applied" in {
 *     val dateConstraint = List(DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Any, Day.First), OffsetDate(0)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> "26",
 *         FormComponentId("accPeriodStartDate-month") -> "02",
 *         FormComponentId("accPeriodStartDate-year")  -> "2020"
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.toEither should beLeft(Map(fieldValue.id -> Set(s"sample label must be the first day of the month")))
 *   }
 *
 *   "Date 35-12-2017" should "return Is not Valid" in {
 *     val dateConstraint = List(DateConstraint(Before, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> "35",
 *         FormComponentId("accPeriodStartDate-month") -> "12",
 *         FormComponentId("accPeriodStartDate-year")  -> "2017"
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.toEither should beLeft(
 *       Map(fieldValue.id.withSuffix("day") -> Set(s"sample label day must not be greater than 31")))
 *   }
 *
 *   "Date 15-5-222017" should "Invalid number of digits" in {
 *     val dateConstraint = List(DateConstraint(Before, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
 *     val constraints = DateConstraints(dateConstraint)
 *     val date = Date(constraints, Offset(0), None)
 *
 *     val fieldValue = mkFormComponent(date)
 *     val fieldValues = List(fieldValue)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> "15",
 *         FormComponentId("accPeriodStartDate-month") -> "5",
 *         FormComponentId("accPeriodStartDate-year")  -> "222017"
 *       ))
 *
 *     val result =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.toEither should beLeft(
 *       Map(fieldValue.id.withSuffix("year") -> Set(s"sample label year must be a 4 digit number")))
 *   }
 *
 *   /\**
 *     * Without Date Constraints
 *     *\/
 *   "Date validations" should "be applied apparently from mandatory field" in {
 *     val date = Date(AnyDate, Offset(0), None)
 *
 *     val fieldValue = FormComponent(
 *       FormComponentId("accPeriodStartDate"),
 *       date,
 *       toSmartString("sample label"),
 *       None,
 *       None,
 *       None,
 *       false,
 *       false,
 *       false,
 *       true,
 *       false,
 *       None)
 *
 *     val fieldValues = List(fieldValue)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> "Tuesday",
 *         FormComponentId("accPeriodStartDate-month") -> "Jan",
 *         FormComponentId("accPeriodStartDate-year")  -> LocalDate.now().getYear.toString
 *       ))
 *
 *     val result: ValidatedType[Unit] =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.toEither should beLeft(
 *       Map(
 *         fieldValue.id.withSuffix("day")   -> Set("sample label day must be numeric"),
 *         fieldValue.id.withSuffix("month") -> Set("sample label month must be numeric")
 *       ))
 *   }
 *
 *   "Date validations" should "return suitable error if empty" in {
 *     val date = Date(AnyDate, Offset(0), None)
 *
 *     val fieldValue = FormComponent(
 *       FormComponentId("accPeriodStartDate"),
 *       date,
 *       toSmartString("sample label"),
 *       None,
 *       None,
 *       None,
 *       false,
 *       false,
 *       false,
 *       true,
 *       false,
 *       None)
 *
 *     val fieldValues = List(fieldValue)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate-day")   -> "",
 *         FormComponentId("accPeriodStartDate-month") -> "",
 *         FormComponentId("accPeriodStartDate-year")  -> ""
 *       ))
 *
 *     val result: ValidatedType[Unit] =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.toEither should beLeft(
 *       Map(
 *         fieldValue.id.withSuffix("day")   -> Set("sample label must be entered"),
 *         fieldValue.id.withSuffix("month") -> Set("sample label must be entered"),
 *         fieldValue.id.withSuffix("year")  -> Set("sample label must be entered")
 *       ))
 *   }
 *
 *   "Date validations" should "fail if field ids are using wrong separator" in {
 *     val date = Date(AnyDate, Offset(0), None)
 *
 *     val fieldValue = FormComponent(
 *       FormComponentId("accPeriodStartDate"),
 *       date,
 *       toSmartString("sample label"),
 *       None,
 *       None,
 *       None,
 *       false,
 *       false,
 *       false,
 *       true,
 *       false,
 *       None)
 *
 *     val fieldValues = List(fieldValue)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate.day")   -> "01",
 *         FormComponentId("accPeriodStartDate.month") -> "01",
 *         FormComponentId("accPeriodStartDate.year")  -> "1970"
 *       ))
 *
 *     val result: ValidatedType[Unit] =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.toEither should beLeft(Map(FormComponentId("accPeriodStartDate") -> Set("sample label is missing")))
 *   }
 *
 *   "Date validations" should "return supplied error message" in {
 *     val date = Date(AnyDate, Offset(0), None)
 *
 *     val fieldValue = FormComponent(
 *       FormComponentId("accPeriodStartDate"),
 *       date,
 *       toSmartString("sample label"),
 *       None,
 *       None,
 *       None,
 *       false,
 *       false,
 *       false,
 *       true,
 *       false,
 *       Some(toSmartString("New error message"))
 *     )
 *
 *     val fieldValues = List(fieldValue)
 *
 *     val data = mkFormDataRecalculated(
 *       VariadicFormData.ones(
 *         FormComponentId("accPeriodStartDate.day")   -> "01",
 *         FormComponentId("accPeriodStartDate.month") -> "01",
 *         FormComponentId("accPeriodStartDate.year")  -> "1970"
 *       ))
 *
 *     val result: ValidatedType[Unit] =
 *       mkComponentsValidator(data).validate(fieldValue, fieldValues, GetEmailCodeFieldMatcher.noop).futureValue
 *
 *     result.toEither should beLeft(Map(FormComponentId("accPeriodStartDate") -> Set("New error message")))
 *   }
 * } */
