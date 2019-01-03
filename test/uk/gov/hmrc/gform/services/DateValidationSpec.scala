/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.services

import java.time.LocalDate

import cats.instances.future._
import cats.scalatest.EitherMatchers
import cats.scalatest.ValidatedValues._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar.mock
import org.scalatest.{ FlatSpec, Matchers }
import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.gform.GraphSpec
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ComponentsValidator
import uk.gov.hmrc.http.HeaderCarrier

class DateValidationSpec extends FlatSpec with Matchers with EitherMatchers with ScalaFutures with GraphSpec {
  val retrievals = mock[MaterialisedRetrievals]
  implicit lazy val hc = HeaderCarrier()

  "After Today 1" should "accepts dates after tomorrow" in {
    val dateConstraint = List(DateConstraint(After, Today, OffsetDate(1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val speccedDate = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      true,
      false,
      false,
      true,
      false,
      None)

    val acceptedAfter = LocalDate.now().plusDays(2)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate-day")   -> Seq(acceptedAfter.getDayOfMonth.toString),
        FormComponentId("accPeriodStartDate-month") -> Seq(acceptedAfter.getMonthValue.toString),
        FormComponentId("accPeriodStartDate-year")  -> Seq(acceptedAfter.getYear.toString)
      ))

    val result: ValidatedType = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate).validate(speccedDate).futureValue

    result.value shouldBe (())
  }

  "After Today 0" should "accepts dates after today" in {
    val dateConstraint = List(DateConstraint(After, Today, OffsetDate(0)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      true,
      false,
      false,
      true,
      false,
      None)

    val acceptedAfter = LocalDate.now().plusDays(1)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate-day")   -> Seq(acceptedAfter.getDayOfMonth.toString),
        FormComponentId("accPeriodStartDate-month") -> Seq(acceptedAfter.getMonthValue.toString),
        FormComponentId("accPeriodStartDate-year")  -> Seq(acceptedAfter.getYear.toString)
      ))

    val result = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate)
      .validate(fieldValue)
      .futureValue

    result.toEither should beRight(())
  }

  "After 2017-06-16 5" should "accepts dates after 2017-06-21" in {
    val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      true,
      false,
      false,
      true,
      false,
      None)

    val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(6)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate-day")   -> Seq(acceptedAfter.getDayOfMonth.toString),
        FormComponentId("accPeriodStartDate-month") -> Seq(acceptedAfter.getMonthValue.toString),
        FormComponentId("accPeriodStartDate-year")  -> Seq(acceptedAfter.getYear.toString)
      ))

    val result = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate)
      .validate(fieldValue)
      .futureValue

    result.value shouldBe (())
  }

  "validation for After 2017-06-16 5" should "return invalid for dates that aren't after 2017-06-21" in {
    val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      true,
      false,
      false,
      true,
      false,
      None)

    val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(2)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate-day")   -> Seq(acceptedAfter.getDayOfMonth.toString),
        FormComponentId("accPeriodStartDate-month") -> Seq(acceptedAfter.getMonthValue.toString),
        FormComponentId("accPeriodStartDate-year")  -> Seq(acceptedAfter.getYear.toString)
      ))

    val result = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate)
      .validate(fieldValue)
      .futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("sample label should be after 21 June 2017")))
  }

  "After Today -1" should "accepts today and dates in future" in {
    val dateConstraint = List(DateConstraint(After, Today, OffsetDate(-1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      true,
      false,
      false,
      true,
      false,
      None)

    val acceptedAfter = LocalDate.now()

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate-day")   -> Seq(acceptedAfter.getDayOfMonth.toString),
        FormComponentId("accPeriodStartDate-month") -> Seq(acceptedAfter.getMonthValue.toString),
        FormComponentId("accPeriodStartDate-year")  -> Seq(acceptedAfter.getYear.toString)
      ))

    val result = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate)
      .validate(fieldValue)
      .futureValue

    result.value shouldBe (())
  }

  "After 2017-06-16 -5" should "accepts dates after 2017-06-11" in {
    val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      true,
      false,
      false,
      true,
      false,
      None)

    val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(-4)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate-day")   -> Seq(acceptedAfter.getDayOfMonth.toString),
        FormComponentId("accPeriodStartDate-month") -> Seq(acceptedAfter.getMonthValue.toString),
        FormComponentId("accPeriodStartDate-year")  -> Seq(acceptedAfter.getYear.toString)
      ))

    val result = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate)
      .validate(fieldValue)
      .futureValue

    result.value shouldBe (())
  }

  "Before Today 1" should "accepts today and dates in past" in {
    val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      true,
      false,
      false,
      true,
      false,
      None)

    val acceptedAfter = LocalDate.now()

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate-day")   -> Seq(acceptedAfter.getDayOfMonth.toString),
        FormComponentId("accPeriodStartDate-month") -> Seq(acceptedAfter.getMonthValue.toString),
        FormComponentId("accPeriodStartDate-year")  -> Seq(acceptedAfter.getYear.toString)
      ))

    val result = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate)
      .validate(fieldValue)
      .futureValue

    result.value shouldBe (())
  }

  "Before Today 0" should "accepts dates before today" in {
    val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(0)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      true,
      false,
      false,
      true,
      false,
      None)

    val acceptedAfter = LocalDate.now().plusDays(-1)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate-day")   -> Seq(acceptedAfter.getDayOfMonth.toString),
        FormComponentId("accPeriodStartDate-month") -> Seq(acceptedAfter.getMonthValue.toString),
        FormComponentId("accPeriodStartDate-year")  -> Seq(acceptedAfter.getYear.toString)
      ))

    val result = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate)
      .validate(fieldValue)
      .futureValue

    result.value shouldBe (())
  }

  "Before Today -1" should "accepts dates before yesterday" in {
    val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(-1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      true,
      false,
      false,
      true,
      false,
      None)

    val acceptedAfter = LocalDate.now().plusDays(-2)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate-day")   -> Seq(acceptedAfter.getDayOfMonth.toString),
        FormComponentId("accPeriodStartDate-month") -> Seq(acceptedAfter.getMonthValue.toString),
        FormComponentId("accPeriodStartDate-year")  -> Seq(acceptedAfter.getYear.toString)
      ))

    val result = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate)
      .validate(fieldValue)
      .futureValue

    result.value shouldBe (())
  }

  "Before 2017-06-16 -5" should "accepts dates before 2017-06-11" in {
    val dateConstraint = List(DateConstraint(Before, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      true,
      false,
      false,
      true,
      false,
      None)

    val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(-6)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate-day")   -> Seq(acceptedAfter.getDayOfMonth.toString),
        FormComponentId("accPeriodStartDate-month") -> Seq(acceptedAfter.getMonthValue.toString),
        FormComponentId("accPeriodStartDate-year")  -> Seq(acceptedAfter.getYear.toString)
      ))

    val result = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate)
      .validate(fieldValue)
      .futureValue

    result.value shouldBe (())
  }

  "Date 35-12-2017" should "return Is not Valid" in {
    val dateConstraint = List(DateConstraint(Before, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      true,
      false,
      false,
      true,
      false,
      None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate-day")   -> Seq("35"),
        FormComponentId("accPeriodStartDate-month") -> Seq("12"),
        FormComponentId("accPeriodStartDate-year")  -> Seq("2017")
      ))

    val result = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate)
      .validate(fieldValue)
      .futureValue

    result.toEither should beLeft(
      Map(fieldValue.id.withSuffix("day") -> Set(s"sample label day must not be greater than 31")))
  }

  "Date 15-5-222017" should "Invalid number of digits" in {
    val dateConstraint = List(DateConstraint(Before, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      true,
      false,
      false,
      true,
      false,
      None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate-day")   -> Seq("15"),
        FormComponentId("accPeriodStartDate-month") -> Seq("5"),
        FormComponentId("accPeriodStartDate-year")  -> Seq("222017")
      ))

    val result = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate)
      .validate(fieldValue)
      .futureValue

    result.toEither should beLeft(
      Map(fieldValue.id.withSuffix("year") -> Set(s"sample label year must be a 4 digit number")))
  }

  /**
    * Without Date Constraints
    */
  "Date validations" should "be applied apparently from mandatory field" in {
    val date = Date(AnyDate, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      false,
      false,
      false,
      true,
      false,
      None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate-day")   -> Seq("Tuesday"),
        FormComponentId("accPeriodStartDate-month") -> Seq("Jan"),
        FormComponentId("accPeriodStartDate-year")  -> Seq(LocalDate.now().getYear.toString)
      ))

    val result: ValidatedType = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate).validate(fieldValue).futureValue

    result.toEither should beLeft(
      Map(
        fieldValue.id.withSuffix("day")   -> Set("sample label day must be numeric"),
        fieldValue.id.withSuffix("month") -> Set("sample label month must be numeric")
      ))
  }

  "Date validations" should "fail if field ids are using wrong separator" in {
    val date = Date(AnyDate, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      false,
      false,
      false,
      true,
      false,
      None)

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate.day")   -> Seq("01"),
        FormComponentId("accPeriodStartDate.month") -> Seq("01"),
        FormComponentId("accPeriodStartDate.year")  -> Seq("1970")
      ))

    val result: ValidatedType = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate).validate(fieldValue).futureValue

    result.toEither should beLeft(Map(FormComponentId("accPeriodStartDate") -> Set("sample label is missing")))
  }

  "Date validations" should "return supplied error message" in {
    val date = Date(AnyDate, Offset(0), None)

    val fieldValue = FormComponent(
      FormComponentId("accPeriodStartDate"),
      date,
      "sample label",
      None,
      None,
      None,
      false,
      false,
      false,
      true,
      false,
      Some("New error message"))

    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("accPeriodStartDate.day")   -> Seq("01"),
        FormComponentId("accPeriodStartDate.month") -> Seq("01"),
        FormComponentId("accPeriodStartDate.year")  -> Seq("1970")
      ))

    val result: ValidatedType = new ComponentsValidator(
      data,
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate).validate(fieldValue).futureValue

    result.toEither should beLeft(Map(FormComponentId("accPeriodStartDate") -> Set("New error message")))
  }
}
