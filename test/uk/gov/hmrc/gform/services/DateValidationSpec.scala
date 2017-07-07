/*
 * Copyright 2017 HM Revenue & Customs
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

import cats.scalatest.EitherMatchers
import cats.scalatest.ValidatedValues._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar.mock
import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.gformbackend.model.EnvelopeId
import uk.gov.hmrc.gform.models.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.gform.validation.ComponentsValidator
import uk.gov.hmrc.play.http.HeaderCarrier

class DateValidationSpec extends FlatSpec with Matchers with EitherMatchers with ScalaFutures {

  "After Today 1" should "accepts dates after tomorrow" in {
    val dateConstraint = List(DateConstraint(After, Today, OffsetDate(1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val speccedDate = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true, false, false)

    val acceptedAfter = LocalDate.now().plusDays(2)

    val data = Map(
      FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString)
    )

    val result: ValidatedType = new ComponentsValidator(speccedDate, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.value shouldBe (())
  }

  "After Today 0" should "accepts dates after today" in {
    val dateConstraint = List(DateConstraint(After, Today, OffsetDate(0)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true, false, false)

    val acceptedAfter = LocalDate.now().plusDays(1)

    val data = Map(
      FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString)
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beRight(())
  }

  "After 2017-06-16 5" should "accepts dates after 2017-06-21" in {
    val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true, false, false)

    val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(6)

    val data = Map(
      FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString)
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.value shouldBe (())
  }

  "validation for After 2017-06-16 5" should "return invalid for dates that aren't after 2017-06-21" in {
    val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true, false, false)

    val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(2)

    val data = Map(
      FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString)
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set(s"Date should be after 2017-06-21")))
  }

  "After Today -1" should "accepts today and dates in future" in {
    val dateConstraint = List(DateConstraint(After, Today, OffsetDate(-1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true, false, false)

    val acceptedAfter = LocalDate.now()

    val data = Map(
      FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString)
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.value shouldBe (())
  }

  "After 2017-06-16 -5" should "accepts dates after 2017-06-11" in {
    val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true, false, false)

    val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(-4)

    val data = Map(
      FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString)
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.value shouldBe (())
  }

  "Before Today 1" should "accepts today and dates in past" in {
    val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true, false, false)

    val acceptedAfter = LocalDate.now()

    val data = Map(
      FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString)
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.value shouldBe (())
  }

  "Before Today 0" should "accepts dates before today" in {
    val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(0)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true, false, false)

    val acceptedAfter = LocalDate.now().plusDays(-1)

    val data = Map(
      FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString)
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.value shouldBe (())
  }

  "Before Today -1" should "accepts dates before yesterday" in {
    val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(-1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true, false, false)

    val acceptedAfter = LocalDate.now().plusDays(-2)

    val data = Map(
      FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString)
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.value shouldBe (())
  }

  "Before 2017-06-16 -5" should "accepts dates before 2017-06-11" in {
    val dateConstraint = List(DateConstraint(Before, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true, false, false)

    val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(-6)

    val data = Map(
      FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString)
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.value shouldBe (())
  }

  "Date 35-12-2017" should "return Is not Valid" in {
    val dateConstraint = List(DateConstraint(Before, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true, false, false)

    val data = Map(
      FieldId("accPeriodStartDate.day") -> Seq("35"),
      FieldId("accPeriodStartDate.month") -> Seq("12"),
      FieldId("accPeriodStartDate.year") -> Seq("2017")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id.withSuffix("day") -> Set(s"entered is greater than 31")))
  }

  "Date 15-5-222017" should "Invalid number of digits" in {
    val dateConstraint = List(DateConstraint(Before, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true, false, false)

    val data = Map(
      FieldId("accPeriodStartDate.day") -> Seq("15"),
      FieldId("accPeriodStartDate.month") -> Seq("5"),
      FieldId("accPeriodStartDate.year") -> Seq("222017")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id.withSuffix("year") -> Set(s"is not a 4 digit number")))
  }

  /**
   * Without Date Constraints
   */
  "Date validations" should "be applied apparently from mandatory field" in {
    val date = Date(AnyDate, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, false,
      false, false)

    val data = Map(
      FieldId("accPeriodStartDate.day") -> Seq("Tuesday"),
      FieldId("accPeriodStartDate.month") -> Seq("Jan"),
      FieldId("accPeriodStartDate.year") -> Seq(LocalDate.now().getYear.toString)
    )

    val result: ValidatedType = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(
      fieldValue.id.withSuffix("day") -> Set(s"must be non-numeric"),
      fieldValue.id.withSuffix("month") -> Set(s"must be non-numeric")
    ))
  }

  "Date validations" should "fail if field ids are using wrong separator" in {
    val date = Date(AnyDate, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, false,
      false, false)

    val data = Map(
      FieldId("accPeriodStartDate-day") -> Seq("01"),
      FieldId("accPeriodStartDate-month") -> Seq("01"),
      FieldId("accPeriodStartDate-year") -> Seq("1970")
    )

    val result: ValidatedType = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(FieldId("accPeriodStartDate") -> Set("Date is missing")))
  }

  implicit lazy val hc = HeaderCarrier()
}