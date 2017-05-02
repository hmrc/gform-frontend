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

package uk.gov.hmrc.bforms.services

import java.time.LocalDate

import org.scalatest.{FlatSpec, Matchers}
import uk.gov.hmrc.bforms.models.ValidationUtil.ValidatedType
import uk.gov.hmrc.bforms.models.components._
import uk.gov.hmrc.bforms.service.ValidationService.CompData

class DateValidationSpec extends FlatSpec with Matchers {

  "After Today 1" should "accepts dates after tomorrow" in {
    val dateConstraint = List(DateConstraint(After, Today, OffsetDate(1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, true, false, false)

    val acceptedAfter = LocalDate.now().plusDays(2).getDayOfMonth

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.toString),
      FieldId("accPeriodStartDate.month") -> Seq(LocalDate.now().getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(LocalDate.now().getYear.toString))

    val result: ValidatedType = CompData(fieldValue, data).validateComponents

    val unit: Unit = ()

    result.isValid shouldBe true
    result.getOrElse("") shouldBe unit
  }

  "After Today 0" should "accepts dates after today" in {
    val dateConstraint = List(DateConstraint(After, Today, OffsetDate(0)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, true, false, false)

    val acceptedAfter = LocalDate.now().plusDays(1).getDayOfMonth

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.toString),
      FieldId("accPeriodStartDate.month") -> Seq(LocalDate.now.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(LocalDate.now.getYear.toString))

    val result = CompData(fieldValue, data).validateComponents

    val unit: Unit = ()

    result.isValid shouldBe true
    result.getOrElse("") shouldBe unit
  }

  "After 2017-06-16 5" should "accepts dates after 2017-06-21" in {
    val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, true, false, false)

    val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(6)

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString))

    val result = CompData(fieldValue, data).validateComponents

    val unit: Unit = ()

    result.isValid shouldBe true
    result.getOrElse("") shouldBe unit
  }

  "validation for After 2017-06-16 5" should "return invalid for dates that aren't after 2017-06-21" in {
    val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, true, false, false)

    val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(2)

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString))

    val result = CompData(fieldValue, data).validateComponents

    result.isInvalid shouldBe true
  }

  "After Today -1" should "accepts today and dates in future" in {
    val dateConstraint = List(DateConstraint(After, Today, OffsetDate(-1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, true, false, false)

    val acceptedAfter = LocalDate.now().getDayOfMonth

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.toString),
      FieldId("accPeriodStartDate.month") -> Seq("9"),
      FieldId("accPeriodStartDate.year") -> Seq("2017"))

    val result = CompData(fieldValue, data).validateComponents

    val unit: Unit = ()

    result.isValid shouldBe true
    result.getOrElse("") shouldBe unit
  }

  "After 2017-06-16 -5" should "accepts dates after 2017-06-11" in {
    val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, true, false, false)

    val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(-4)

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString))

    val result = CompData(fieldValue, data).validateComponents

    val unit: Unit = ()

    result.isValid shouldBe true
    result.getOrElse("") shouldBe unit
  }


  "Before Today 1" should "accepts today and dates in past" in {
    val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, true, false, false)

    val acceptedAfter = LocalDate.now().getDayOfMonth

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.toString),
      FieldId("accPeriodStartDate.month") -> Seq("5"),
      FieldId("accPeriodStartDate.year") -> Seq("2017"))

    val result = CompData(fieldValue, data).validateComponents

    val unit: Unit = ()

    result.isValid shouldBe true
    result.getOrElse("") shouldBe unit
  }


  "Before Today 0" should "accepts dates before today" in {
    val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(0)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, true, false, false)

    val acceptedAfter = LocalDate.now().plusDays(-1).getDayOfMonth

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.toString),
      FieldId("accPeriodStartDate.month") -> Seq("2"),
      FieldId("accPeriodStartDate.year") -> Seq("2017"))

    val result = CompData(fieldValue, data).validateComponents

    val unit: Unit = ()

    result.isValid shouldBe true
    result.getOrElse("") shouldBe unit
  }


  "Before Today -1" should "accepts dates before yesterday" in {
    val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(-1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, true, false, false)

    val acceptedAfter = LocalDate.now().plusDays(-2).getDayOfMonth

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.toString),
      FieldId("accPeriodStartDate.month") -> Seq("1"),
      FieldId("accPeriodStartDate.year") -> Seq("2017"))

    val result = CompData(fieldValue, data).validateComponents

    val unit: Unit = ()

    result.isValid shouldBe true
    result.getOrElse("") shouldBe unit
  }

  "Before 2017-06-16 -5" should "accepts dates before 2017-06-11" in {
    val dateConstraint = List(DateConstraint(Before, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, true, false, false)

    val acceptedAfter = LocalDate.of(2017, 6, 16).plusDays(-6)

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter.getDayOfMonth.toString),
      FieldId("accPeriodStartDate.month") -> Seq(acceptedAfter.getMonthValue.toString),
      FieldId("accPeriodStartDate.year") -> Seq(acceptedAfter.getYear.toString))

    val result = CompData(fieldValue, data).validateComponents

    val unit: Unit = ()

    result.isValid shouldBe true
    result.getOrElse("") shouldBe unit
  }
}