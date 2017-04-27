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
import org.slf4j.LoggerFactory
import uk.gov.hmrc.bforms.models.components._
import uk.gov.hmrc.bforms.service.ValidationService
import uk.gov.hmrc.bforms.service.ValidationService.CompData

/**
  * Created by dimitra on 21/04/17.
  */
class ValidationServiceSpec extends FlatSpec with Matchers {

  lazy val log = LoggerFactory.getLogger(classOf[ValidationServiceSpec])

  "After Today 1" should "accepts dates after tomorrow" in {
    val dateConstraint = List(DateConstraint(After, Today, OffsetDate(1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true)

    val acceptedAfter = (LocalDate.now().getDayOfMonth + 2).toString

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter),
      FieldId("accPeriodStartDate.month") -> Seq("4"),
      FieldId("accPeriodStartDate.year") -> Seq("2017"))

    val result = CompData(fieldValue, data).validateComponents

    log.info(">>>>> result: " + result)
//    result.size shouldBe 1
//    result.map(validated => validated.isValid) shouldBe List(true)
  }

  /*"After Today 0" should "accepts dates after today" in {
    val dateConstraint = List(DateConstraint(After, Today, OffsetDate(0)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true)

    val acceptedAfter = (LocalDate.now().getDayOfMonth + 1).toString

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter),
      FieldId("accPeriodStartDate.month") -> Seq("4"),
      FieldId("accPeriodStartDate.year") -> Seq("2017"))

    val result = CompData(fieldValue, data).validateComponents

    result.size shouldBe 1
    result.map(validated => validated.isValid) shouldBe List(true)
  }

  "After 2017-06-16 5" should "accepts dates after 2017-06-21" in {
    val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true)


    val data = Map(FieldId("accPeriodStartDate.day") -> Seq("22"),
      FieldId("accPeriodStartDate.month") -> Seq("6"),
      FieldId("accPeriodStartDate.year") -> Seq("2017"))

    val result = CompData(fieldValue, data).validateComponents

    result.size shouldBe 1
    result.map(validated => validated.isValid) shouldBe List(true)
  }

  "After Today -1" should "accepts today and dates in future" in {
    val dateConstraint = List(DateConstraint(After, Today, OffsetDate(-1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true)

    val acceptedAfter = LocalDate.now().getDayOfMonth.toString

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter),
      FieldId("accPeriodStartDate.month") -> Seq("4"),
      FieldId("accPeriodStartDate.year") -> Seq("2017"))

    val result = CompData(fieldValue, data).validateComponents

    result.size shouldBe 1
    result.map(validated => validated.isValid) shouldBe List(true)
  }

  "After 2017-06-16 -5" should "accepts dates after 2017-06-11" in {
    val dateConstraint = List(DateConstraint(After, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true)

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq("12"),
      FieldId("accPeriodStartDate.month") -> Seq("6"),
      FieldId("accPeriodStartDate.year") -> Seq("2017"))

    val result = CompData(fieldValue, data).validateComponents

    log.info(">>>>> result: " + result)
    result.size shouldBe 1
    result.map(validated => validated.isValid) shouldBe List(true)
  }

  // Before
  "Before Today 1" should "accepts today and dates in past" in {
    val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true)

    val acceptedAfter = LocalDate.now().getDayOfMonth.toString

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter),
      FieldId("accPeriodStartDate.month") -> Seq("4"),
      FieldId("accPeriodStartDate.year") -> Seq("2017"))

    val result = CompData(fieldValue, data).validateComponents

    result.size shouldBe 1
    result.map(validated => validated.isValid) shouldBe List(true)
  }

  "Before Today 0" should "accepts dates after today" in {
    val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(0)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true)

    val acceptedAfter = (LocalDate.now().getDayOfMonth - 1).toString

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter),
      FieldId("accPeriodStartDate.month") -> Seq("4"),
      FieldId("accPeriodStartDate.year") -> Seq("2017"))

    val result = CompData(fieldValue, data).validateComponents

    result.size shouldBe 1
    result.map(validated => validated.isValid) shouldBe List(true)
  }

  "Before Today -1" should "accepts dates before yesterday" in {
    val dateConstraint = List(DateConstraint(Before, Today, OffsetDate(-1)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true)

    val acceptedAfter = (LocalDate.now().getDayOfMonth - 2).toString

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq(acceptedAfter),
      FieldId("accPeriodStartDate.month") -> Seq("4"),
      FieldId("accPeriodStartDate.year") -> Seq("2017"))

    val result = CompData(fieldValue, data).validateComponents

    result.size shouldBe 1
    result.map(validated => validated.isValid) shouldBe List(true)
  }

  "Before 2017-06-16 -5" should "accepts dates before 2017-06-11" in {
    val dateConstraint = List(DateConstraint(Before, ConcreteDate(2017, 6, 16), OffsetDate(-5)))
    val constraints = DateConstraints(dateConstraint)
    val date = Date(constraints, Offset(0), None)

    val fieldValue = FieldValue(FieldId("accPeriodStartDate"), date,
      "sample label", None, None, true)

    val data = Map(FieldId("accPeriodStartDate.day") -> Seq("10"),
      FieldId("accPeriodStartDate.month") -> Seq("6"),
      FieldId("accPeriodStartDate.year") -> Seq("2017"))

    val result = CompData(fieldValue, data).validateComponents

    result.size shouldBe 1
    result.map(validated => validated.isValid) shouldBe List(true)
  }*/
}