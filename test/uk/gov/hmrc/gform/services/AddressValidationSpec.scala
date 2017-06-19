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
import org.scalatest.{FlatSpec, Matchers}
import uk.gov.hmrc.gform.models.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.gform.service.ValidationService.CompData

class AddressValidationSpec extends FlatSpec with Matchers with EitherMatchers {

  "non-international" should "accept uk, street1, town and postcode only" in {
    val address = Address(international = false)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, true, true, false)

    val data = Map(FieldId("x-uk") -> Seq("true"),
      FieldId("x-street1") -> Seq("S"),
      FieldId("x-town") -> Seq("T"),
      FieldId("x-postcode") -> Seq("P1 1P"))

    val result : ValidatedType = CompData(speccedAddress, data).validateComponents

    result.value should be (())
  }

  "non-international" should "return invalid for town and postcode, but no street1" in {
    val address = Address(international = false)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, true, true, false)

    val data = Map(FieldId("x-uk") -> Seq("true"),
      FieldId("x-town") -> Seq("T"),
      FieldId("x-postcode") -> Seq("P1 1P"))

    val result : ValidatedType = CompData(speccedAddress, data).validateComponents

    result.toEither should beLeft(Map(speccedAddress.id.withJSSafeSuffix("street1") -> Set("must be entered")))
  }

  "non-international" should "return invalid for street1 and postcode, but no town" in {
    val address = Address(international = false)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, true, true, false)

    val data = Map(FieldId("x-uk") -> Seq("true"),
      FieldId("x-street1") -> Seq("S"),
      FieldId("x-postcode") -> Seq("P1 1P"))

    val result : ValidatedType = CompData(speccedAddress, data).validateComponents

    result.toEither should beLeft(Map(speccedAddress.id.withJSSafeSuffix("town") -> Set("must be entered")))
  }

  "non-international" should "return invalid for street1, town but no postcode" in {
    val address = Address(international = false)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, true, true, false)

    val data = Map(FieldId("x-uk") -> Seq("true"),
      FieldId("x-street1") -> Seq("S"),
      FieldId("x-town") -> Seq("T"))

    val result : ValidatedType = CompData(speccedAddress, data).validateComponents

    result.toEither should beLeft(Map(speccedAddress.id.withJSSafeSuffix("postcode") -> Set("must be entered")))
  }

  "non-international" should "return invalid address, street1, town but no postcode" in {
    val address = Address(international = false)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, true, true, false)

    val data = Map(FieldId("x-uk") -> Seq("true"),
      FieldId("x-street1") -> Seq("S"),
      FieldId("x-town") -> Seq("T"))

    val result : ValidatedType = CompData(speccedAddress, data).validateComponents

    result.toEither should beLeft(Map(speccedAddress.id.withJSSafeSuffix("postcode") -> Set("must be entered")))
  }

  "international" should "accept not uk, street1, town and country" in {
    val address = Address(international = true)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, true, true, false)

    val data = Map(FieldId("x-uk") -> Seq("false"),
      FieldId("x-street1") -> Seq("S"),
      FieldId("x-town") -> Seq("T"),
      FieldId("x-country") -> Seq("C"))

    val result : ValidatedType = CompData(speccedAddress, data).validateComponents

    result.value should be (())
  }

  "international" should "return invalid for not uk, street1, town and postcode but no country" in {
    val address = Address(international = true)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, true, true, false)

    val data = Map(FieldId("x-uk") -> Seq("false"),
      FieldId("x-street1") -> Seq("S"),
      FieldId("x-town") -> Seq("T"),
      FieldId("x-postcode") -> Seq("P1 1P"))

    val result : ValidatedType = CompData(speccedAddress, data).validateComponents

    result.toEither should beLeft(Map(speccedAddress.id.withJSSafeSuffix("postcode") -> Set("must not be entered"),
      speccedAddress.id.withJSSafeSuffix("country") -> Set("must be entered")))
  }

  "international" should "return invalid for not uk, street1, town, postcode and country" in {
    val address = Address(international = true)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, true, true, false)

    val data = Map(FieldId("x-uk") -> Seq("false"),
      FieldId("x-street1") -> Seq("S"),
      FieldId("x-town") -> Seq("T"),
      FieldId("x-postcode") -> Seq("P1 1P"),
      FieldId("x-country") -> Seq("C"))

    val result : ValidatedType = CompData(speccedAddress, data).validateComponents

    result.toEither should beLeft(Map(speccedAddress.id.withJSSafeSuffix("postcode") -> Set("must not be entered")))
  }

  "international" should "return invalid for uk, street1, town and country, but no postcode" in {
    val address = Address(international = true)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, true, true, false)

    val data = Map(FieldId("x-uk") -> Seq("true"),
      FieldId("x-street1") -> Seq("S"),
      FieldId("x-town") -> Seq("T"),
      FieldId("x-country") -> Seq("C"))

    val result : ValidatedType = CompData(speccedAddress, data).validateComponents

    result.toEither should beLeft(Map(speccedAddress.id.withJSSafeSuffix("postcode") -> Set("must be entered"),
      speccedAddress.id.withJSSafeSuffix("country") -> Set("must not be entered")))
  }



}