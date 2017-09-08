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

import cats.scalatest.EitherMatchers
import cats.scalatest.ValidatedValues._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar.mock
import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.models.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Address, FieldId, FieldValue }
import uk.gov.hmrc.gform.validation.{ ComponentsValidator, ValidationValues }
import uk.gov.hmrc.play.http.HeaderCarrier

class AddressValidationSpec extends FlatSpec with Matchers with EitherMatchers with ScalaFutures {

  implicit lazy val hc = HeaderCarrier()

  "non-international" should "accept uk, street1, street3, streep 3, street4 and postcode" in {
    val address = Address(international = false)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, None, true, true, false, true, None)

    val data = Map(
      FieldId("x-uk") -> Seq("true"),
      FieldId("x-street1") -> Seq("S1"),
      FieldId("x-street2") -> Seq("S2"),
      FieldId("x-street3") -> Seq("S3"),
      FieldId("x-street4") -> Seq("S4"),
      FieldId("x-postcode") -> Seq("P1 1P")
    )
    val result: ValidatedType = new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever")).validate(speccedAddress).futureValue

    result.value should be(())
  }

  "non-international" should "accept uk, street1 and postcode only" in {
    val address = Address(international = false)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, None, true, true, false, true, None)

    val data = Map(
      FieldId("x-uk") -> Seq("true"),
      FieldId("x-street1") -> Seq("S"),
      FieldId("x-postcode") -> Seq("P1 1P")
    )

    val result: ValidatedType = new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever")).validate(speccedAddress).futureValue

    result.value should be(())
  }

  "non-international" should "return invalid for postcode, but no street1" in {
    val address = Address(international = false)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, None, true, true, false, true, None)

    val data = Map(
      FieldId("x-uk") -> Seq("true"),
      FieldId("x-postcode") -> Seq("P1 1P")
    )

    val result: ValidatedType = new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever")).validate(speccedAddress).futureValue

    result.toEither should beLeft(Map(speccedAddress.id.withSuffix("street1") -> Set("must be entered")))
  }

  "non-international" should "return invalid for street1 but no postcode" in {
    val address = Address(international = false)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, None, true, true, false, true, None)

    val data = Map(
      FieldId("x-uk") -> Seq("true"),
      FieldId("x-street1") -> Seq("S")
    )

    val result: ValidatedType = new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever")).validate(speccedAddress).futureValue

    result.toEither should beLeft(Map(speccedAddress.id.withSuffix("postcode") -> Set("must be entered")))
  }

  "international" should "accept not uk, street1, country" in {
    val address = Address(international = true)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, None, true, true, false, true, None)

    val data = Map(
      FieldId("x-uk") -> Seq("false"),
      FieldId("x-street1") -> Seq("S"),
      FieldId("x-country") -> Seq("C")
    )

    val result: ValidatedType = new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever")).validate(speccedAddress).futureValue

    result.value should be(())
  }

  "international" should "return invalid for not uk, street1, but no country" in {
    val address = Address(international = true)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, None, true, true, false, true, None)

    val data = Map(
      FieldId("x-uk") -> Seq("false"),
      FieldId("x-street1") -> Seq("S")
    )

    val result: ValidatedType = new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever")).validate(speccedAddress).futureValue

    result.toEither should beLeft(Map(speccedAddress.id.withSuffix("country") -> Set("must be entered")))
  }

  "international" should "return invalid for not uk, street1, postcode and country" in {
    val address = Address(international = true)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, None, true, true, false, true, None)

    val data = Map(
      FieldId("x-uk") -> Seq("false"),
      FieldId("x-street1") -> Seq("S"),
      FieldId("x-postcode") -> Seq("P1 1P"),
      FieldId("x-country") -> Seq("C")
    )

    val result: ValidatedType = new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever")).validate(speccedAddress).futureValue

    result.toEither should beLeft(Map(speccedAddress.id.withSuffix("postcode") -> Set("must not be entered")))
  }

  "international" should "return invalid for uk, street1, country, but no postcode" in {
    val address = Address(international = true)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, None, true, true, false, true, None)

    val data = Map(
      FieldId("x-uk") -> Seq("true"),
      FieldId("x-street1") -> Seq("S"),
      FieldId("x-country") -> Seq("C")
    )

    val result: ValidatedType = new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever")).validate(speccedAddress).futureValue

    result.toEither should beLeft(Map(
      speccedAddress.id.withSuffix("postcode") -> Set("must be entered"),
      speccedAddress.id.withSuffix("country") -> Set("must not be entered")
    ))
  }

  "Address validation" should "fail when field separator is wrong" in {
    val address = Address(international = true)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, None, true, true, false, true, None)

    val data = Map(
      FieldId("x@uk") -> Seq("true"),
      FieldId("x@street1") -> Seq("S"),
      FieldId("x@country") -> Seq("C")
    )

    val result: ValidatedType = new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever")).validate(speccedAddress).futureValue

    result.toEither should beLeft(
      Map(
        FieldId("x-country") -> Set("must be entered"),
        FieldId("x-street1") -> Set("must be entered")
      )
    )
  }

  "Address validation" should "throw back custom validation" in {
    val address = Address(international = true)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, None, true, true, false, true, Some("New Error Message"))

    val data = Map(
      FieldId("x@uk") -> Seq("true"),
      FieldId("x@street1") -> Seq("S"),
      FieldId("x@country") -> Seq("C")
    )

    val result: ValidatedType = new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever")).validate(speccedAddress).futureValue

    result.toEither should beLeft(
      Map(
        FieldId("x-country") -> Set("New Error Message"),
        FieldId("x-street1") -> Set("New Error Message")
      )
    )
  }

  "Address validation" should "pass with valid data" in {
    val address = Address(international = true)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, None, true, true, false, true, None)

    val data = Map(
      FieldId("x-uk") -> Seq("true"),
      FieldId("x-street1") -> Seq(List.fill(ValidationValues.addressLine)("a").mkString),
      FieldId("x-street2") -> Seq(List.fill(ValidationValues.addressLine)("a").mkString),
      FieldId("x-street3") -> Seq(List.fill(ValidationValues.addressLine)("a").mkString),
      FieldId("x-street4") -> Seq(List.fill(ValidationValues.addressLine4)("a").mkString),
      FieldId("x-postcode") -> Seq("C")
    )

    val result: ValidatedType = new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever")).validate(speccedAddress).futureValue

    result.toEither should beRight(())
  }

  "Address validation" should "pass with valid required data omitting the optional" in {
    val address = Address(international = true)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, None, true, true, false, true, None)

    val data = Map(
      FieldId("x-uk") -> Seq("true"),
      FieldId("x-street1") -> Seq(List.fill(ValidationValues.addressLine)("a").mkString),
      FieldId("x-postcode") -> Seq("C")
    )

    val result: ValidatedType = new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever")).validate(speccedAddress).futureValue

    result.toEither should beRight(())
  }

  "Address validation" should "fail when the field is fails validation" in {
    val address = Address(international = true)

    val speccedAddress = FieldValue(FieldId("x"), address, "l", None, None, true, true, false, true, None)

    val data = Map(
      FieldId("x-uk") -> Seq("true"),
      FieldId("x-street1") -> Seq(List.fill(36)("a").mkString),
      FieldId("x-street2") -> Seq(List.fill(36)("a").mkString),
      FieldId("x-street3") -> Seq(List.fill(36)("a").mkString),
      FieldId("x-street4") -> Seq(List.fill(28)("a").mkString),
      FieldId("x-postcode") -> Seq("C")
    )

    val result: ValidatedType = new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever")).validate(speccedAddress).futureValue

    result.toEither should beLeft(
      Map(
        FieldId("x-street2") -> Set(s"this field is too long must be at most 35"),
        FieldId("x-street1") -> Set(s"this field is too long must be at most 35"),
        FieldId("x-street3") -> Set(s"this field is too long must be at most 35"),
        FieldId("x-street4") -> Set(s"this field is too long must be at most 27")
      )
    )
  }
}
