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
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar.mock
import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ComponentsValidator
import uk.gov.hmrc.play.http.HeaderCarrier

class NumberValidationSpec extends FlatSpec with Matchers with EitherMatchers with ScalaFutures {

  implicit lazy val hc = HeaderCarrier()

  "Number format" should "accepts whole numbers" in {
    val textConstraint = Number()
    val number = Text(textConstraint, Constant(""))

    val fieldValue = FieldValue(FieldId("n"), number,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("123")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beRight(())
  }

  "Number format" should "return invalid for non-numeric" in {
    val textConstraint = Number()
    val number = Text(textConstraint, Constant(""))

    val fieldValue = FieldValue(FieldId("n"), number,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("THX1138")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("must be a number")))
  }

  "Number format" should "accepts decimal fractions" in {
    val textConstraint = Number()
    val number = Text(textConstraint, Constant(""))

    val fieldValue = FieldValue(FieldId("n"), number,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("123.4")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beRight(())
  }

  "PositiveWholeNumber format" should "return invalid for non-numeric" in {
    val textConstraint = PositiveNumber(maxFractionalDigits = 0)
    val number = Text(textConstraint, Constant(""))

    val fieldValue = FieldValue(FieldId("n"), number,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("123.4")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("must be a whole number")))
  }

  "PositiveNumber format" should "accepts whole numbers" in {
    val textConstraint = PositiveNumber()
    val number = Text(textConstraint, Constant(""))

    val fieldValue = FieldValue(FieldId("n"), number,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("123")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beRight(())
  }

  "Number format" should "accepts negative numbers" in {
    val textConstraint = Number()
    val number = Text(textConstraint, Constant(""))

    val fieldValue = FieldValue(FieldId("n"), number,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("-789")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beRight(())
  }

  "PositiveNumber format" should "return invalid for negative" in {
    val textConstraint = PositiveNumber()
    val number = Text(textConstraint, Constant(""))

    val fieldValue = FieldValue(FieldId("n"), number,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("-789")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("must be a positive number")))
  }

  "Number format" should "return invalid for too many digits" in {
    val textConstraint = Number()
    val number = Text(textConstraint, Constant(""))

    val fieldValue = FieldValue(FieldId("n"), number,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("1234567890123456789.87654321")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("number must be at most 11 whole digits and decimal fraction must be at most 2 digits")))
  }

  "Number format" should "return invalid for too many whole digits" in {
    val textConstraint = Number()
    val number = Text(textConstraint, Constant(""))

    val fieldValue = FieldValue(FieldId("n"), number,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("1234567890123456789.87")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("number must be at most 11 whole digits")))
  }

  "Number(maxFractionalDigits = 0) format" should "return invalid for too many whole digits" in {
    val textConstraint = Number(maxFractionalDigits = 0)
    val number = Text(textConstraint, Constant(""))

    val fieldValue = FieldValue(FieldId("n"), number,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("1234567890123456789")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("must be at most 11 digits")))
  }

  "Number format" should "return invalid for too many fractional digits" in {
    val textConstraint = Number()
    val number = Text(textConstraint, Constant(""))

    val fieldValue = FieldValue(FieldId("n"), number,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("9.87654321")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("decimal fraction must be at most 2 digits")))
  }

  "Number(2,1) format" should "return invalid for too many digits" in {
    val textConstraint = Number(2, 1)
    val number = Text(textConstraint, Constant(""))

    val fieldValue = FieldValue(FieldId("n"), number,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("123.21")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("number must be at most 2 whole digits and decimal fraction must be at most 1 digits")))
  }

  "Number(2,1) format" should "return supplied error message" in {
    val textConstraint = Number(2, 1)
    val number = Text(textConstraint, Constant(""))

    val fieldValue = FieldValue(FieldId("n"), number,
      "sample label", None, None, true, false, false, Some("New error message"))

    val data = Map(
      FieldId("n") -> Seq("123.21")
    )

    val result = new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("New error message")))
  }
}