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

import org.scalatest.mockito.MockitoSugar.mock
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ComponentsValidator
import uk.gov.hmrc.play.http.HeaderCarrier

class FormatValidationSpec extends Spec {

  implicit lazy val hc = HeaderCarrier()

  def validator(fieldValue: FieldValue, data: Map[FieldId, Seq[String]]) = {
    new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever"))
  }

  "Sterling Format" should "Valid with whole number below 11 digits" in {
    val textConstrait = Sterling
    val text = Text(textConstrait, Constant(""), false)

    val fieldValue = FieldValue(FieldId("n"), text,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("12345678910")
    )

    val result = validator(fieldValue, data).validate().futureValue

    result.toEither should beRight(())
  }

  "Sterling Format" should "" in {
    val textConstrait = Sterling
    val text = Text(textConstrait, Constant(""), false)

    val fieldValue = FieldValue(FieldId("n"), text,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("1234567891011")
    )

    val result = validator(fieldValue, data).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("must be at most 11 digits")))
  }

  "UkBankAccountNumber Format" should "be valid with 8 digits" in {
    val textConstrait = UkBankAccountNumber
    val text = Text(textConstrait, Constant(""), false)

    val fieldValue = FieldValue(FieldId("n"), text,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("12345678")
    )

    val result = validator(fieldValue, data).validate().futureValue

    result.toEither should beRight(())
  }

  "UkBankAccountNumber Format" should "be invalid with 9" in {
    val textConstrait = UkBankAccountNumber
    val text = Text(textConstrait, Constant(""), false)

    val fieldValue = FieldValue(FieldId("n"), text,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("123456789")
    )

    val result = validator(fieldValue, data).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("must be a whole number of 8 length")))
  }

  "UkBankAccountNumber Format" should "be invalid with decimals" in {
    val textConstrait = UkBankAccountNumber
    val text = Text(textConstrait, Constant(""), false)

    val fieldValue = FieldValue(FieldId("n"), text,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n") -> Seq("123456789.12345678")
    )

    val result = validator(fieldValue, data).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("must be a whole number")))
  }

  "UkSortCode" should "be valid with 2 digits in each box" in {
    val textConstrait = UkSortCode
    val text = Text(textConstrait, Constant(""), false)

    val fieldValue = FieldValue(FieldId("n"), text,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n-1") -> Seq("12"),
      FieldId("n-2") -> Seq("12"),
      FieldId("n-3") -> Seq("12")
    )

    val result = validator(fieldValue, data).validate().futureValue

    result.toEither should beRight(())
  }

  "UkSortCode" should "be invalid with 3 digits in one box" in {
    val textConstrait = UkSortCode
    val text = Text(textConstrait, Constant(""), false)

    val fieldValue = FieldValue(FieldId("n"), text,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n-1") -> Seq("12"),
      FieldId("n-2") -> Seq("123"),
      FieldId("n-3") -> Seq("12")
    )

    val result = validator(fieldValue, data).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("must be a whole number of 2 length")))
  }

  "UkSortCode" should "be invalid with decimals" in {
    val textConstrait = UkSortCode
    val text = Text(textConstrait, Constant(""), false)

    val fieldValue = FieldValue(FieldId("n"), text,
      "sample label", None, None, true, false, false, None)

    val data = Map(
      FieldId("n-1") -> Seq("1.2"),
      FieldId("n-2") -> Seq("1.3"),
      FieldId("n-3") -> Seq("1.2")
    )

    val result = validator(fieldValue, data).validate().futureValue

    result.toEither should beLeft(Map(fieldValue.id -> Set("must be a whole number")))
  }

}
