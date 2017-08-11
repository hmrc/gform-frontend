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
import uk.gov.hmrc.gform.sharedmodel.ExampleData._

class FormatValidationSpec extends Spec {

  "Sterling Format" should "Valid with whole number below 11 digits" in createSuccessTest("12345678910", Sterling)
  "Sterling Format" should "" in createFailTest("1234567891011", Sterling, "must be at most 11 digits")
  "UkBankAccountNumber Format" should "be valid with 8 digits" in createSuccessTest("12345678", UkBankAccountNumber)

  "UkBankAccountNumber Format" should "be invalid with 9" in createFailTest("123456789", UkBankAccountNumber, "must be a whole number of 8 length")
  "UkBankAccountNumber Format" should "be invalid with decimals" in createFailTest("123456789.12345678", UkBankAccountNumber, "must be a whole number")

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

    val result = validator(fieldValue, data)

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

    val result = validator(fieldValue, data)

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

    val result = validator(fieldValue, data)

    result.toEither should beLeft(Map(fieldValue.id -> Set("must be a whole number")))
  }

  "TelephoneNumber" should "be valid within limit of 30" in createSuccessTest("123456789101112131415161718192", TelephoneNumber)
  "TelephoneNumber" should "be valid with special characters" in createSuccessTest("+44 1234 567890", TelephoneNumber)
  "TelephoneNumber" should "be invalid with over the limit" in createFailTest("1234567891011121314151617181920", TelephoneNumber, "Entered too many characters")
  "Email" should "be valid with proper structure" in createSuccessTest("test@test.com", Email)
  "Email" should "be invalid with anvalid email address" in createFailTest("testtest.com", Email, "This email address is not valid")
  "UTR" should "be valid " in createSuccessTest("1000000000", UTR)
  "UTR" should "be invalid with decimals" in createFailTest("123456789", UTR, "Not a valid Id")
  "NINO" should "be valid with a valid NINO " in createSuccessTest("AA111111A", NINO)
  "NINO" should "be return Invalid with an incorrect NINO" in createFailTest("AA111111", NINO, "Not a valid Id")
  "BasicText" should "return valid with text" in createSuccessTest("This is test text", BasicText)
  "BasicText" should "return invalid with invalid text" in createFailTest(List.fill[String](100001)("a").mkString, BasicText, "The text is over 100000 so is not valid")
  "ShortText" should "return valid with shortText" in createSuccessTest("this is test text", ShortText)
  "ShortText" should "return invalid with too long of text" in createFailTest(List.fill(1001)("a").mkString, ShortText, "the text is too long for the validation")
  "Text(min, max)" should "return valid with in constraints text" in createSuccessTest("this is in constraints", TextWithRestrictions(1, 100))
  "Text(min, max)" should "return invalid with too long of text" in createFailTest(List.fill(101)("a").mkString, TextWithRestrictions(1, 100), "Entered too many characters")
  private def createSuccessTest(data: String, contraint: TextConstraint) =
    validator(fieldValueFunction(contraint), getData(data)).toEither should beRight(())

  private def createFailTest(data: String, constrait: TextConstraint, errorMessage: String) =
    validator(fieldValueFunction(constrait), getData(data)).toEither should beLeft(Map(default -> Set(errorMessage)))

  private val getData: String => Map[FieldId, Seq[String]] = str => Map(default -> Seq(str))

  implicit lazy val hc = HeaderCarrier()

  private def validator(fieldValue: FieldValue, data: Map[FieldId, Seq[String]]) = {
    new ComponentsValidator(fieldValue, data, mock[FileUploadService], EnvelopeId("whatever")).validate().futureValue
  }

  private val fieldValueFunction: TextConstraint => FieldValue = contraint => fieldValue(Text(contraint, Constant(""), false))

}
