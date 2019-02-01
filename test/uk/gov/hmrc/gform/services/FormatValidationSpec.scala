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

import cats.instances.future._
import org.scalatest.mockito.MockitoSugar.mock
import uk.gov.hmrc.gform.{ GraphSpec, Spec }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ComponentsValidator
import uk.gov.hmrc.http.HeaderCarrier

class FormatValidationSpec extends Spec with GraphSpec {

  "Sterling Format" should "Valid with whole number below 11 digits" in createSuccessTest(
    "12345678910",
    Sterling.defaultRounding)
  "Sterling Format" should "" in createFailTest(
    "1234567891011",
    Sterling.defaultRounding,
    "sample label must be at most 11 digits")
  "UkBankAccountNumber Format" should "be valid with 8 digits" in createSuccessTest("12345678", UkBankAccountNumber)

  "UkBankAccountNumber Format" should "be invalid with 9" in createFailTest(
    "123456789",
    UkBankAccountNumber,
    "sample label must be 8 numbers")
  "UkBankAccountNumber Format" should "be invalid with decimals" in createFailTest(
    "123456789.12345678",
    UkBankAccountNumber,
    "sample label must be a whole number")

  "UkSortCode" should "be valid with 2 digits in each box" in {
    val text = UkSortCode(Value)

    val fieldValue =
      FormComponent(FormComponentId("n"), text, "sample label", None, None, None, true, false, false, true, false, None)

    val data = Map(
      FormComponentId("n-1") -> Seq("12"),
      FormComponentId("n-2") -> Seq("12"),
      FormComponentId("n-3") -> Seq("12")
    )

    val result = validator(fieldValue, data)

    result.toEither should beRight(())
  }

  "UkSortCode" should "be invalid with 3 digits in one box" in {
    val text = UkSortCode(Value)

    val fieldValue =
      FormComponent(FormComponentId("n"), text, "sample label", None, None, None, true, false, false, true, false, None)

    val data = Map(
      FormComponentId("n-1") -> Seq("12"),
      FormComponentId("n-2") -> Seq("123"),
      FormComponentId("n-3") -> Seq("12")
    )

    val result = validator(fieldValue, data)

    result.toEither should beLeft(Map(fieldValue.id -> Set("sample label must be 2 numbers")))
  }

  "UkSortCode" should "return an error message" in {
    val text = UkSortCode(Value)

    val fieldValue =
      FormComponent(FormComponentId("n"), text, "sample label", None, None, None, true, false, false, true, false, None)

    val data = Map(
      FormComponentId("n-1") -> Seq(""),
      FormComponentId("n-2") -> Seq(""),
      FormComponentId("n-3") -> Seq("")
    )

    val result = validator(fieldValue, data)

    result.toEither should beLeft(Map(fieldValue.id -> Set("sample label values must be two digit numbers")))
  }

  "UkSortCode" should "return invalid data on -" in {
    val text = UkSortCode(Value)

    val fieldValue =
      FormComponent(FormComponentId("n"), text, "sample label", None, None, None, true, false, false, true, false, None)

    val data = Map(
      FormComponentId("n-1") -> Seq("-1"),
      FormComponentId("n-2") -> Seq("24"),
      FormComponentId("n-3") -> Seq("24")
    )

    val result = validator(fieldValue, data)

    result.toEither should beLeft(Map(fieldValue.id -> Set("sample label must be 2 numbers")))
  }

  "UkSortCode" should "be invalid with decimals" in {
    val text = UkSortCode(Value)

    val fieldValue =
      FormComponent(FormComponentId("n"), text, "sample label", None, None, None, true, false, false, true, false, None)

    val data = Map(
      FormComponentId("n-1") -> Seq("1.2"),
      FormComponentId("n-2") -> Seq("1.3"),
      FormComponentId("n-3") -> Seq("1.2")
    )

    val result = validator(fieldValue, data)

    result.toEither should beLeft(Map(fieldValue.id -> Set("sample label must be a whole number")))
  }

  "TelephoneNumber" should "be valid within limit of 30" in createSuccessTest(
    "123456789101112131415161718192",
    TelephoneNumber)
  "TelephoneNumber" should "be valid with special characters" in createSuccessTest("+44 1234 567890", TelephoneNumber)
  "TelephoneNumber" should "be invalid with over the limit" in createFailTest(
    "1234567891011121314151617181920",
    TelephoneNumber,
    "sample label has more than 30 characters")
  "Email" should "be valid with proper structure" in createSuccessTest("test@test.com", Email)
  "Email" should "be invalid with invalid email address" in createFailTest(
    "testtest.com",
    Email,
    "sample label is not valid")
  "Email" should "be invalid with too long email address" in createFailTest(
    List.fill(241)("a").mkString + "@test.com",
    Email,
    "sample label has more than 241 characters")
  "UTR" should "be valid " in createSuccessTest("1000000000", UTR)
  "UTR" should "be invalid with decimals" in createFailTest("123456789", UTR, "sample label is not a valid Id")
  "NINO" should "be valid with a valid NINO " in createSuccessTest("AA111111A", NINO)
  "NINO" should "be return Invalid with an incorrect NINO" in createFailTest(
    "AA111111",
    NINO,
    "sample label is not a valid Id")
  "UkVrn" should "return valid standard" in createSuccessTest("GB999999973", UkVrn)
  "UkVrn" should "return valid branch" in createSuccessTest("GB999999973001", UkVrn)
  "UkVrn" should "return valid gpvernment" in createSuccessTest("GBGD001", UkVrn)
  "UkVrn" should "return valid health" in createSuccessTest("GBHA599", UkVrn)
  "UkVrn" should "return invalid without the GB precedding" in createFailTest(
    "ABCD111111111",
    UkVrn,
    "sample label is not a valid VRN")
  "UkVrn" should "return invalid if too short" in createFailTest("GB123", UkVrn, "sample label is not a valid VRN")

  "CompanyRegistrationNumber" should "return valid Company Registration Number with 8 digits" in createSuccessTest(
    "12345678",
    CompanyRegistrationNumber)
  "CompanyRegistrationNumber" should "return valid Company Registration Number with 2 letters followed by 6 digits" in createSuccessTest(
    "SC123456",
    CompanyRegistrationNumber)
  "CompanyRegistrationNumber" should "return invalid without one of the previous conditions" in createFailTest(
    "K8765432",
    CompanyRegistrationNumber,
    "sample label is not a valid Company Registration Number")
  "CompanyRegistrationNumber" should "return invalid if too short" in createFailTest(
    "SO1234",
    CompanyRegistrationNumber,
    "sample label is not a valid Company Registration Number")
  "CompanyRegistrationNumber" should "return invalid if too long" in createFailTest(
    "SO1234567890",
    CompanyRegistrationNumber,
    "sample label is not a valid Company Registration Number")
  "CompanyRegistrationNumber" should "return invalid if too many letters at the start" in createFailTest(
    "BNR12345",
    CompanyRegistrationNumber,
    "sample label is not a valid Company Registration Number")

  "EORI" should "return valid EORI with 12 digits" in createSuccessTest("GB123456789123", EORI)
  "EORI" should "return valid EORI with 15 digits" in createSuccessTest("FR123456789123456", EORI)
  "EORI" should "return invalid without one of the previous conditions" in createFailTest(
    "K8765432",
    EORI,
    "sample label is not a valid EORI")
  "EORI" should "return invalid as too many letters in data - 12 num" in createFailTest(
    "XGB123456789123",
    EORI,
    "sample label is not a valid EORI")
  "EORI" should "return invalid as too few letters in data - 12 num" in createFailTest(
    "3GB123456789123",
    EORI,
    "sample label is not a valid EORI")
  "EORI" should "return invalid as too many letters in data - 15 num" in createFailTest(
    "XFR123456789123456",
    EORI,
    "sample label is not a valid EORI")
  "EORI" should "return invalid as too few letters in data - 15 num" in createFailTest(
    "3FR123456789123456",
    EORI,
    "sample label is not a valid EORI")
  "EORI" should "return invalid if too short" in createFailTest(
    "GB12345678912",
    EORI,
    "sample label is not a valid EORI")
  "EORI" should "return invalid if too long - 12 num" in createFailTest(
    "GB1234567891233",
    EORI,
    "sample label is not a valid EORI")
  "EORI" should "return invalid if too long - 15 num" in createFailTest(
    "FR1234567891234563",
    EORI,
    "sample label is not a valid EORI")

  "NonUkCountryCode" should "return valid" in createSuccessTest("US", NonUkCountryCode)
  "NonUkCountryCode" should "return invalid if code is UK" in createFailTest(
    "UK",
    NonUkCountryCode,
    "sample label is not a valid non UK country code")
  "NonUkCountryCode" should "return invalid if it's too short" in createFailTest(
    "U",
    NonUkCountryCode,
    "sample label is not a valid non UK country code")
  "NonUkCountryCode" should "return invalid if it's too long" in createFailTest(
    "USA",
    NonUkCountryCode,
    "sample label is not a valid non UK country code")
  "Country Code" should "return valid if it's any valid country code" in createSuccessTest("UK", CountryCode)
  "Country Code" should "return invalid if it's too long" in createFailTest(
    "UTT",
    CountryCode,
    "sample label is not a valid country code")
  "Country Code" should "return invalid if it's too short" in createFailTest(
    "U",
    CountryCode,
    "sample label is not a valid country code")
  "BasicText" should "return valid with text" in createSuccessTest("This is test text", BasicText)
  "BasicText" should "return invalid with invalid text" in createFailTest(
    List.fill[String](100001)("a").mkString,
    BasicText,
    "sample label can only include letters, numbers, spaces and round, square, angled or curly brackets, apostrophes, hyphens, dashes, periods, pound signs, plus signs, semi-colons, colons, asterisks, question marks, equal signs, forward slashes, ampersands, exclamation marks, @ signs, hash signs, dollar signs, euro signs, back ticks, tildes, double quotes and underscores"
  )
  "ShortText" should "return valid with shortText" in createSuccessTest("this is test text", ShortText)
  "ShortText" should "return invalid with too long of text" in createFailTest(
    List.fill(1001)("a").mkString,
    ShortText,
    "sample label can only include letters, numbers, spaces, hyphens, ampersands and apostrophes"
  )
  "Text(min, max)" should "return valid with in constraints text" in createSuccessTest(
    "this is in constraints",
    TextWithRestrictions(1, 100))
  "Text(min, max)" should "return invalid with too long of text" in createFailTest(
    List.fill(101)("a").mkString,
    TextWithRestrictions(1, 100),
    "sample label has more than 100 characters")
  private def createSuccessTest(data: String, contraint: TextConstraint) =
    validator(fieldValueFunction(contraint), getData(data)).toEither should beRight(())

  private def createFailTest(data: String, constrait: TextConstraint, errorMessage: String) =
    validator(fieldValueFunction(constrait), getData(data)).toEither should beLeft(Map(default -> Set(errorMessage)))

  private val getData: String => Map[FormComponentId, Seq[String]] = str => Map(default -> Seq(str))
  val retrievals: MaterialisedRetrievals = mock[MaterialisedRetrievals]
  implicit lazy val hc = HeaderCarrier()

  private def validator(fieldValue: FormComponent, data: Map[FormComponentId, Seq[String]]) =
    new ComponentsValidator(
      mkFormDataRecalculated(data),
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ExampleData.formTemplate)
      .validate(fieldValue)
      .futureValue

  private val fieldValueFunction: TextConstraint => FormComponent = contraint => fieldValue(Text(contraint, Value))

}
