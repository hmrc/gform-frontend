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

package uk.gov.hmrc.gform.validation

import cats.instances.future._
import org.scalatest.mockito.MockitoSugar.mock
import play.api.i18n.Messages
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.{ GraphSpec, Spec }
import uk.gov.hmrc.http.HeaderCarrier

class FormatValidationSpec(implicit messages: Messages, l: LangADT) extends Spec with GraphSpec {

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
      FormComponent(
        FormComponentId("n"),
        text,
        toLocalisedString("sample label"),
        None,
        None,
        None,
        true,
        false,
        false,
        true,
        false,
        None)
    val fieldValues = List(fieldValue)

    val data = Map(
      FormComponentId("n-1") -> Seq("12"),
      FormComponentId("n-2") -> Seq("12"),
      FormComponentId("n-3") -> Seq("12")
    )

    val result = validator(fieldValue, fieldValues, data)

    result.toEither should beRight(())
  }

  "UkSortCode" should "be invalid with 3 digits in one box" in {
    val text = UkSortCode(Value)

    val fieldValue =
      FormComponent(
        FormComponentId("n"),
        text,
        toLocalisedString("sample label"),
        None,
        None,
        None,
        true,
        false,
        false,
        true,
        false,
        None)

    val fieldValues = List(fieldValue)

    val data = Map(
      FormComponentId("n-1") -> Seq("12"),
      FormComponentId("n-2") -> Seq("123"),
      FormComponentId("n-3") -> Seq("12")
    )

    val result = validator(fieldValue, fieldValues, data)

    result.toEither should beLeft(Map(fieldValue.id -> Set("sample label must be 2 numbers")))
  }

  "UkSortCode" should "return an error message" in {
    val text = UkSortCode(Value)

    val fieldValue =
      FormComponent(
        FormComponentId("n"),
        text,
        toLocalisedString("sample label"),
        None,
        None,
        None,
        true,
        false,
        false,
        true,
        false,
        None)

    val fieldValues = List(fieldValue)

    val data = Map(
      FormComponentId("n-1") -> Seq(""),
      FormComponentId("n-2") -> Seq(""),
      FormComponentId("n-3") -> Seq("")
    )

    val result = validator(fieldValue, fieldValues, data)

    result.toEither should beLeft(Map(fieldValue.id -> Set("sample label values must be two digit numbers")))
  }

  "UkSortCode" should "return invalid data on -" in {
    val text = UkSortCode(Value)

    val fieldValue =
      FormComponent(
        FormComponentId("n"),
        text,
        toLocalisedString("sample label"),
        None,
        None,
        None,
        true,
        false,
        false,
        true,
        false,
        None)

    val fieldValues = List(fieldValue)

    val data = Map(
      FormComponentId("n-1") -> Seq("-1"),
      FormComponentId("n-2") -> Seq("24"),
      FormComponentId("n-3") -> Seq("24")
    )

    val result = validator(fieldValue, fieldValues, data)

    result.toEither should beLeft(Map(fieldValue.id -> Set("sample label must be 2 numbers")))
  }

  "UkSortCode" should "be invalid with decimals" in {
    val text = UkSortCode(Value)

    val fieldValue =
      FormComponent(
        FormComponentId("n"),
        text,
        toLocalisedString("sample label"),
        None,
        None,
        None,
        true,
        false,
        false,
        true,
        false,
        None)

    val fieldValues = List(fieldValue)

    val data = Map(
      FormComponentId("n-1") -> Seq("1.2"),
      FormComponentId("n-2") -> Seq("1.3"),
      FormComponentId("n-3") -> Seq("1.2")
    )

    val result = validator(fieldValue, fieldValues, data)

    result.toEither should beLeft(Map(fieldValue.id -> Set("sample label must be a whole number")))
  }

  "TelephoneNumber" should "be valid within limit of 25" in createSuccessTest(
    "1234567890123456789012345",
    TelephoneNumber)
  "TelephoneNumber" should "be valid with special characters" in createSuccessTest("+44 1234 567890", TelephoneNumber)
  "TelephoneNumber" should "be invalid with over the limit" in createFailTest(
    "1234567891011121314151617181920",
    TelephoneNumber,
    "sample label has more than 25 characters")

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
  "UkVrn" should "return invalid if too short" in createFailTest(
    "GB123",
    UkVrn,
    "sample label has fewer than 7 characters")
  "UkVrn" should "return invalid if too long" in createFailTest(
    "GB1234567887548876554",
    UkVrn,
    "sample label has more than 14 characters")

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
    "sample label has fewer than 8 characters")
  "CompanyRegistrationNumber" should "return invalid if too long" in createFailTest(
    "SO1234567890",
    CompanyRegistrationNumber,
    "sample label has more than 8 characters")
  "CompanyRegistrationNumber" should "return invalid if too many letters at the start" in createFailTest(
    "BNR12345",
    CompanyRegistrationNumber,
    "sample label is not a valid Company Registration Number")

  "EORI" should "return valid EORI with 7 digits" in createSuccessTest("FR1234567", EORI)
  "EORI" should "return valid EORI with 7 digits/letters" in createSuccessTest("FR1234ABC", EORI)
  "EORI" should "return valid EORI with 7 letters" in createSuccessTest("FRABCDEFG", EORI)
  "EORI" should "return valid EORI with 8 digits" in createSuccessTest("FR12345678", EORI)
  "EORI" should "return valid EORI with 8 digits/letters" in createSuccessTest("FR12345DKF", EORI)
  "EORI" should "return valid EORI with 8 letters" in createSuccessTest("FRABCDEFGH", EORI)
  "EORI" should "return valid EORI with 9 digits" in createSuccessTest("FR123456789", EORI)
  "EORI" should "return valid EORI with 9 digits/letters" in createSuccessTest("FR12345DAFE", EORI)
  "EORI" should "return valid EORI with 9 letters" in createSuccessTest("FRABCDEFGHI", EORI)
  "EORI" should "return valid EORI with 10 digits" in createSuccessTest("FR1234567891", EORI)
  "EORI" should "return valid EORI with 10 digits/letters" in createSuccessTest("FR1234567FEA", EORI)
  "EORI" should "return valid EORI with 10 letters" in createSuccessTest("FRABCDEFGHIJ", EORI)
  "EORI" should "return valid EORI with 11 digits" in createSuccessTest("FR12345678911", EORI)
  "EORI" should "return valid EORI with 11 digits/letters" in createSuccessTest("FRABCDEFGH513", EORI)
  "EORI" should "return valid EORI with 11 letters" in createSuccessTest("FRABCDEFGHIJK", EORI)
  "EORI" should "return valid EORI with 12 digits" in createSuccessTest("GB123456789123", EORI)
  "EORI" should "return valid EORI with 12 digits/letters" in createSuccessTest("GBABCDEFGH6134", EORI)
  "EORI" should "return valid EORI with 12 letters" in createSuccessTest("GBABCDEFGHIJKL", EORI)
  "EORI" should "return valid EORI with 13 digits" in createSuccessTest("GB1234567891233", EORI)
  "EORI" should "return valid EORI with 13 digits/letters" in createSuccessTest("GB123456789GEAF", EORI)
  "EORI" should "return valid EORI with 13 letters" in createSuccessTest("GBABCDEFGHIJKLM", EORI)
  "EORI" should "return valid EORI with 14 digits" in createSuccessTest("GB12345678912333", EORI)
  "EORI" should "return valid EORI with 14 digits/letters" in createSuccessTest("GB12345678912FAW", EORI)
  "EORI" should "return valid EORI with 14 letters" in createSuccessTest("GBABCDEFGHIJKLMN", EORI)
  "EORI" should "return valid EORI with 15 digits" in createSuccessTest("FR123456789123456", EORI)
  "EORI" should "return valid EORI with 15 digits/letters" in createSuccessTest("FR12345678912FYHB", EORI)
  "EORI" should "return valid EORI with 15 letters" in createSuccessTest("FRABCDEFGHIJKLMNO", EORI)
  "EORI" should "return invalid without one of the previous conditions" in createFailTest(
    "K8765432",
    EORI,
    "sample label has fewer than 9 characters")
  "EORI" should "return invalid as too many characters in data" in createFailTest(
    "XB1234567891123456",
    EORI,
    "sample label has more than 17 characters")
  "EORI" should "return invalid as too few letters in data" in createFailTest(
    "GB123456",
    EORI,
    "sample label has fewer than 9 characters")
  "EORI" should "return invalid as starts with number" in createFailTest(
    "1XFR1234567893456",
    EORI,
    "sample label is not a valid EORI")
  "EORI" should "return invalid as too few letters at start" in createFailTest(
    "F12345678",
    EORI,
    "sample label is not a valid EORI")
  "EORI" should "return invalid if contains special characters" in createFailTest(
    "GB123456789*2",
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
    "sample label has fewer than 2 characters")
  "NonUkCountryCode" should "return invalid if it's too long" in createFailTest(
    "USA",
    NonUkCountryCode,
    "sample label has more than 2 characters")
  "Country Code" should "return valid if it's any valid country code" in createSuccessTest("UK", CountryCode)
  "Country Code" should "return invalid if it's too long" in createFailTest(
    "UTT",
    CountryCode,
    "sample label has more than 2 characters")
  "Country Code" should "return invalid if it's too short" in createFailTest(
    "U",
    CountryCode,
    "sample label has fewer than 2 characters")
  "BasicText" should "return valid with text" in createSuccessTest("This is test text", BasicText)
  "BasicText" should "return invalid with too many characters" in createFailTest(
    List.fill[String](100001)("a").mkString,
    BasicText,
    "sample label has more than 100000 characters")
  "BasicText" should "return invalid withe invalid characters" in createFailTest(
    List.fill[String](5)("%").mkString,
    BasicText,
    "sample label can only include letters, numbers, spaces and round, square, angled or curly brackets, apostrophes, hyphens, dashes, periods, pound signs, plus signs, semi-colons, colons, asterisks, question marks, equal signs, forward slashes, ampersands, exclamation marks, @ signs, hash signs, dollar signs, euro signs, back ticks, tildes, double quotes and underscores"
  )
  "ShortText" should "return valid with shortText" in createSuccessTest("this is test text", ShortText.default)
  "ShortText" should "return invalid with too long of text" in createFailTest(
    List.fill(1001)("a").mkString,
    ShortText.default,
    "sample label has more than 1000 characters"
  )
  "ShortText(min, max)" should "return valid with in ShortText" in createSuccessTest(
    "this is in constraints",
    ShortText(1, 100))
  "Text(min, max)" should "return valid with in constraints text" in createSuccessTest(
    "this is in constraints",
    TextWithRestrictions(1, 100))
  "Text(min, max)" should "return invalid with too long of text" in createFailTest(
    List.fill(101)("a").mkString,
    TextWithRestrictions(1, 100),
    "sample label has more than 100 characters")
  private def createSuccessTest(data: String, contraint: TextConstraint) =
    validator(fieldValueFunction(contraint), getFormComponentList(contraint), getData(data)).toEither should beRight(())

  private def createFailTest(data: String, constrait: TextConstraint, errorMessage: String) =
    validator(fieldValueFunction(constrait), getFormComponentList(constrait), getData(data)).toEither should beLeft(
      Map(default -> Set(errorMessage)))

  private val getData: String => Map[FormComponentId, Seq[String]] = str => Map(default -> Seq(str))
  private val getFormComponentList: TextConstraint => List[FormComponent] = contraint =>
    List(fieldValue(Text(contraint, Value)))
  val retrievals: MaterialisedRetrievals = mock[MaterialisedRetrievals]
  implicit lazy val hc = HeaderCarrier()

  private val lookupRegistry = new LookupRegistry(Map.empty)

  private def validator(
    fieldValue: FormComponent,
    fieldValues: List[FormComponent],
    data: Map[FormComponentId, Seq[String]]) =
    new ComponentsValidator(
      mkFormDataRecalculated(data),
      mock[FileUploadService],
      EnvelopeId("whatever"),
      retrievals,
      booleanExprEval,
      ThirdPartyData.empty,
      ExampleData.formTemplate,
      lookupRegistry
    ).validate(fieldValue, fieldValues).futureValue

  private val fieldValueFunction: TextConstraint => FormComponent = contraint => fieldValue(Text(contraint, Value))

}
