/*
 * Copyright 2018 HM Revenue & Customs
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

import cats.data.Validated
import cats.data.Validated.Valid
import cats.scalatest.EitherMatchers
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar.mock
import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ComponentsValidator
import cats.implicits._
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.http.HeaderCarrier

class NumberValidationSpec extends Spec {
  val retrievals: Retrievals = mock[Retrievals]
  "Number format" should "accepts whole numbers" in new Test {
    override val value = "123"
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe ().valid
  }

  "Number format" should "accept stirling pound and commas within numbers" in new Test {
    override val value = "£1,234"
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe ().valid
  }

  "Number format" should "return invalid for non-numeric" in new Test {
    override val value = "THX1138"
    val expected = Map(`fieldValue - number`.id -> Set("must be a number")).invalid[Unit]
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe expected withClue "we don't support alphabetics in number formats"
  }

  "Number format" should "accepts decimal fractions" in new Test {
    override val value = "123.4"
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe ().valid
  }

  "PositiveWholeNumber format" should "return invalid for decimal fractions" in new Test {
    override val value = "123.4"
    val textConstraint = PositiveNumber(maxFractionalDigits = 0)
    val number = Text(textConstraint, Constant(""))
    override def `fieldValue - number` = super.`fieldValue - number`.copy(`type` = number)
    val expected = Map(`fieldValue - number`.id -> Set("must be a whole number")).invalid[Unit]
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe expected withClue "we don't support dots in number formats"
  }

  "PositiveNumber format" should "accept whole numbers" in new Test {
    override val value = "123"
    val textConstraint = PositiveNumber()
    val number = Text(textConstraint, Constant(""))
    override def `fieldValue - number` = super.`fieldValue - number`.copy(`type` = number)
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe ().valid
  }

  "Number format" should "accept negative numbers" in new Test {
    override val value = "-789"
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe ().valid
  }

  "PositiveNumber format" should "return invalid for negative" in new Test {
    override val value = "-123"
    val textConstraint = PositiveNumber()
    val number = Text(textConstraint, Constant(""))
    override def `fieldValue - number` = super.`fieldValue - number`.copy(`type` = number)
    val expected = Map(`fieldValue - number`.id -> Set("must be a positive number")).invalid[Unit]
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe expected withClue "we don't support negative numbers in postive number formats"
  }

  "Number format" should "return invalid for too many digits" in new Test {
    override val value = "1234567890123456789.87654321"
    val expected = Map(`fieldValue - number`.id -> Set("number must be at most 11 whole digits and decimal fraction must be at most 2 digits")).invalid[Unit]
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe expected
  }

  "Number format" should "return invalid for too many whole digits" in new Test {
    override val value = "1234567890123456789.87"
    val expected = Map(`fieldValue - number`.id -> Set("number must be at most 11 whole digits")).invalid[Unit]
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe expected
  }

  "Number(maxFractionalDigits = 0) format" should "return invalid for too many whole digits" in new Test {
    override val value = "1234567890123456789"
    val textConstraint = Number(maxFractionalDigits = 0)
    val number = Text(textConstraint, Constant(""))
    override def `fieldValue - number` = super.`fieldValue - number`.copy(`type` = number)
    val expected = Map(`fieldValue - number`.id -> Set("must be at most 11 digits")).invalid[Unit]
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe expected
  }

  "Number format" should "return invalid for too many fractional digits" in new Test {
    override val value = "9.87654321"
    val expected = Map(`fieldValue - number`.id -> Set("decimal fraction must be at most 2 digits")).invalid[Unit]
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe expected
  }

  "Number(2,1) format" should "return invalid for too many digits" in new Test {
    override val value = "123.21"
    val textConstraint = Number(2, 1)
    val number = Text(textConstraint, Constant(""))
    override def `fieldValue - number` = super.`fieldValue - number`.copy(`type` = number)
    val expected = Map(`fieldValue - number`.id -> Set("number must be at most 2 whole digits and decimal fraction must be at most 1 digits")).invalid[Unit]
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe expected
  }

  trait Test extends ExampleData {
    def value: String

    override def `formField - number` = FormField(`fieldId - number`, value)

    override def `fieldValue - number` = FormComponent(
      `fieldId - number`,
      Text(Number(), Constant("")),
      "sample label", None, None, None, true, false, false, false, None
    )

    override def data = Map(
      `fieldId - number` -> `formField - number`
    )

    def validate(fieldValue: FormComponent, data: Map[FormComponentId, Seq[String]]) =
      new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever"), retrievals).validate(fieldValue)

    implicit lazy val hc: HeaderCarrier = HeaderCarrier()
  }

}