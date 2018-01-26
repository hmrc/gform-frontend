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

import org.scalatest.mockito.MockitoSugar.mock
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.Retrievals
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

class ValidIfValidationSpec extends Spec {
  val retrievals: Retrievals = mock[Retrievals]
  "Valid if " should "return no errors" in new Test {
    override val value = "15"
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe ().valid
  }
  "Valid if " should "return an error if it's condition is not met" in new Test {
    override val value = "12"
    val expected = Map(`fieldValue - number`.id -> Set("Please enter required data")).invalid[Unit]
    validate(`fieldValue - number - validIf`, rawDataFromBrowser).futureValue shouldBe expected
  }
  "Valid if " should "return the error for invalid data instead of it's own" in new Test {
    override val value = "THX1138"
    val expected = Map(`fieldValue - number`.id -> Set("must be a number")).invalid[Unit]
    validate(`fieldValue - number`, rawDataFromBrowser).futureValue shouldBe expected withClue "we don't support alphabetics in number formats"
  }

  trait Test extends ExampleData {
    def value: String

    override def `formField - number` = FormField(`fieldId - number`, value)

    override def `fieldValue - number - validIf` = FormComponent(
      `fieldId - number`,
      Text(Number(), Constant("")),
      "sample label", None, None, Some(ValidIf(Equals(FormCtx("number"), Constant("15")))), true, false, false, false, false, None
    )

    override def data = Map(
      `fieldId - number` -> `formField - number`
    )

    def validate(fieldValue: FormComponent, data: Map[FormComponentId, Seq[String]]) =
      new ComponentsValidator(data, mock[FileUploadService], EnvelopeId("whatever"), retrievals).validate(fieldValue)

    implicit lazy val hc: HeaderCarrier = HeaderCarrier()
  }
}
