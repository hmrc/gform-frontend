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

import cats.implicits._
import org.scalatest.mockito.MockitoSugar.mock
import play.api.i18n.Messages
import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormField, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.{ GraphSpec, Spec }
import uk.gov.hmrc.http.HeaderCarrier

import scala.collection.immutable.List

class ValidIfValidationSpec(implicit messages: Messages, l: LangADT) extends Spec with GraphSpec {

  val retrievals: MaterialisedRetrievals = mock[MaterialisedRetrievals]
  "Valid if " should "return no errors for valid if it's condition is met" in new Test {
    override val value = "15"
    validate(`fieldValue - number`, List(`fieldValue - number`), rawDataFromBrowser).futureValue shouldBe ().valid
  }
  "Valid if " should "return no errors for an info field even though it's condition is not met" in new Test {
    validate(`fieldValue - info`, List(`fieldValue - info`), rawDataFromBrowser).futureValue shouldBe ().valid
  }
  "Valid if " should "return an error for a text field if it's condition is not met" in new Test {
    override val value = "12"
    val expected =
      Map(`fieldValue - number`.id -> Set("sample label must be entered")).invalid[Unit]
    validate(`fieldValue - number`, List(`fieldValue - number`), rawDataFromBrowser).futureValue shouldBe expected
  }
  "Valid if " should "return an error for a choice field if it's condition is not met" in new Test {
    val expected =
      Map(`fieldValue - choice`.id -> Set("sample label must be selected")).invalid[Unit]
    validate(`fieldValue - choice`, List(`fieldValue - choice`), rawDataFromBrowser).futureValue shouldBe expected
  }
  "Valid if " should "return the error for invalid data instead of it's own" in new Test {
    override val value = "THX1138"
    val expected =
      Map(`fieldValue - number`.id -> Set("sample label must be a number")).invalid[Unit]
    validate(`fieldValue - number`, List(`fieldValue - number`), rawDataFromBrowser).futureValue shouldBe expected withClue "we don't support alphabetics in number formats"
  }

  trait Test extends ExampleData {
    def value: String = ""

    private val lookupRegistry = new LookupRegistry(Map.empty)

    override def `formField - number` = FormField(`fieldId - number`, value)

    override def `fieldValue - number` = FormComponent(
      `fieldId - number`,
      Text(Number(), Value),
      toLocalisedString("sample label"),
      None,
      None,
      Some(ValidIf(Equals(FormCtx("number"), Constant("15")))),
      true,
      false,
      false,
      false,
      false,
      None
    )

    override def validIf = Some(ValidIf(Equals(FormCtx("choice"), Value)))

    override def data = Map(
      `fieldId - number` -> `formField - number`
    )

    def validate(fieldValue: FormComponent, fieldValues: List[FormComponent], data: Map[FormComponentId, Seq[String]]) =
      new ComponentsValidator(
        mkFormDataRecalculated(data),
        mock[FileUploadService],
        EnvelopeId("whatever"),
        retrievals,
        booleanExprEval,
        ThirdPartyData.empty,
        ExampleData.formTemplate,
        lookupRegistry
      ).validate(fieldValue, fieldValues)

    implicit lazy val hc: HeaderCarrier = HeaderCarrier()
  }
}
