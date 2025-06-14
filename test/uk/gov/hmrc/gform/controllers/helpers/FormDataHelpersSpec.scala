/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.controllers.helpers

import cats.data.NonEmptyList
import play.api.mvc.{ AnyContentAsFormUrlEncoded, Results }
import play.api.test.FakeRequest
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.controllers.RequestRelatedData
import uk.gov.hmrc.gform.eval.{ RevealingChoiceInfo, StandaloneSumInfo, StaticTypeInfo, SumInfo }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.SingletonWithNumber
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.optics.FormModelRenderPageOptics
import uk.gov.hmrc.gform.models.{ Bracket, BracketsWithSectionNumber, DataExpanded, EnteredVariadicFormData, FormModel, Singleton }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Checkbox, Choice, Constant, EORI, FormComponentId, FormTemplateId, Horizontal, NINO, OptionData, Page, RevealingChoice, RevealingChoiceElement, RoundingMode, SaUTR, SectionNumber, Sterling, Text, UkEORI, UkVrn, Value, WholeSterling }

import scala.concurrent.Future

class FormDataHelpersSpec extends Spec {

  private def toFormFields(xs: List[(String, String)]): List[FormField] = xs.map { case (fcId, value) =>
    FormField(FormComponentId(fcId).modelComponentId, value)
  }

  private def toOptionData(xs: NonEmptyList[String]): NonEmptyList[OptionData.IndexBased] =
    xs.map(l => OptionData.IndexBased(toSmartString(l), None, None, None, None))

  private def toOptionData(s: String): OptionData.IndexBased =
    OptionData.IndexBased(toSmartString(s), None, None, None, None)

  "updateFormField" should "update FormField in form data" in {
    val formFields = toFormFields(
      List(
        "1" -> "one",
        "2" -> "two",
        "3" -> "three"
      )
    )

    val existingForm = Form(
      FormId("666"),
      EnvelopeId("id1"),
      UserId("usr"),
      FormTemplateId("temp"),
      None,
      FormData(formFields),
      Accepted,
      VisitIndex.Classic(Set.empty),
      ThirdPartyData(
        NotChecked,
        Map.empty,
        QueryParams.empty,
        None,
        BooleanExprCache.empty,
        None,
        None,
        None,
        None,
        None,
        None
      ),
      None,
      FormComponentIdToFileIdMapping.empty,
      TaskIdTaskStatusMapping.empty,
      ConfirmationExprMapping.empty
    )

    val updatedForm =
      FormDataHelpers.updateFormField(existingForm, FormField(FormComponentId("2").modelComponentId, "xxx"))

    updatedForm.formData.fields contains toFormFields(List(("1", "one"), ("2", "xxx"), ("3", "three")))
  }

  "processResponseDataFromBody" should "build variadicFormData and requestRelatedData" in new TestFixture {

    val continuationFunction = (requestRelatedData: RequestRelatedData) =>
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) =>
        (_: EnteredVariadicFormData) => {
          requestRelatedData shouldBe RequestRelatedData(Map("actionField" -> List("save")))
          variadicFormData shouldBe VariadicFormData[SourceOrigin.OutOfDate](
            Map(
              purePure("formField1") -> VariadicValue.One("value1")
            )
          )
          Future.successful(Results.Ok)
        }

    val future = FormDataHelpers
      .processResponseDataFromBody(request, FormModelRenderPageOptics(formModel, RecData.empty))(
        continuationFunction
      )
    future.futureValue shouldBe Results.Ok
  }

  it should "remove currency symbol in value for formComponent with type Text(Sterling)" in new TestFixture {

    override lazy val fields = List(mkFormComponent("amountField", Text(Sterling(RoundingMode.Up, false), Value)))
    override lazy val requestBodyParams = Map("amountField" -> Seq("£111"))

    val continuationFunction = (_: RequestRelatedData) =>
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) =>
        (_: EnteredVariadicFormData) => {
          variadicFormData shouldBe VariadicFormData[SourceOrigin.OutOfDate](
            Map(
              purePure("amountField") -> VariadicValue.One("111")
            )
          )
          Future.successful(Results.Ok)
        }

    val future = FormDataHelpers
      .processResponseDataFromBody(request, FormModelRenderPageOptics(formModel, RecData.empty))(
        continuationFunction
      )
    future.futureValue shouldBe Results.Ok
  }

  it should "remove currency symbol in value for formComponent with type Text(WholeSterling(true/false))" in new TestFixture {

    override lazy val fields = List(mkFormComponent("amountField", Text(WholeSterling(true), Value)))
    override lazy val requestBodyParams = Map("amountField" -> Seq("£111"))

    val continuationFunction = (_: RequestRelatedData) =>
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) =>
        (_: EnteredVariadicFormData) => {
          variadicFormData shouldBe VariadicFormData[SourceOrigin.OutOfDate](
            Map(
              purePure("amountField") -> VariadicValue.One("111")
            )
          )
          Future.successful(Results.Ok)
        }

    val future = FormDataHelpers
      .processResponseDataFromBody(request, FormModelRenderPageOptics(formModel, RecData.empty))(
        continuationFunction
      )
    future.futureValue shouldBe Results.Ok
  }

  it should "remove GB from VAT number if it conforms to the standard format" in new TestFixture {

    override lazy val fields = List(mkFormComponent("vatField", Text(UkVrn, Value)))
    override lazy val requestBodyParams = Map("vatField" -> Seq("GB 123  456   789"))

    val continuationFunction = (_: RequestRelatedData) =>
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) =>
        (_: EnteredVariadicFormData) => {
          variadicFormData shouldBe VariadicFormData[SourceOrigin.OutOfDate](
            Map(
              purePure("vatField") -> VariadicValue.One("123456789")
            )
          )
          Future.successful(Results.Ok)
        }

    val future = FormDataHelpers
      .processResponseDataFromBody(request, FormModelRenderPageOptics(formModel, RecData.empty))(
        continuationFunction
      )
    future.futureValue shouldBe Results.Ok
  }

  it should "uppercase Nino" in new TestFixture {

    override lazy val fields = List(mkFormComponent("ninoField", Text(NINO, Value)))
    override lazy val requestBodyParams = Map("ninoField" -> Seq("Ab123456C"))

    val continuationFunction = (_: RequestRelatedData) =>
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) =>
        (_: EnteredVariadicFormData) => {
          variadicFormData shouldBe VariadicFormData[SourceOrigin.OutOfDate](
            Map(
              purePure("ninoField") -> VariadicValue.One("AB123456C")
            )
          )
          Future.successful(Results.Ok)
        }

    val future = FormDataHelpers
      .processResponseDataFromBody(request, FormModelRenderPageOptics(formModel, RecData.empty))(
        continuationFunction
      )
    future.futureValue shouldBe Results.Ok
  }

  it should "uppercase EORI" in new TestFixture {

    override lazy val fields = List(mkFormComponent("eoriField", Text(EORI, Value)))
    override lazy val requestBodyParams = Map("eoriField" -> Seq("fr1234567"))

    val continuationFunction = (_: RequestRelatedData) =>
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) =>
        (_: EnteredVariadicFormData) => {
          variadicFormData shouldBe VariadicFormData[SourceOrigin.OutOfDate](
            Map(
              purePure("eoriField") -> VariadicValue.One("FR1234567")
            )
          )
          Future.successful(Results.Ok)
        }

    val future = FormDataHelpers
      .processResponseDataFromBody(request, FormModelRenderPageOptics(formModel, RecData.empty))(
        continuationFunction
      )
    future.futureValue shouldBe Results.Ok
  }

  it should "uppercase UkEORI" in new TestFixture {

    override lazy val fields = List(mkFormComponent("ukEoriField", Text(UkEORI, Value)))
    override lazy val requestBodyParams = Map("ukEoriField" -> Seq("gb123456789123"))

    val continuationFunction = (_: RequestRelatedData) =>
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) =>
        (_: EnteredVariadicFormData) => {
          variadicFormData shouldBe VariadicFormData[SourceOrigin.OutOfDate](
            Map(
              purePure("ukEoriField") -> VariadicValue.One("GB123456789123")
            )
          )
          Future.successful(Results.Ok)
        }

    val future = FormDataHelpers
      .processResponseDataFromBody(request, FormModelRenderPageOptics(formModel, RecData.empty))(
        continuationFunction
      )
    future.futureValue shouldBe Results.Ok
  }

  it should "trim and replace CRLF with LF in app body parameters" in new TestFixture {

    override lazy val requestBodyParams = Map("formField1" -> Seq("value1\r\n23 "))

    val continuationFunction = (requestRelatedData: RequestRelatedData) =>
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) =>
        (_: EnteredVariadicFormData) => {
          variadicFormData shouldBe VariadicFormData[SourceOrigin.OutOfDate](
            Map(
              purePure("formField1") -> VariadicValue.One("value1\n23")
            )
          )
          Future.successful(Results.Ok)
        }

    val future = FormDataHelpers
      .processResponseDataFromBody(request, FormModelRenderPageOptics(formModel, RecData.empty))(
        continuationFunction
      )
    future.futureValue shouldBe Results.Ok
  }

  it should "remove choice(Choice, RevealingChoice) fields from VariadicFormData, if missing in request" in new TestFixture {

    override lazy val fields = List(
      mkFormComponent("formField1", Value),
      mkFormComponent(
        "choice1",
        Choice(
          Checkbox,
          toOptionData(NonEmptyList.of("Label1", "Label2")),
          Horizontal,
          List.empty,
          None,
          None,
          None,
          LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
          None,
          None,
          false
        )
      ),
      mkFormComponent(
        "revealingChoice1",
        RevealingChoice(
          List(
            RevealingChoiceElement(
              toOptionData("Label1"),
              List(mkFormComponent("revealingChoiceField1", Value)),
              None,
              false
            ),
            RevealingChoiceElement(
              toOptionData("Label2"),
              List(mkFormComponent("revealingChoiceField2", Value)),
              None,
              false
            )
          ),
          true
        )
      )
    )

    val continuationFunction = (requestRelatedData: RequestRelatedData) =>
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) =>
        (_: EnteredVariadicFormData) => {
          variadicFormData shouldBe VariadicFormData[SourceOrigin.OutOfDate](
            Map(
              purePure("formField1") -> VariadicValue.One(
                "value1"
              )
            )
          )
          Future.successful(Results.Ok)
        }

    val persistedData: RecData[SourceOrigin.Current] = RecData.fromData(
      VariadicFormData[SourceOrigin.Current](
        Map(
          purePure("formField1")            -> VariadicValue.One("value1"),
          purePure("choice1")               -> VariadicValue.Many(Seq("0", "1")),
          purePure("revealingChoice1")      -> VariadicValue.Many(Seq("0")),
          purePure("revealingChoiceField1") -> VariadicValue.One("revealingChoiceFieldValue1")
        )
      )
    )
    val future = FormDataHelpers
      .processResponseDataFromBody(
        request,
        FormModelRenderPageOptics(
          formModel,
          persistedData
        ),
        Some(SectionNumber.classicZero)
      )(
        continuationFunction
      )
    future.futureValue shouldBe Results.Ok
  }

  it should "remove spaces in SaUTR" in new TestFixture {

    override lazy val fields = List(mkFormComponent("utrField", Text(SaUTR, Value)))
    override lazy val requestBodyParams = Map("utrField" -> Seq("11 11 11 11 11"))

    val continuationFunction = (_: RequestRelatedData) =>
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) =>
        (_: EnteredVariadicFormData) => {
          variadicFormData shouldBe VariadicFormData[SourceOrigin.OutOfDate](
            Map(
              purePure("utrField") -> VariadicValue.One("1111111111")
            )
          )
          Future.successful(Results.Ok)
        }

    val future = FormDataHelpers
      .processResponseDataFromBody(request, FormModelRenderPageOptics(formModel, RecData.empty))(
        continuationFunction
      )
    future.futureValue shouldBe Results.Ok
  }

  trait TestFixture {
    lazy val fields = List(mkFormComponent("formField1", Constant("value1")))
    lazy val section = mkSection(fields)
    lazy val formModel = FormModel[DataExpanded](
      BracketsWithSectionNumber.Classic[DataExpanded](
        NonEmptyList.one(
          Bracket.NonRepeatingPage[DataExpanded](
            SingletonWithNumber(Singleton(section.page.asInstanceOf[Page[DataExpanded]]), SectionNumber.classicZero),
            section
          )
        )
      ),
      StaticTypeInfo.empty,
      RevealingChoiceInfo.empty,
      SumInfo.empty,
      StandaloneSumInfo.empty,
      None
    )
    lazy val requestBodyParams = Map("formField1" -> Seq("value1"), "actionField" -> Seq("save"))
    lazy val request = FakeRequest().withBody(AnyContentAsFormUrlEncoded(requestBodyParams))
  }

  private def purePure(fieldId: String) =
    ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId(fieldId)))
}
