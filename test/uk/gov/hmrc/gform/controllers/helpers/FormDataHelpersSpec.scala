/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.gform.models.Bracket.NonRepeatingPage
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.optics.FormModelRenderPageOptics
import uk.gov.hmrc.gform.models.{ BracketsWithSectionNumber, DataExpanded, FormModel, Singleton }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Checkbox, Choice, Constant, FormComponentId, FormTemplateId, Horizontal, Page, RevealingChoice, RevealingChoiceElement, RoundingMode, SectionNumber, Sterling, Text, Value, WholeSterling }

import scala.concurrent.Future

class FormDataHelpersSpec extends Spec {

  private def toFormFields(xs: List[(String, String)]): List[FormField] = xs.map { case (fcId, value) =>
    FormField(FormComponentId(fcId).modelComponentId, value)
  }

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
      FormData(formFields),
      Accepted,
      VisitIndex(Set.empty),
      ThirdPartyData(None, NotChecked, Map.empty, QueryParams.empty, None, BooleanExprCache.empty),
      None,
      FormComponentIdToFileIdMapping.empty
      //EvaluationResults.empty
    )

    val updatedForm =
      FormDataHelpers.updateFormField(existingForm, FormField(FormComponentId("2").modelComponentId, "xxx"))

    updatedForm.formData.fields contains toFormFields(List(("1", "one"), ("2", "xxx"), ("3", "three")))
  }

  "processResponseDataFromBody" should "build variadicFormData and requestRelatedData" in new TestFixture {

    val continuationFunction = (requestRelatedData: RequestRelatedData) =>
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) => {
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
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) => {
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
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) => {
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

  it should "trim and replace CRLF with LF in app body parameters" in new TestFixture {

    override lazy val requestBodyParams = Map("formField1" -> Seq("value1\r\n23 "))

    val continuationFunction = (requestRelatedData: RequestRelatedData) =>
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) => {
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
          NonEmptyList.of(toSmartString("Label1"), toSmartString("Label2")),
          Horizontal,
          List.empty,
          None,
          None
        )
      ),
      mkFormComponent(
        "revealingChoice1",
        RevealingChoice(
          List(
            RevealingChoiceElement(
              toSmartString("Label1"),
              List(mkFormComponent("revealingChoiceField1", Value)),
              None,
              false
            ),
            RevealingChoiceElement(
              toSmartString("Label2"),
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
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) => {
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
        Some(SectionNumber(0))
      )(
        continuationFunction
      )
    future.futureValue shouldBe Results.Ok
  }

  "addSpacesInGovermentGatewayId" should "add a space after every 2 digits" in {
    val input = "9692090753538380"
    val result = FormDataHelpers.addSpacesInGovermentGatewayId(input)
    result shouldBe "96 92 09 07 53 53 83 80"
  }

  trait TestFixture {
    lazy val fields = List(mkFormComponent("formField1", Constant("value1")))
    lazy val section = mkSection(fields)
    lazy val formModel = FormModel[DataExpanded](
      BracketsWithSectionNumber[DataExpanded](
        NonEmptyList.one(
          NonRepeatingPage[DataExpanded](
            Singleton(section.page.asInstanceOf[Page[DataExpanded]]),
            SectionNumber(0),
            section
          )
        )
      ),
      StaticTypeInfo.empty,
      RevealingChoiceInfo.empty,
      SumInfo.empty,
      StandaloneSumInfo.empty
    )
    lazy val requestBodyParams = Map("formField1" -> Seq("value1"), "actionField" -> Seq("save"))
    lazy val request = FakeRequest().withBody(AnyContentAsFormUrlEncoded(requestBodyParams))
  }

  private def purePure(fieldId: String) =
    ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId(fieldId)))
}
