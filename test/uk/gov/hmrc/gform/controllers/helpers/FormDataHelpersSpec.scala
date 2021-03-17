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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, FormComponentId, FormTemplateId, Page, SectionNumber }

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
            ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId("formField1"))) -> VariadicValue.One("value1")
          )
        )
        Future.successful(Results.Ok)
      }

    val future = FormDataHelpers
      .processResponseDataFromBody(request, FormModelRenderPageOptics(formModel, RecData.empty))(continuationFunction)
    future.futureValue shouldBe Results.Ok
  }

  it should "trim and replace CRLF with LF in app body parameters" in new TestFixture {

    override lazy val requestBodyParams = Map("formField1" -> Seq("value1\r\n23 "))

    val continuationFunction = (requestRelatedData: RequestRelatedData) =>
      (variadicFormData: VariadicFormData[SourceOrigin.OutOfDate]) => {
        variadicFormData shouldBe VariadicFormData[SourceOrigin.OutOfDate](
          Map(
            ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId("formField1"))) -> VariadicValue.One(
              "value1\n23"
            )
          )
        )
        Future.successful(Results.Ok)
      }

    val future = FormDataHelpers
      .processResponseDataFromBody(request, FormModelRenderPageOptics(formModel, RecData.empty))(continuationFunction)
    future.futureValue shouldBe Results.Ok
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

}
