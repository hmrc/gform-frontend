/*
 * Copyright 2026 HM Revenue & Customs
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

import munit.FunSuite
import play.api.i18n.Messages
import play.api.mvc.{ AnyContentAsFormUrlEncoded, Result, Results }
import play.api.test.{ FakeRequest, Helpers }
import scala.concurrent.Future
import uk.gov.hmrc.gform.RealJsonTemplateSupport
import uk.gov.hmrc.gform.controllers.RequestRelatedData
import uk.gov.hmrc.gform.models.EnteredVariadicFormData
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.recalculation.{ EvaluationContext, MongoUserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ SectionNumber, TemplateSectionIndex }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, VariadicFormData, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.form.VisitIndex

class FormDataHelpersSuite extends FunSuite {

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  val data = List(
    (
      MongoUserData(),
      Option.empty[SectionNumber],
      Map("formField1" -> Seq("value1"), "actionField" -> Seq("save")),
      RequestRelatedData(Map("actionField" -> List("save"))),
      VariadicFormData(
        Map(
          purePure("formField1") -> VariadicValue.One("value1")
        )
      ),
      "text-field-text.json build variadicFormData and requestRelatedData"
    ),
    (
      MongoUserData(),
      Option.empty[SectionNumber],
      Map("amountField" -> Seq("£111")),
      RequestRelatedData.empty,
      VariadicFormData(
        Map(
          purePure("amountField") -> VariadicValue.One("111")
        )
      ),
      "text-field-sterling.json remove currency symbol in value for formComponent with type Text(Sterling)"
    ),
    (
      MongoUserData(),
      Option.empty[SectionNumber],
      Map("amountField" -> Seq("£111")),
      RequestRelatedData.empty,
      VariadicFormData(
        Map(
          purePure("amountField") -> VariadicValue.One("111")
        )
      ),
      "text-field-whole-sterling.json remove currency symbol in value for formComponent with type Text(WholeSterling(true/false))"
    ),
    (
      MongoUserData(),
      Option.empty[SectionNumber],
      Map("vatField" -> Seq("GB 123  456   789")),
      RequestRelatedData.empty,
      VariadicFormData(
        Map(
          purePure("vatField") -> VariadicValue.One("123456789")
        )
      ),
      "text-field-ukvrn.json remove GB from VAT number if it conforms to the standard format"
    ),
    (
      MongoUserData(),
      Option.empty[SectionNumber],
      Map("ninoField" -> Seq("Ab123456C")),
      RequestRelatedData.empty,
      VariadicFormData(
        Map(
          purePure("ninoField") -> VariadicValue.One("AB123456C")
        )
      ),
      "text-field-nino.json uppercase Nino"
    ),
    (
      MongoUserData(),
      Option.empty[SectionNumber],
      Map("eoriField" -> Seq("fr1234567")),
      RequestRelatedData.empty,
      VariadicFormData(
        Map(
          purePure("eoriField") -> VariadicValue.One("FR1234567")
        )
      ),
      "text-field-eori.json uppercase EORI"
    ),
    (
      MongoUserData(),
      Option.empty[SectionNumber],
      Map("ukEoriField" -> Seq("gb123456789123")),
      RequestRelatedData.empty,
      VariadicFormData(
        Map(
          purePure("ukEoriField") -> VariadicValue.One("GB123456789123")
        )
      ),
      "text-field-ukeori.json uppercase UkEORI"
    ),
    (
      MongoUserData(),
      Option.empty[SectionNumber],
      Map("formField1" -> Seq("value1\r\n23 ")),
      RequestRelatedData.empty,
      VariadicFormData(
        Map(
          purePure("formField1") -> VariadicValue.One("value1\n23")
        )
      ),
      "text-field-text.json trim and replace CRLF with LF in app body parameters"
    ),
    (
      MongoUserData(),
      Option.empty[SectionNumber],
      Map("utrField" -> Seq("11 11 11 11 11")),
      RequestRelatedData.empty,
      VariadicFormData(
        Map(
          purePure("utrField") -> VariadicValue.One("1111111111")
        )
      ),
      "text-field-sautr.json remove spaces in SaUTR"
    ),
    (
      MongoUserData(
        "formField1"            -> VariadicValue.One("value1"),
        "choice1"               -> VariadicValue.Many(Seq("0", "1")),
        "revealingChoice1"      -> VariadicValue.Many(Seq("0")),
        "revealingChoiceField1" -> VariadicValue.One("revealingChoiceFieldValue1")
      ),
      Some(SectionNumber.Classic.NormalPage(TemplateSectionIndex(0))),
      Map.empty[String, Seq[String]],
      RequestRelatedData.empty,
      VariadicFormData(
        Map(
          purePure("formField1") -> VariadicValue.One("value1")
        )
      ),
      "choice-cleanup.json remove choice(Choice, RevealingChoice) fields from VariadicFormData, if missing in request"
    )
  )

  data.zipWithIndex.map {
    case (
          (
            mongoUserData,
            maybeSectionNumber,
            requestBodyParams,
            expectedRequestRelatedData,
            expectedVariadicFormData,
            filenameWithComment
          ),
          i
        ) =>
      val index = i + 1
      val parts = filenameWithComment.split(" ", 2) // limit = 2 means split into at most 2 parts
      val (filename, comment) = if (parts.length == 1) {
        (parts(0), "")
      } else {
        (parts(0), parts(1).trim)
      }

      if (filename.isEmpty) {
        throw new Exception(s"Missing file name in: '$filenameWithComment'")
      }

      if (comment.endsWith("ref")) {
        refreshTemplates()
      }

      test(s"$filename $comment ($index)") {
        runTest(
          mongoUserData,
          maybeSectionNumber,
          requestBodyParams,
          expectedRequestRelatedData,
          expectedVariadicFormData,
          filename
        )
      }
  }

  def refreshTemplates() = RealJsonTemplateSupport.refreshTemplates("form-data-helpers")

  implicit val messages: Messages = Helpers.stubMessages(
    Helpers.stubMessagesApi(
      Map(
        "en" -> Map()
      )
    )
  )
  implicit val l: LangADT = LangADT.En

  private def purePure(fieldId: String) =
    ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId(fieldId)))

  def runTest(
    mongoUserData: MongoUserData,
    maybeSectionNumber: Option[SectionNumber],
    requestBodyParams: Map[String, Seq[String]],
    expectedRequestRelatedData: RequestRelatedData,
    expectedVariadicFormData: VariadicFormData,
    filename: String
  ): Future[Result] = {

    val filePath = s"test-templates/form-data-helpers/internal/$filename"

    val formModelOptics = RealJsonTemplateSupport.mkFormModelOptics(
      mongoUserData,
      VisitIndex.empty,
      EvaluationContext.empty,
      filePath
    )

    val request = FakeRequest().withBody(AnyContentAsFormUrlEncoded(requestBodyParams))

    val continuationFunction = (requestRelatedData: RequestRelatedData) =>
      (variadicFormData: VariadicFormData) =>
        (_: EnteredVariadicFormData) => {
          assertEquals(requestRelatedData, expectedRequestRelatedData)
          assertEquals(variadicFormData, expectedVariadicFormData)
          Future.successful(Results.Ok)
        }

    FormDataHelpers.processResponseDataFromBody(request, formModelOptics, maybeSectionNumber)(
      continuationFunction
    )
  }
}
