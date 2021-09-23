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

package uk.gov.hmrc.gform.pdf

import org.mockito.ArgumentMatchersSugar
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{ Millis, Seconds, Span }
import play.api.i18n.Messages
import play.api.test.{ FakeRequest, Helpers }
import uk.gov.hmrc.gform.Helpers.{ mkDataOutOfDate, toSmartString }
import uk.gov.hmrc.gform.auth.models.Role
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, CacheData }
import uk.gov.hmrc.gform.eval.smartstring.{ RealSmartStringEvaluatorFactory, SmartStringEvaluator }
import uk.gov.hmrc.gform.fileupload.{ Envelope, EnvelopeWithMapping, FileUploadAlgebra }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkFormTemplate, mkSection }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ FormModelSupport, SectionSelectorType }
import uk.gov.hmrc.gform.pdf.model.PDFModel.HeaderFooter
import uk.gov.hmrc.gform.pdf.model.PDFType
import uk.gov.hmrc.gform.sharedmodel.ExampleData.{ buildForm, buildFormComponent }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormData, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Instruction, Value }
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT }
import uk.gov.hmrc.gform.summary.SubmissionDetails
import uk.gov.hmrc.gform.validation.{ FieldOk, ValidationResult, ValidationService }
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import scala.collection.immutable.List
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PDFRenderServiceSpec
    extends AnyFlatSpec with Matchers with ArgumentMatchersSugar with IdiomaticMockito with FormModelSupport
    with ScalaFutures with PdfRenderServiceExpectations {

  override implicit val patienceConfig =
    PatienceConfig(timeout = scaled(Span(2, Seconds)), interval = scaled(Span(100, Millis)))

  implicit val request = FakeRequest()
  implicit val headerCarrier = HeaderCarrier()
  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages =
    Helpers.stubMessages(
      Helpers.stubMessagesApi(
        Map(
          "en" ->
            Map(
              "summary.checkYourAnswers" -> "Check your answers",
              "submission.details"       -> "Submission Details",
              "submission.date"          -> "Submission Date",
              "submission.reference"     -> "Submission Reference",
              "submission.mark"          -> "Submission Mark"
            )
        )
      )
    )

  trait Fixture {
    val fileUploadAlgebra = mock[FileUploadAlgebra[Future]]
    val validationService = mock[ValidationService]
    val pdfRenderService = new PDFRenderService(fileUploadAlgebra, validationService)

    lazy val variadicFormData = mkDataOutOfDate(
      "name" -> "name-value"
    )

    lazy val fcName = buildFormComponent(
      "name",
      Value
    )

    lazy val sections = List(
      mkSection(
        List(fcName)
      )
    )

    lazy val validationResult: ValidationResult = new ValidationResult(
      Map(
        fcName.id -> FieldOk(fcName, "name-value")
      ),
      None
    )

    lazy val form: Form =
      buildForm(
        FormData(List.empty)
      )

    lazy val formTemplate = mkFormTemplate(sections)
    lazy val cache = AuthCacheWithForm(retrievals, form, formTemplate, Role.Customer, maybeAccessCode)
    lazy val formModelOptics: FormModelOptics[DataOrigin.Mongo] = mkFormModelOptics(formTemplate, variadicFormData)

    implicit lazy val smartStringEvaluator: SmartStringEvaluator = new RealSmartStringEvaluatorFactory()
      .apply(formModelOptics.formModelVisibilityOptics, retrievals, maybeAccessCode, form, formTemplate)

    fileUploadAlgebra.getEnvelope(*[EnvelopeId])(*[HeaderCarrier]) shouldReturn Future.successful(Envelope.empty)
    validationService.validateFormModel(
      *[CacheData],
      *[EnvelopeWithMapping],
      *[FormModelVisibilityOptics[DataOrigin.Mongo]]
    )(
      *[HeaderCarrier],
      *[Messages],
      *[LangADT],
      *[SmartStringEvaluator]
    ) shouldReturn Future.successful(validationResult)
  }

  "createPDFHtml - PDFType.Summary" should "render summary PDF HTML for given form model" in new Fixture {
    implicit val now: LocalDateTime = LocalDateTime.now()
    whenReady(
      pdfRenderService.createPDFHtml[DataOrigin.Mongo, SectionSelectorType.Normal, PDFType.Summary](
        "PDF Title",
        Some("Page title"),
        cache,
        formModelOptics,
        Some(HeaderFooter(Some(toSmartString("Some PDF header")), Some(toSmartString("Some PDF footer")))),
        Some(SubmissionDetails(ExampleData.submission, "abcdefgh"))
      )
    ) { pdfHtml =>
      pdfHtml.html.trimLines shouldBe nonRepeatingPageSummaryPDFHTML
    }
  }

  "createPDFHtml - PDFType.Instruction" should "render instruction PDF HTML for given form model" in new Fixture {

    override lazy val fcName = buildFormComponent(
      "name",
      Value,
      Some(Instruction(Some(toSmartString("name-instruction")), Some(1)))
    )

    override lazy val sections = List(
      mkSection(
        List(fcName),
        Some(Instruction(Some(toSmartString("page1-instruction")), Some(1)))
      )
    )

    whenReady(
      pdfRenderService.createPDFHtml[DataOrigin.Mongo, SectionSelectorType.Normal, PDFType.Instruction](
        "PDF Title",
        Some("Page title"),
        cache,
        formModelOptics,
        Some(HeaderFooter(Some(toSmartString("Some PDF header")), Some(toSmartString("Some PDF footer")))),
        None
      )
    ) { pdfHtml =>
      println(pdfHtml.html)
      pdfHtml.html.trimLines shouldBe nonRepeatingPageInstructionPDFHTML
    }
  }
}
