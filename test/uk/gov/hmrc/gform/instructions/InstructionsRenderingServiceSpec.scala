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

package uk.gov.hmrc.gform.instructions

import java.time.LocalDateTime
import cats.MonadError
import cats.data.NonEmptyList
import cats.instances.future._
import org.mockito.ArgumentMatchersSugar
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Span }
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.test.{ FakeRequest, Helpers }
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, MaterialisedRetrievals, Role }
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, CacheData }
import uk.gov.hmrc.gform.eval.FileIdsWithMapping
import uk.gov.hmrc.gform.eval.smartstring.{ RealSmartStringEvaluatorFactory, SmartStringEvaluator }
import uk.gov.hmrc.gform.eval.EvaluationContext
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadAlgebra }
import uk.gov.hmrc.gform.graph.{ FormTemplateBuilder, Recalculation, RecalculationResult }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ FormModel, Interim, SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AcknowledgementSectionPdf, Constant, FormComponent, FormPhase, FormTemplate, RevealingChoice, RevealingChoiceElement, Section, Value }
import uk.gov.hmrc.gform.submission.{ DmsMetaData, Submission, SubmissionId }
import uk.gov.hmrc.gform.summary.SubmissionDetails
import uk.gov.hmrc.gform.validation.HtmlFieldId.Indexed
import uk.gov.hmrc.gform.validation.{ ComponentField, FieldOk, HtmlFieldId, ValidationResult, ValidationService }
import uk.gov.hmrc.http.{ HeaderCarrier, SessionId }

import scala.collection.immutable.List
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class InstructionsRenderingServiceSpec
    extends AnyWordSpecLike with Matchers with ScalaFutures with ExampleData
    with InstructionsRenderingServiceSpecExpectations with ArgumentMatchersSugar with IdiomaticMockito {

  override implicit val patienceConfig =
    PatienceConfig(timeout = scaled(Span(5000, Millis)), interval = scaled(Span(15, Millis)))

  "createInstructionPDFHtml" should {

    "generate HTML for non-repeating-section" in new TestFixture {

      lazy val page1Field1 = buildFormComponent(
        "page1Field1",
        Constant("page1Field2Text"),
        Some(buildInstruction("page1Field1Instruction", Some(1)))
      )

      lazy val page1Field2 = buildFormComponent(
        "page1Field2",
        Constant("page1Field2Text"),
        Some(buildInstruction("page1Field2Instruction", Some(2)))
      )

      lazy val page2Field1 = buildFormComponent(
        "page2Field1",
        Constant("page2Field1Text"),
        Some(buildInstruction("page2Field1Instruction", Some(2)))
      )

      lazy val page2Field2 = buildFormComponent(
        "page2Field2",
        Constant("page2Field2Text"),
        Some(buildInstruction("page2Field2Instruction", Some(1)))
      )

      override lazy val validationResult: ValidationResult = new ValidationResult(
        Map(
          page1Field1.id -> FieldOk(page1Field1, "page1Field1Value"),
          page1Field2.id -> FieldOk(page1Field1, "page1Field2Value"),
          page2Field1.id -> FieldOk(page1Field1, "page2Field1Value"),
          page2Field2.id -> FieldOk(page1Field1, "page2Field2Value")
        ),
        None
      )

      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(page1Field1.id.modelComponentId, "page1Field1Value"),
              FormField(page1Field2.id.modelComponentId, "page1Field2Value"),
              FormField(page2Field1.id.modelComponentId, "page2Field1Value"),
              FormField(page2Field2.id.modelComponentId, "page2Field2Value")
            )
          )
        )

      override lazy val formTemplate: FormTemplate = formTemplateWithInstructions(
        List(
          nonRepeatingPageSection(
            title = "page1",
            instruction = Some(buildInstruction("page1Instruction", Some(2))),
            fields = List(page1Field1, page1Field2)
          ),
          nonRepeatingPageSection(
            title = "page2",
            instruction = Some(buildInstruction("page2Instruction", Some(1))),
            fields = List(page2Field1, page2Field2)
          )
        )
      )

      val pdfHtml =
        instructionRenderingService.createInstructionPDFHtml(cache, submissionDetails, formModelOptics).futureValue
      trimLines(pdfHtml.html) shouldBe nonRepeatingSectionsHtml
    }

    "generate HTML for instruction pdf - non repeating section with group" in new TestFixture {

      lazy val page1Field1GroupElement1 = buildFormComponent(
        "page1Field1GroupElement1",
        Constant("page1FieldGroupElement1Text"),
        Some(buildInstruction("page1Field1GroupElement1Instruction", Some(2)))
      )

      lazy val page1Field1GroupElement2 = buildFormComponent(
        "page1Field1GroupElement2",
        Constant("page1FieldGroupElement2Text"),
        Some(buildInstruction("page1Field1GroupElement2Instruction", Some(1)))
      )

      lazy val page1Field1 =
        buildFormComponent(
          "page1Field1",
          FormTemplateBuilder.mkGroup(2, List(page1Field1GroupElement1, page1Field1GroupElement2)),
          Some(buildInstruction("page1Field1Instruction", Some(1)))
        )

      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(page1Field1.withIndex(1).modelComponentId, ""),
              FormField(page1Field1.withIndex(2).modelComponentId, ""),
              FormField(page1Field1GroupElement1.withIndex(1).modelComponentId, "page1Field1GroupElement1Value1"),
              FormField(page1Field1GroupElement1.withIndex(2).modelComponentId, "page1Field1GroupElement1Value2"),
              FormField(page1Field1GroupElement2.withIndex(1).modelComponentId, "page1Field1GroupElement2Value1"),
              FormField(page1Field1GroupElement2.withIndex(2).modelComponentId, "page1Field1GroupElement2Value2")
            )
          )
        )

      override lazy val validationResult: ValidationResult = new ValidationResult(
        Map(
          page1Field1GroupElement1.withIndex(1).id -> FieldOk(
            page1Field1GroupElement1.withIndex(1),
            "page1Field1GroupElement1Value1"
          ),
          page1Field1GroupElement1.withIndex(2).id -> FieldOk(
            page1Field1GroupElement1.withIndex(2),
            "page1Field1GroupElement1Value2"
          ),
          page1Field1GroupElement2.withIndex(1).id -> FieldOk(
            page1Field1GroupElement2.withIndex(1),
            "page1Field1GroupElement2Value1"
          ),
          page1Field1GroupElement2.withIndex(2).id -> FieldOk(
            page1Field1GroupElement2.withIndex(2),
            "page1Field1GroupElement2Value2"
          )
        ),
        None
      )

      override lazy val formTemplate: FormTemplate = formTemplateWithInstructions(
        List(
          nonRepeatingPageSection(
            title = "page1",
            instruction = Some(buildInstruction("page1Instruction", Some(1))),
            fields = List(page1Field1)
          )
        )
      )

      val pdfHtml =
        instructionRenderingService.createInstructionPDFHtml(cache, submissionDetails, formModelOptics).futureValue
      trimLines(pdfHtml.html) shouldBe nonRepeatingSectionsWithGroupHtml
    }

    "generate HTML for repeating section" in new TestFixture {

      lazy val page1Field1 = buildFormComponent(
        "page1Field1",
        Constant("page1Field2Text"),
        Some(buildInstruction("page1Field1Instruction", Some(1)))
      )

      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(page1Field1.withIndex(1).modelComponentId, ""),
              FormField(page1Field1.withIndex(2).modelComponentId, "")
            )
          )
        )

      override lazy val validationResult: ValidationResult = new ValidationResult(
        Map(
          page1Field1.withIndex(1).id -> FieldOk(page1Field1.withIndex(1), "page1Field1Value1"),
          page1Field1.withIndex(2).id -> FieldOk(page1Field1.withIndex(2), "page1Field1Value2")
        ),
        None
      )

      override lazy val formTemplate: FormTemplate = formTemplateWithInstructions(
        List(
          repeatingSection(
            title = "page1",
            fields = List(page1Field1),
            instruction = Some(buildInstruction("page1Instruction", Some(1))),
            repeatsExpr = Constant("2")
          )
        )
      )

      val pdfHtml =
        instructionRenderingService.createInstructionPDFHtml(cache, submissionDetails, formModelOptics).futureValue
      trimLines(pdfHtml.html) shouldBe repeatingSectionHtml
    }

    "generate HTML for instruction pdf - revealing choice" in new TestFixture {

      lazy val revealingChoice1Field: FormComponent = buildFormComponent(
        "revealingChoice1Field",
        Constant("value1"),
        Some(buildInstruction("revealingChoice1FieldInstruction", Some(1)))
      )
      lazy val revealingChoice2Field: FormComponent = buildFormComponent(
        "revealingChoice2Field",
        Constant("value2"),
        Some(buildInstruction("revealingChoice2FieldInstruction", Some(1)))
      )
      lazy val revealingChoiceField: FormComponent = buildFormComponent(
        name = "revealingChoiceField",
        componentType = RevealingChoice(
          List(
            RevealingChoiceElement(
              toSmartString("choice1"),
              revealingChoice1Field :: Nil,
              None,
              true
            ),
            RevealingChoiceElement(
              toSmartString("choice2"),
              revealingChoice2Field :: Nil,
              None,
              true
            )
          ),
          true
        ),
        instruction = Some(buildInstruction("revealingChoiceFieldInstruction", Some(1)))
      )

      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(revealingChoiceField.modelComponentId, "0,1"),
              FormField(revealingChoice1Field.modelComponentId, "value1"),
              FormField(revealingChoice2Field.modelComponentId, "value2")
            )
          )
        )

      override lazy val validationResult: ValidationResult = new ValidationResult(
        Map(
          revealingChoiceField.id -> ComponentField(
            revealingChoiceField,
            Map(
              HtmlFieldId.indexed(revealingChoiceField.id, 0) -> FieldOk(revealingChoiceField, "0"),
              HtmlFieldId.indexed(revealingChoiceField.id, 1) -> FieldOk(revealingChoiceField, "0")
            )
          ),
          revealingChoice1Field.id -> FieldOk(revealingChoice1Field, "value1"),
          revealingChoice2Field.id -> FieldOk(revealingChoice2Field, "value2")
        ),
        None
      )

      override lazy val formTemplate: FormTemplate = formTemplateWithInstructions(
        List(
          nonRepeatingPageSection(
            title = "revealingChoicePage",
            instruction = Some(buildInstruction("revealingChoicePageInstruction", Some(1))),
            fields = List(revealingChoiceField)
          )
        )
      )

      val pdfHtml =
        instructionRenderingService.createInstructionPDFHtml(cache, submissionDetails, formModelOptics).futureValue
      trimLines(pdfHtml.html) shouldBe revealingChoiceSectionHtml
    }

    "generate HTML for add-to-list" in new TestFixture {

      lazy val addToListQuestionComponent = addToListQuestion("addToListQuestion")
      lazy val page1Field =
        buildFormComponent("page1Field", Value, Some(buildInstruction("page1FieldInstruction")))
      lazy val page2Field =
        buildFormComponent("page2Field", Value, Some(buildInstruction("page2FieldInstruction")))

      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(page1Field.withIndex(1).modelComponentId, "page1Field-value1"),
              FormField(page1Field.withIndex(2).modelComponentId, "page1Field-value2"),
              FormField(page2Field.withIndex(1).modelComponentId, "page2Field-value1"),
              FormField(page2Field.withIndex(2).modelComponentId, "page2Field-value2"),
              FormField(addToListQuestionComponent.withIndex(1).modelComponentId, "0"),
              FormField(addToListQuestionComponent.withIndex(2).modelComponentId, "1")
            )
          )
        )

      override lazy val validationResult: ValidationResult = new ValidationResult(
        Map(
          page1Field.withIndex(1).id -> FieldOk(page1Field.withIndex(1), "page1Field-value1"),
          page1Field.withIndex(2).id -> FieldOk(page1Field.withIndex(2), "page1Field-value2"),
          page2Field.withIndex(1).id -> FieldOk(page2Field.withIndex(1), "page2Field-value1"),
          page2Field.withIndex(2).id -> FieldOk(page2Field.withIndex(2), "page2Field-value2"),
          addToListQuestionComponent.withIndex(1).id -> ComponentField(
            addToListQuestionComponent.withIndex(1),
            Map(
              Indexed(addToListQuestionComponent.withIndex(1).id, 0) -> FieldOk(
                addToListQuestionComponent.withIndex(1),
                "1"
              )
            )
          ),
          addToListQuestionComponent.withIndex(2).id -> ComponentField(
            addToListQuestionComponent.withIndex(2),
            Map(
              Indexed(addToListQuestionComponent.withIndex(2).id, 1) -> FieldOk(
                addToListQuestionComponent.withIndex(2),
                "0"
              )
            )
          )
        ),
        None
      )

      override lazy val formTemplate: FormTemplate = formTemplateWithInstructions(
        List(
          addToListSection(
            "add To List",
            "addToListDesc",
            "addToListShortName",
            "addToListSummaryName",
            addToListQuestionComponent,
            Some(buildInstruction("addToListInstruction", Some(1))),
            List(
              toPage("page1", Some(buildInstruction("page1Instruction", Some(2))), List(page1Field)),
              toPage("page2", Some(buildInstruction("page2Instruction", Some(1))), List(page2Field))
            ),
            None
          )
        )
      )

      val pdfHtml =
        instructionRenderingService.createInstructionPDFHtml(cache, submissionDetails, formModelOptics).futureValue
      trimLines(pdfHtml.html) shouldBe addToListSectionHtml
    }
  }

  trait TestFixture {

    def formTemplateWithInstructions(sectionList: List[Section] = List.empty) = {
      val hmrcDmsDest = hmrcDms.copy(includeInstructionPdf = true)
      val acknowledgementSection = ackSection.copy(
        instructionPdf = Some(
          AcknowledgementSectionPdf(Some(toSmartString("some-pdf-header")), Some(toSmartString("some-pdf-footer")))
        )
      )
      buildFormTemplate(
        destinationList
          .copy(destinations = NonEmptyList.of(hmrcDmsDest), acknowledgementSection = acknowledgementSection),
        sections = sectionList
      )
    }

    val i18nSupport: I18nSupport = new I18nSupport {
      override def messagesApi: MessagesApi = Helpers.stubMessagesApi()
    }

    implicit val sectionSelectorNormal: SectionSelector[SectionSelectorType.Normal] =
      SectionSelector.normal

    implicit val request = FakeRequest()
    implicit val headerCarrier = HeaderCarrier()
    implicit val langADT = LangADT.En
    val submissionRef = SubmissionRef("some-submission-ref")
    val retrievals = AnonymousRetrievals(SessionId("session-id"))
    val maybeAccessCode = Some(AccessCode("some-access-code"))

    lazy val form: Form = buildForm
    lazy val formTemplate: FormTemplate = buildFormTemplate

    val cache = AuthCacheWithForm(retrievals, form, formTemplate, Role.Customer, maybeAccessCode)

    val mockFileUploadService = mock[FileUploadAlgebra[Future]]
    val mockValidationService = mock[ValidationService]
    val mockRecalculation = mock[Recalculation[Future, Throwable]]

    val instructionRenderingService =
      new InstructionsRenderingService(i18nSupport, mockFileUploadService, mockValidationService)

    lazy val validationResult = ValidationResult.empty

    mockFileUploadService.getEnvelope(*[EnvelopeId])(*[HeaderCarrier]) returns Future.successful(Envelope(List.empty))
    mockValidationService
      .validateFormModel(*[CacheData], *[EnvelopeWithMapping], *[FormModelVisibilityOptics[DataOrigin.Mongo]])(
        *[HeaderCarrier],
        *[Messages],
        *[LangADT],
        *[SmartStringEvaluator]
      ) returns Future.successful(validationResult)
    mockRecalculation.recalculateFormDataNew(
      *[VariadicFormData[SourceOrigin.OutOfDate]],
      *[FormModel[Interim]],
      *[FormTemplate],
      *[MaterialisedRetrievals],
      *[ThirdPartyData],
      *[EvaluationContext]
    )(*[MonadError[Future, Throwable]]) returns Future.successful(
      RecalculationResult.empty(
        new EvaluationContext(
          formTemplate._id,
          submissionRef,
          maybeAccessCode,
          retrievals,
          ThirdPartyData.empty,
          authConfig,
          headerCarrier,
          Option.empty[FormPhase],
          FileIdsWithMapping.empty,
          Map.empty,
          Set.empty,
          Set.empty,
          LangADT.En
        )
      )
    )

    val formModelOptics: FormModelOptics[DataOrigin.Mongo] = FormModelOptics
      .mkFormModelOptics[DataOrigin.Mongo, Future, SectionSelectorType.Normal](
        cache.variadicFormData[SectionSelectorType.WithDeclaration],
        cache,
        mockRecalculation
      )
      .futureValue
    val submissionDetails = Some(
      SubmissionDetails(
        Submission(
          SubmissionId(form._id, EnvelopeId("some-envelope-id")),
          LocalDateTime.now(),
          submissionRef,
          EnvelopeId("some-envelope-id"),
          DmsMetaData(form.formTemplateId)
        ),
        ""
      )
    )

    implicit val smartStringEvaluator: SmartStringEvaluator = new RealSmartStringEvaluatorFactory()
      .apply(formModelOptics.formModelVisibilityOptics, retrievals, maybeAccessCode, form, formTemplate)
  }

  implicit class FormComponentOps(formComponent: FormComponent) {
    def withIndex(index: Int) =
      formComponent.copy(id = formComponent.id.copy(value = index + "_" + formComponent.id.value))
  }
}
