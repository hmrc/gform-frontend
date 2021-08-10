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

import cats.MonadError
import cats.instances.future._
import org.mockito.ArgumentMatchersSugar
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Span }
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.test.{ FakeRequest, Helpers }
import uk.gov.hmrc.gform.eval.FileIdsWithMapping
import uk.gov.hmrc.gform.models.Visibility
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, MaterialisedRetrievals, Role }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.EvaluationContext
import uk.gov.hmrc.gform.eval.smartstring.{ RealSmartStringEvaluatorFactory, SmartStringEvaluator }
import uk.gov.hmrc.gform.graph.{ Recalculation, RecalculationResult }
import uk.gov.hmrc.gform.instructions.FormModelInstructionSummaryConverter.{ ChoiceElement, RevealingChoiceField, SimpleField }
import uk.gov.hmrc.gform.models.{ FormModel, Interim, SectionSelectorType }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.mkFormComponentWithInstr
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ CalendarDate, FormComponent }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormModelOptics, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, ExampleData, LangADT, SourceOrigin, SubmissionRef, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, FormTemplate, Instruction, Text, TextConstraint }
import uk.gov.hmrc.gform.validation.{ ComponentField, FieldError, FieldGlobalOk, FieldOk, HtmlFieldId, ValidationResult }
import uk.gov.hmrc.http.{ HeaderCarrier, SessionId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Address.{ country, postcode, street1, street2, street3, street4, uk }
import FormModelInstructionSummaryConverter.{ PageData, fieldOrdering }

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class InstructionPDFPageConverterSpec
    extends AnyFlatSpecLike with ScalaFutures with Matchers with ExampleData with ArgumentMatchersSugar
    with IdiomaticMockito {

  override implicit val patienceConfig =
    PatienceConfig(timeout = scaled(Span(3000, Millis)), interval = scaled(Span(15, Millis)))

  trait Fixture {

    val mockRecalculation = mock[Recalculation[Future, Throwable]]
    lazy val form: Form = buildForm
    lazy val formTemplate: FormTemplate = buildFormTemplate

    implicit val request = FakeRequest()
    implicit val headerCarrier = HeaderCarrier()
    implicit val langADT = LangADT.En
    implicit val messages = Helpers.stubMessagesApi().preferred(request)
    val submissionRef = SubmissionRef("some-submission-ref")
    val retrievals = AnonymousRetrievals(SessionId("session-id"))
    val maybeAccessCode = Some(AccessCode("some-access-code"))
    val cache = AuthCacheWithForm(retrievals, form, formTemplate, Role.Customer, maybeAccessCode)

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
          None,
          FileIdsWithMapping.empty,
          Map.empty,
          Set.empty,
          Set.empty,
          Map.empty,
          langADT,
          messages
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
    implicit val smartStringEvaluator: SmartStringEvaluator = new RealSmartStringEvaluatorFactory()
      .apply(formModelOptics.formModelVisibilityOptics, retrievals, maybeAccessCode, form, formTemplate)

    val textComponent = fieldValue(
      Text(TextConstraint.default, Constant("some text value")),
      instruction = Some(Instruction(Some(toSmartString("sample label - instruction")), None))
    )

    val textComponentPrefixSuffix = fieldValue(
      Text(
        TextConstraint.default,
        Constant("some text value"),
        prefix = Some(toSmartString("PREFIX")),
        suffix = Some(toSmartString("SUFFIX"))
      ),
      instruction = Some(Instruction(Some(toSmartString("sample label - instruction")), None))
    )
  }

  "convert" should "convert a Page to PageData" in new Fixture {
    val validationResult: ValidationResult = new ValidationResult(
      Map(
        textComponent.id -> FieldOk(textComponent, "some text value")
      ),
      None
    )

    val pageData = InstructionPDFPageConverter.convert(
      mkPage[Visibility](
        "Some Page Title",
        instruction = Some(buildInstruction("Some Page Title Instruction")),
        formComponents = List(textComponent)
      ),
      sectionNumber0,
      cache,
      envelopeWithMapping,
      validationResult
    )
    pageData shouldBe Some(
      PageData(
        Some("Some Page Title Instruction"),
        List(SimpleField(Some("sample label - instruction"), List("some text value"))),
        sectionNumber0.value.toString
      )
    )
  }

  it should "return empty when no fields have instruction defined" in new Fixture {
    val validationResult: ValidationResult = new ValidationResult(
      Map(
        textComponent.id -> FieldOk(textComponent, "some text value")
      ),
      None
    )

    val pageData = InstructionPDFPageConverter.convert(
      mkPage[Visibility](
        "Some Page Title",
        instruction = Some(buildInstruction("Some Page Title Instruction")),
        formComponents = List(textComponent.copy(instruction = None))
      ),
      sectionNumber0,
      cache,
      envelopeWithMapping,
      validationResult
    )
    pageData shouldBe None
  }

  "mapFormComponent" should "return PageField for text component, when validation result is OK" in new Fixture {
    val validationResult: ValidationResult = new ValidationResult(
      Map(
        textComponent.id -> FieldOk(textComponent, "some text value")
      ),
      None
    )

    val pageFieldData =
      InstructionPDFPageConverter
        .mapFormComponent(textComponent, cache, sectionNumber0, validationResult, envelopeWithMapping)

    pageFieldData shouldBe SimpleField(Some("sample label - instruction"), List("some text value"))
  }

  it should "return PageField for text component with value having prefix and suffix, when exists" in new Fixture {
    val validationResult: ValidationResult = new ValidationResult(
      Map(
        textComponent.id -> FieldError(textComponent, "some text value", Set.empty)
      ),
      None
    )
    val pageFieldData =
      InstructionPDFPageConverter
        .mapFormComponent(textComponentPrefixSuffix, cache, sectionNumber0, validationResult, envelopeWithMapping)

    pageFieldData shouldBe SimpleField(Some("sample label - instruction"), List("PREFIX some text value SUFFIX"))
  }

  it should "return PageField for calendarDate component" in new Fixture {

    val calendarDateComponent: FormComponent = mkFormComponentWithInstr(
      "date",
      CalendarDate,
      Some(Instruction(Some(toSmartString("CalendarDate - instruction")), None))
    )

    val validationResult: ValidationResult = new ValidationResult(
      Map(
        calendarDateComponent.id -> ComponentField(
          calendarDateComponent,
          Map(
            HtmlFieldId
              .pure(calendarDateComponent.atomicFormComponentId(CalendarDate.day)) -> FieldOk(
              calendarDateComponent,
              "01"
            ),
            HtmlFieldId
              .pure(calendarDateComponent.atomicFormComponentId(CalendarDate.month)) -> FieldOk(
              calendarDateComponent,
              "02"
            )
          )
        )
      ),
      None
    )
    val pageFieldData =
      InstructionPDFPageConverter
        .mapFormComponent(calendarDateComponent, cache, sectionNumber0, validationResult, envelopeWithMapping)

    pageFieldData shouldBe SimpleField(Some("CalendarDate - instruction"), List("1 date.February"))
  }

  it should "return RevealingChoiceField for revealing choice component, when validation result is OK" in new Fixture {

    val validationResult: ValidationResult = new ValidationResult(
      Map(
        `fieldValue - revealingChoice`.id -> ComponentField(
          `fieldValue - revealingChoice`,
          Map(
            HtmlFieldId.indexed(`fieldValue - revealingChoice`.id, 0) -> FieldOk(`fieldValue - revealingChoice`, "0"),
            HtmlFieldId.indexed(`fieldValue - revealingChoice`.id, 1) -> FieldOk(`fieldValue - revealingChoice`, "0")
          )
        ),
        `fieldValue - text1`.id -> FieldOk(`fieldValue - text1`, "value1"),
        `fieldValue - text2`.id -> FieldOk(`fieldValue - text2`, "value2"),
        `fieldValue - address`.id -> ComponentField(
          `fieldValue - address`,
          Map(
            HtmlFieldId.pure(`fieldValue - address`.atomicFormComponentId(street1)) -> FieldOk(
              `fieldValue - address`,
              "street1-value"
            ),
            HtmlFieldId
              .pure(`fieldValue - address`.atomicFormComponentId(street2)) -> FieldOk(`fieldValue - address`, ""),
            HtmlFieldId
              .pure(`fieldValue - address`.atomicFormComponentId(street3)) -> FieldOk(`fieldValue - address`, ""),
            HtmlFieldId
              .pure(`fieldValue - address`.atomicFormComponentId(street4)) -> FieldOk(`fieldValue - address`, ""),
            HtmlFieldId.pure(`fieldValue - address`.atomicFormComponentId(postcode)) -> FieldOk(
              `fieldValue - address`,
              "postcode-value"
            ),
            HtmlFieldId.pure(`fieldValue - address`.atomicFormComponentId(country)) -> FieldOk(
              `fieldValue - address`,
              "country-value"
            ),
            HtmlFieldId
              .pure(`fieldValue - address`.atomicFormComponentId(uk))    -> FieldOk(`fieldValue - address`, "true"),
            HtmlFieldId.pure(`fieldValue - address`.id.modelComponentId) -> FieldGlobalOk(`fieldValue - address`, "")
          )
        )
      ),
      None
    )

    val pageFieldData = InstructionPDFPageConverter
      .mapFormComponent(`fieldValue - revealingChoice`, cache, sectionNumber0, validationResult, envelopeWithMapping)

    pageFieldData shouldBe RevealingChoiceField(
      Some("Revealing Choice - instruction"),
      List(
        ChoiceElement(
          "choice1",
          List(
            SimpleField(Some("text1 - instruction"), List("value1")),
            SimpleField(Some("text2 - instruction"), List("value2"))
          )
        ),
        ChoiceElement(
          "choice2",
          List(
            SimpleField(Some("Address - instruction"), List("street1-value", "postcode-value", "country-value"))
          )
        )
      )
    )

  }

}
