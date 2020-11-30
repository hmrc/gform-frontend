/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.eval.smartstring

import cats.MonadError
import cats.data.NonEmptyList
import org.mockito.ArgumentMatchersSugar
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.{ Matchers, WordSpec }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Span }
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, MaterialisedRetrievals, Role }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.EvaluationContext
import uk.gov.hmrc.gform.graph.{ Recalculation, RecalculationResult }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ FormModel, Interim, SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormData, FormField, FormModelOptics, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Checkbox, Choice, FormComponent, FormComponentId, FormCtx, FormTemplate, Horizontal, Radio, Value }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.SessionId

import scala.collection.immutable.List
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class RealSmartStringEvaluatorFactorySpec
    extends WordSpec with ExampleData with ArgumentMatchersSugar with IdiomaticMockito with ScalaFutures with Matchers {

  override implicit val patienceConfig =
    PatienceConfig(timeout = scaled(Span(5000, Millis)), interval = scaled(Span(15, Millis)))

  "SmartStringEvaluator" should {

    "evaluate SmartString with no interpolations" in new TestFixture {
      val result: String = smartStringEvaluator
        .apply(toSmartStringExpression("Smart string", Value), false)

      result shouldBe "Smart string"
    }

    "evaluate SmartString with FormCtx (type text) interpolation" in new TestFixture {
      lazy val textField: FormComponent = buildFormComponent(
        "textField",
        Value
      )
      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(textField.modelComponentId, "textValue")
            )))
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(nonRepeatingPageSection(title = "page1", fields = List(textField))))

      val result: String = smartStringEvaluator
        .apply(toSmartStringExpression("Smart string {0}", FormCtx(FormComponentId("textField"))), false)

      result shouldBe "Smart string textValue"
    }

    "evaluate SmartString with FormCtx (type choice) interpolation" in new TestFixture {

      lazy val choiceField: FormComponent = buildFormComponent(
        "choiceField",
        Choice(Radio, NonEmptyList.of(toSmartString("Yes"), toSmartString("No")), Horizontal, List.empty, None),
        None
      )
      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(choiceField.modelComponentId, "0")
            )))
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(nonRepeatingPageSection(title = "page1", fields = List(choiceField))))

      val result: String = smartStringEvaluator
        .apply(toSmartStringExpression("Smart string {0}", FormCtx(FormComponentId("choiceField"))), false)

      result shouldBe "Smart string Yes"
    }

    "evaluate SmartString with FormCtx (type choice, nested context) interpolation" in new TestFixture {

      lazy val textField: FormComponent = buildFormComponent(
        "textField",
        Value
      )

      lazy val choiceField: FormComponent = buildFormComponent(
        "choiceField",
        Choice(
          Radio,
          NonEmptyList.of(
            toSmartStringExpression("Yes {0}", FormCtx(FormComponentId("textField"))),
            toSmartStringExpression("No {0}", FormCtx(FormComponentId("textField")))),
          Horizontal,
          List.empty,
          None
        ),
        None
      )
      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(choiceField.modelComponentId, "0"),
              FormField(textField.modelComponentId, "textValue")
            )))
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(nonRepeatingPageSection(title = "page1", fields = List(textField, choiceField))))

      val result: String = smartStringEvaluator
        .apply(toSmartStringExpression("Smart string {0}", FormCtx(FormComponentId("choiceField"))), false)

      result shouldBe "Smart string Yes textValue"
    }

    "evaluate SmartString with FormCtx (type choice, with multi) interpolation" in new TestFixture {

      lazy val multiChoiceField: FormComponent = buildFormComponent(
        "multiChoiceField",
        Choice(
          Checkbox,
          NonEmptyList.of(toSmartString("Choice1"), toSmartString("Choice2")),
          Horizontal,
          List.empty,
          None),
        None
      )
      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(multiChoiceField.modelComponentId, "0,1")
            )))
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(nonRepeatingPageSection(title = "page1", fields = List(multiChoiceField))))

      val result: String = smartStringEvaluator
        .apply(toSmartStringExpression("Smart string {0}", FormCtx(FormComponentId("multiChoiceField"))), false)

      result shouldBe "Smart string Choice1,Choice2"
    }
  }

  trait TestFixture {

    implicit val headerCarrier: HeaderCarrier = HeaderCarrier()
    implicit val langADT: LangADT = LangADT.En
    implicit val sectionSelectorWithDeclaration: SectionSelector[SectionSelectorType.WithDeclaration] =
      SectionSelector.withDeclaration
    val retrievals = AnonymousRetrievals(SessionId("session-id"))
    val maybeAccessCode = Some(AccessCode("some-access-code"))
    val submissionRef = SubmissionRef("some-submission-ref")

    lazy val form: Form = buildForm
    lazy val formTemplate: FormTemplate = buildFormTemplate
    lazy val cache: AuthCacheWithForm =
      AuthCacheWithForm(retrievals, form, formTemplate, Role.Customer, maybeAccessCode)

    val mockRecalculation = mock[Recalculation[Future, Throwable]]
    mockRecalculation.recalculateFormDataNew(
      *[VariadicFormData[SourceOrigin.OutOfDate]],
      *[FormModel[Interim]],
      *[FormTemplate],
      *[MaterialisedRetrievals],
      *[ThirdPartyData],
      *[Option[AccessCode]],
      *[EnvelopeId],
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
          headerCarrier)))

    lazy val formModelOptics: FormModelOptics[DataOrigin.Mongo] = FormModelOptics
      .mkFormModelOptics[DataOrigin.Mongo, Future, SectionSelectorType.WithDeclaration](
        cache.variadicFormData[SectionSelectorType.WithDeclaration],
        cache,
        mockRecalculation)
      .futureValue

    lazy val factory = new RealSmartStringEvaluatorFactory()

    lazy val smartStringEvaluator: SmartStringEvaluator =
      factory.apply(formModelOptics.formModelVisibilityOptics, retrievals, maybeAccessCode, form, formTemplate)
  }
}
