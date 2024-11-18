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

package uk.gov.hmrc.gform.eval.smartstring

import cats.MonadError
import cats.data.NonEmptyList
import org.mockito.ArgumentMatchersSugar
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Span }
import play.api.i18n.Messages
import play.api.test.Helpers
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, MaterialisedRetrievals, Role }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.{ EvaluationContext, EvaluationResults, ExpressionResult, FileIdsWithMapping }
import uk.gov.hmrc.gform.eval.ExpressionResult._
import uk.gov.hmrc.gform.graph.{ GraphData, RecData, Recalculation, RecalculationResult }
import uk.gov.hmrc.gform.models.DataRetrieveAll
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ FormModel, Interim, SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, FormField, FormModelOptics, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Checkbox, Choice, Concat, Constant, Expr, FileSizeLimit, FormComponent, FormComponentId, FormCtx, FormPhase, FormTemplate, FormTemplateContext, HideZeroDecimals, Horizontal, IfElse, IsFalse, Number, OptionData, PositiveNumber, Radio, RepeatedComponentsDetails, RevealingChoice, RevealingChoiceElement, RoundingMode, Sterling, Value }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.http.{ HeaderCarrier, SessionId }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import uk.gov.hmrc.gform.lookup.LookupRegistry

class RealSmartStringEvaluatorFactorySpec
    extends AnyWordSpecLike with ExampleData with ArgumentMatchersSugar with IdiomaticMockito with ScalaFutures
    with Matchers {

  override implicit val patienceConfig: PatienceConfig =
    PatienceConfig(timeout = scaled(Span(5000, Millis)), interval = scaled(Span(15, Millis)))

  private def toOptionData(xs: NonEmptyList[String]): NonEmptyList[OptionData.IndexBased] =
    xs.map(l => OptionData.IndexBased(toSmartString(l), None, None, None))

  private def toOptionData(s: String): OptionData.IndexBased = OptionData.IndexBased(toSmartString(s), None, None, None)

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
            )
          )
        )
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(nonRepeatingPageSection(title = "page1", fields = List(textField)))
      )

      val result: String = smartStringEvaluator
        .apply(toSmartStringExpression("Smart string {0}", FormCtx(FormComponentId("textField"))), false)

      result shouldBe "Smart string textValue"
    }

    "evaluate SmartString with FormCtx (type indexed text values) interpolation" in new TestFixture {
      lazy val textField: FormComponent = buildFormComponent(
        "textField",
        Value
      )
      lazy val (modelCompId1, modelCompId2) =
        (textField.modelComponentId.expandWithPrefix(1), textField.modelComponentId.expandWithPrefix(2))
      override lazy val indexedComponentIds: List[ModelComponentId] =
        List(modelCompId1, modelCompId2)
      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(modelCompId1, "value1"),
              FormField(modelCompId2, "value2")
            )
          )
        )
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(repeatingSection(title = "page1", fields = List(textField), None, Constant("2")))
      )

      override lazy val exprMap: Map[Expr, ExpressionResult] = Map(
        FormCtx(modelCompId1.toFormComponentId) -> StringResult("value1"),
        FormCtx(modelCompId2.toFormComponentId) -> StringResult("value2")
      )

      val result: String = smartStringEvaluator
        .apply(toSmartStringExpression("Smart string {0}", FormCtx(FormComponentId("textField"))), false)

      result shouldBe "Smart string value1, value2"
    }

    "evaluate SmartString with FormCtx, even when the component is not visible" in new TestFixture {
      lazy val textField: FormComponent = buildFormComponent(
        "textField",
        Value
      )
      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(textField.modelComponentId, "textValue")
            )
          )
        )
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(nonRepeatingPageSection(title = "page1", fields = List(textField)))
      )

      val result: String = smartStringEvaluator
        .apply(toSmartStringExpression("Smart string {0}", FormCtx(FormComponentId("textFieldInvisible"))), false)

      result shouldBe "Smart string "
    }

    "evaluate SmartString with FormCtx (type choice) interpolation" in new TestFixture {

      lazy val choiceField: FormComponent = buildFormComponent(
        "choiceField",
        Choice(
          Radio,
          toOptionData(NonEmptyList.of("Yes", "No")),
          Horizontal,
          List.empty,
          None,
          None,
          None,
          LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
          None,
          None,
          false
        ),
        None
      )
      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(choiceField.modelComponentId, "0")
            )
          )
        )
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(nonRepeatingPageSection(title = "page1", fields = List(choiceField)))
      )

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
          NonEmptyList
            .of(
              toSmartStringExpression("Yes {0}", FormCtx(FormComponentId("textField"))),
              toSmartStringExpression("No {0}", FormCtx(FormComponentId("textField")))
            )
            .map(l => OptionData.IndexBased(l, None, None, None)),
          Horizontal,
          List.empty,
          None,
          None,
          None,
          LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
          None,
          None,
          false
        ),
        None
      )
      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(choiceField.modelComponentId, "0"),
              FormField(textField.modelComponentId, "textValue")
            )
          )
        )
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(nonRepeatingPageSection(title = "page1", fields = List(textField, choiceField)))
      )

      val result: String = smartStringEvaluator
        .apply(toSmartStringExpression("Smart string {0}", FormCtx(FormComponentId("choiceField"))), false)

      result shouldBe "Smart string Yes textValue"
    }

    "evaluate SmartString with FormCtx (type choice, with multi) interpolation - multiple selections" in new TestFixture {

      lazy val multiChoiceField: FormComponent = buildFormComponent(
        "multiChoiceField",
        Choice(
          Checkbox,
          toOptionData(NonEmptyList.of("Choice1", "Choice2")),
          Horizontal,
          List.empty,
          None,
          None,
          None,
          LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
          None,
          None,
          false
        ),
        None
      )
      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(multiChoiceField.modelComponentId, "0,1")
            )
          )
        )
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(nonRepeatingPageSection(title = "page1", fields = List(multiChoiceField)))
      )

      val result: String = smartStringEvaluator
        .apply(toSmartStringExpression("Smart string {0}", FormCtx(FormComponentId("multiChoiceField"))), false)

      result shouldBe "Smart string Choice1, Choice2"
    }

    "evaluate SmartString with FormCtx (type choice, with multi) interpolation - single selection" in new TestFixture {

      lazy val multiChoiceField: FormComponent = buildFormComponent(
        "multiChoiceField",
        Choice(
          Checkbox,
          toOptionData(NonEmptyList.of("Choice1", "Choice2")),
          Horizontal,
          List.empty,
          None,
          None,
          None,
          LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
          None,
          None,
          false
        ),
        None
      )
      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(multiChoiceField.modelComponentId, "1")
            )
          )
        )
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(nonRepeatingPageSection(title = "page1", fields = List(multiChoiceField)))
      )

      val result: String = smartStringEvaluator
        .apply(toSmartStringExpression("Smart string {0}", FormCtx(FormComponentId("multiChoiceField"))), false)

      result shouldBe "Smart string Choice2"
    }

    "evaluate SmartString with FormCtx (type revealingChoice, with multi) interpolation - multiple selections" in new TestFixture {

      lazy val choice1TextField: FormComponent = buildFormComponent("choice1TextField", Value)
      lazy val choice2TextField: FormComponent = buildFormComponent("choice2TextField", Value)
      lazy val revealingChoiceField: FormComponent = buildFormComponent(
        "revealingChoiceField",
        RevealingChoice(
          List(
            RevealingChoiceElement(toOptionData("Option1"), List(choice1TextField), None, true),
            RevealingChoiceElement(toOptionData("Option2"), List(choice2TextField), None, true)
          ),
          true
        ),
        None
      )
      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(revealingChoiceField.modelComponentId, "0,1"),
              FormField(choice1TextField.modelComponentId, "choice1TextFieldValue"),
              FormField(choice2TextField.modelComponentId, "choice2TextFieldValue")
            )
          )
        )
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(nonRepeatingPageSection(title = "page1", fields = List(revealingChoiceField)))
      )

      val result: String = smartStringEvaluator
        .apply(toSmartStringExpression("Smart string {0}", FormCtx(FormComponentId("revealingChoiceField"))), false)

      result shouldBe "Smart string Option1, Option2"
    }

    "evaluate SmartString with FormCtx (type revealingChoice, with multi) interpolation - single selection" in new TestFixture {

      lazy val choice1TextField: FormComponent = buildFormComponent("choice1TextField", Value)
      lazy val choice2TextField: FormComponent = buildFormComponent("choice2TextField", Value)
      lazy val revealingChoiceField: FormComponent = buildFormComponent(
        "revealingChoiceField",
        RevealingChoice(
          List(
            RevealingChoiceElement(toOptionData("Option1"), List(choice1TextField), None, true),
            RevealingChoiceElement(toOptionData("Option2"), List(choice2TextField), None, true)
          ),
          true
        ),
        None
      )
      override lazy val form: Form =
        buildForm(
          FormData(
            List(
              FormField(revealingChoiceField.modelComponentId, "1"),
              FormField(choice1TextField.modelComponentId, "choice1TextFieldValue"),
              FormField(choice2TextField.modelComponentId, "choice2TextFieldValue")
            )
          )
        )
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(nonRepeatingPageSection(title = "page1", fields = List(revealingChoiceField)))
      )

      val result: String = smartStringEvaluator
        .apply(toSmartStringExpression("Smart string {0}", FormCtx(FormComponentId("revealingChoiceField"))), false)

      result shouldBe "Smart string Option2"
    }
  }

  trait TestFixture {

    implicit val headerCarrier: HeaderCarrier = HeaderCarrier()
    implicit val messages: Messages = Helpers.stubMessages(
      Helpers.stubMessagesApi(
        Map.empty
      )
    )
    implicit val langADT: LangADT = LangADT.En
    implicit val sectionSelectorWithDeclaration: SectionSelector[SectionSelectorType.WithDeclaration] =
      SectionSelector.withDeclaration
    val retrievals = AnonymousRetrievals(SessionId("session-id"))
    val maybeAccessCode = Some(AccessCode("some-access-code"))
    val submissionRef = SubmissionRef("some-submission-ref")

    lazy val form: Form = buildForm
    lazy val formTemplate: FormTemplate = buildFormTemplate
    lazy val cache: AuthCacheWithForm =
      AuthCacheWithForm(
        retrievals,
        form,
        FormTemplateContext.basicContext(formTemplate, None),
        Role.Customer,
        maybeAccessCode,
        new LookupRegistry(Map())
      )
    lazy val indexedComponentIds: List[ModelComponentId] = List.empty

    lazy val exprMap: Map[Expr, ExpressionResult] = Map.empty
    val mockRecalculation = mock[Recalculation[Future, Throwable]]
    mockRecalculation.recalculateFormDataNew(
      *[VariadicFormData[SourceOrigin.OutOfDate]],
      *[FormModel[Interim]],
      *[FormTemplate],
      *[MaterialisedRetrievals],
      *[ThirdPartyData],
      *[EvaluationContext],
      *[Messages]
    )(*[MonadError[Future, Throwable]]) returns Future.successful(
      RecalculationResult(
        EvaluationResults(
          exprMap,
          SourceOrigin.changeSource(RecData.fromData(cache.variadicFormData)),
          RepeatedComponentsDetails.empty
        ),
        GraphData.empty,
        BooleanExprCache.empty,
        EvaluationContext(
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
          Set.empty,
          Map.empty,
          LangADT.En,
          messages,
          indexedComponentIds,
          Set.empty,
          FileSizeLimit(1),
          DataRetrieveAll.empty,
          Set.empty[ModelComponentId],
          Map.empty,
          Set.empty,
          new LookupRegistry(Map()),
          Map.empty
        )
      )
    )

    lazy val formModelOptics: FormModelOptics[DataOrigin.Mongo] = FormModelOptics
      .mkFormModelOptics[DataOrigin.Mongo, Future, SectionSelectorType.WithDeclaration](
        cache.variadicFormData[SectionSelectorType.WithDeclaration],
        cache,
        mockRecalculation
      )
      .futureValue

    lazy val factory = new RealSmartStringEvaluatorFactory(messages)

    lazy val smartStringEvaluator: SmartStringEvaluator =
      factory.apply(formModelOptics.formModelVisibilityOptics)
  }

  "evaluate SmartString with FormCtx (type number) with hideZeroDecimals and Sterling text constraint" in new TestFixture {
    lazy val numberField: FormComponent = buildFormComponentWithTextConstraint(
      "numberField",
      Value,
      Sterling(RoundingMode.defaultRoundingMode, positiveOnly = false)
    )
    override lazy val form: Form =
      buildForm(
        FormData(
          List(
            FormField(numberField.modelComponentId, "14.00")
          )
        )
      )
    override lazy val formTemplate: FormTemplate = buildFormTemplate(
      destinationList,
      sections = List(nonRepeatingPageSection(title = "page1", fields = List(numberField)))
    )

    val result: String = smartStringEvaluator
      .apply(
        toSmartStringExpression("Smart string {0}", HideZeroDecimals(FormCtx(FormComponentId("numberField")))),
        false
      )

    result shouldBe "Smart string £14"
  }

  "evaluate SmartString with FormCtx (type number) with hideZeroDecimals and Number text constraint" in new TestFixture {
    lazy val numberField: FormComponent = buildFormComponentWithTextConstraint(
      "numberField",
      Value,
      Number(11, 2, RoundingMode.defaultRoundingMode, None)
    )
    override lazy val form: Form =
      buildForm(
        FormData(
          List(
            FormField(numberField.modelComponentId, "14.00")
          )
        )
      )
    override lazy val formTemplate: FormTemplate = buildFormTemplate(
      destinationList,
      sections = List(nonRepeatingPageSection(title = "page1", fields = List(numberField)))
    )

    val result: String = smartStringEvaluator
      .apply(
        toSmartStringExpression("Smart string {0}", HideZeroDecimals(FormCtx(FormComponentId("numberField")))),
        false
      )

    result shouldBe "Smart string 14"
  }

  "evaluate SmartString with FormCtx (type number) with hideZeroDecimals and Number with unit text constraint" in new TestFixture {
    lazy val numberField: FormComponent = buildFormComponentWithTextConstraint(
      "numberField",
      Value,
      Number(
        11,
        2,
        RoundingMode.defaultRoundingMode,
        Some(LocalisedString(Map(LangADT.En -> "litres", LangADT.Cy -> "litr")))
      )
    )
    override lazy val form: Form =
      buildForm(
        FormData(
          List(
            FormField(numberField.modelComponentId, "14.00")
          )
        )
      )
    override lazy val formTemplate: FormTemplate = buildFormTemplate(
      destinationList,
      sections = List(nonRepeatingPageSection(title = "page1", fields = List(numberField)))
    )

    val result: String = smartStringEvaluator
      .apply(
        toSmartStringExpression("Smart string {0}", HideZeroDecimals(FormCtx(FormComponentId("numberField")))),
        false
      )

    result shouldBe "Smart string 14 litres"
  }

  "evaluate SmartString with FormCtx (type number) with hideZeroDecimals and PositiveNumber text constraint" in new TestFixture {
    lazy val numberField: FormComponent = buildFormComponentWithTextConstraint(
      "numberField",
      Value,
      PositiveNumber(11, 2, RoundingMode.defaultRoundingMode, None)
    )
    override lazy val form: Form =
      buildForm(
        FormData(
          List(
            FormField(numberField.modelComponentId, "14.00")
          )
        )
      )
    override lazy val formTemplate: FormTemplate = buildFormTemplate(
      destinationList,
      sections = List(nonRepeatingPageSection(title = "page1", fields = List(numberField)))
    )

    val result: String = smartStringEvaluator
      .apply(
        toSmartStringExpression("Smart string {0}", HideZeroDecimals(FormCtx(FormComponentId("numberField")))),
        false
      )

    result shouldBe "Smart string 14"
  }

  "evaluate SmartString using Concat function with Sterling component in IfElse statement" in new TestFixture {
    lazy val sterlingField: FormComponent = buildFormComponentWithTextConstraint(
      "sterlingField",
      Value,
      Sterling(RoundingMode.defaultRoundingMode, positiveOnly = true)
    )
    override lazy val form: Form =
      buildForm(
        FormData(
          List(
            FormField(sterlingField.modelComponentId, "1000")
          )
        )
      )
    override lazy val formTemplate: FormTemplate = buildFormTemplate(
      destinationList,
      sections = List(nonRepeatingPageSection(title = "page1", fields = List(sterlingField)))
    )

    val result: String = smartStringEvaluator
      .apply(
        toSmartStringExpression(
          "Smart string {0}",
          IfElse(
            IsFalse,
            Constant("test"),
            Concat(List(Constant("result = "), FormCtx(FormComponentId("sterlingField"))))
          )
        ),
        false
      )

    result shouldBe "Smart string result = £1,000.00"
  }
}
