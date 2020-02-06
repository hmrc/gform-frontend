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

package uk.gov.hmrc.gform.eval

/* import org.scalatest.mockito.MockitoSugar.mock
 * import uk.gov.hmrc.gform.Helpers.toSmartString
 * import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
 * import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, VariadicFormData }
 * import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormField, ThirdPartyData }
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, Contains, Equals, FormComponent, FormComponentId, FormCtx, GreaterThan, GreaterThanOrEquals, LessThan, LessThanOrEquals, NotEquals, Number, Text }
 * import uk.gov.hmrc.gform.{ GraphSpec, Spec }
 * import uk.gov.hmrc.http.HeaderCarrier
 *
 * class BooleanExprEvalSpec extends Spec with GraphSpec {
 *   implicit val hc = HeaderCarrier()
 *
 *   val retrievals: MaterialisedRetrievals = mock[MaterialisedRetrievals]
 *
 *   val equalsCombinations = Table(
 *     ("Ctx value", "left argument", "right argument", "expected result"),
 *     ("0", "number", "0", true),
 *     ("0", "number", "1", false),
 *     ("00", "number", "0", true),
 *     ("0", "number", "00", true),
 *     ("1,000", "number", "1000", true),
 *     ("Fred", "firstName", "Fred", true),
 *     ("Fred", "firstName", "Dave", false),
 *     ("0", "number", "Fred", false),
 *     ("Fred", "firstName", "0", false)
 *   )
 *
 *   forAll(equalsCombinations) { (ctxValue, left, right, result) =>
 *     new Test {
 *       override val value = ctxValue
 *       booleanExprEval
 *         .isTrue(
 *           Equals(FormCtx(left), Constant(right)),
 *           rawDataFromBrowser,
 *           authContext,
 *           Set.empty,
 *           ThirdPartyData.empty,
 *           EnvelopeId(""),
 *           ExampleData.formTemplate) shouldBe result
 *     }
 *   }
 *
 *   val notEqualsCombinations = Table(
 *     ("Ctx value", "left argument", "right argument", "expected result"),
 *     ("0", "number", "0", false),
 *     ("0", "number", "1", true),
 *     ("00", "number", "0", false),
 *     ("0", "number", "00", false),
 *     ("1,000", "number", "1000", false),
 *     ("Fred", "firstName", "Fred", false),
 *     ("Fred", "firstName", "Dave", true),
 *     ("0", "number", "Fred", true),
 *     ("Fred", "firstName", "0", true)
 *   )
 *
 *   forAll(notEqualsCombinations) { (ctxValue, left, right, result) =>
 *     new Test {
 *       override val value = ctxValue
 *       booleanExprEval
 *         .isTrue(
 *           NotEquals(FormCtx(left), Constant(right)),
 *           rawDataFromBrowser,
 *           authContext,
 *           Set.empty,
 *           ThirdPartyData.empty,
 *           EnvelopeId(""),
 *           ExampleData.formTemplate) shouldBe result
 *     }
 *   }
 *
 *   val greaterThanCombinations = Table(
 *     ("Ctx value", "left argument", "right argument", "expected result"),
 *     ("1", "number", "0", true),
 *     ("0", "number", "-1", true),
 *     ("0", "number", "1", false),
 *     ("0", "number", "0", false),
 *     ("Fred", "firstName", "Dave", true),
 *     ("Dave", "firstName", "Fred", false),
 *     ("Fred", "firstName", "Fred", false)
 *   )
 *
 *   forAll(greaterThanCombinations) { (ctxValue, left, right, result) =>
 *     new Test {
 *       override val value = ctxValue
 *       booleanExprEval
 *         .isTrue(
 *           GreaterThan(FormCtx(left), Constant(right)),
 *           rawDataFromBrowser,
 *           authContext,
 *           Set.empty,
 *           ThirdPartyData.empty,
 *           EnvelopeId(""),
 *           ExampleData.formTemplate) shouldBe result
 *     }
 *   }
 *
 *   val greaterThanOrEqualsCombinations = Table(
 *     ("Ctx value", "left argument", "right argument", "expected result"),
 *     ("1", "number", "0", true),
 *     ("0", "number", "-1", true),
 *     ("0", "number", "1", false),
 *     ("0", "number", "0", true),
 *     ("Fred", "firstName", "Dave", true),
 *     ("Dave", "firstName", "Fred", false),
 *     ("Fred", "firstName", "Fred", true)
 *   )
 *
 *   forAll(greaterThanOrEqualsCombinations) { (ctxValue, left, right, result) =>
 *     new Test {
 *       override val value = ctxValue
 *       booleanExprEval
 *         .isTrue(
 *           GreaterThanOrEquals(FormCtx(left), Constant(right)),
 *           rawDataFromBrowser,
 *           authContext,
 *           Set.empty,
 *           ThirdPartyData.empty,
 *           EnvelopeId(""),
 *           ExampleData.formTemplate
 *         ) shouldBe result
 *     }
 *   }
 *
 *   val lessThanCombinations = Table(
 *     ("Ctx value", "left argument", "right argument", "expected result"),
 *     ("1", "number", "0", false),
 *     ("0", "number", "-1", false),
 *     ("0", "number", "1", true),
 *     ("0", "number", "0", false),
 *     ("Fred", "firstName", "Dave", false),
 *     ("Dave", "firstName", "Fred", true),
 *     ("Fred", "firstName", "Fred", false)
 *   )
 *
 *   forAll(lessThanCombinations) { (ctxValue, left, right, result) =>
 *     new Test {
 *       override val value = ctxValue
 *       booleanExprEval
 *         .isTrue(
 *           LessThan(FormCtx(left), Constant(right)),
 *           rawDataFromBrowser,
 *           authContext,
 *           Set.empty,
 *           ThirdPartyData.empty,
 *           EnvelopeId(""),
 *           ExampleData.formTemplate) shouldBe result
 *     }
 *   }
 *
 *   val lessThanOrEqualsCombinations = Table(
 *     ("Ctx value", "left argument", "right argument", "expected result"),
 *     ("1", "number", "0", false),
 *     ("0", "number", "-1", false),
 *     ("0", "number", "1", true),
 *     ("0", "number", "0", true),
 *     ("Fred", "firstName", "Dave", false),
 *     ("Dave", "firstName", "Fred", true),
 *     ("Fred", "firstName", "Fred", true)
 *   )
 *
 *   forAll(lessThanOrEqualsCombinations) { (ctxValue, left, right, result) =>
 *     new Test {
 *       override val value = ctxValue
 *       booleanExprEval
 *         .isTrue(
 *           LessThanOrEquals(FormCtx(left), Constant(right)),
 *           rawDataFromBrowser,
 *           authContext,
 *           Set.empty,
 *           ThirdPartyData.empty,
 *           EnvelopeId(""),
 *           ExampleData.formTemplate) shouldBe result
 *     }
 *   }
 *
 *   val containsCombinations = Table(
 *     ("Ctx value", "left argument", "right argument", "expected result"),
 *     (Seq(), "number", "0", false),
 *     (Seq("0"), "number", "0", true),
 *     (Seq("0"), "number", "1", false),
 *     (Seq("0", "1"), "number", "0", true),
 *     (Seq("0", "1"), "number", "1", true),
 *     (Seq("0", "1"), "number", "2", false)
 *   )
 *
 *   forAll(containsCombinations) { (ctxValue, left, right, result) =>
 *     new Test {
 *       override val value = "" // Irrelevant
 *       booleanExprEval
 *         .isTrue(
 *           Contains(FormCtx(left), Constant(right)),
 *           VariadicFormData.manys(FormComponentId(left) -> ctxValue),
 *           authContext,
 *           Set.empty,
 *           ThirdPartyData.empty,
 *           EnvelopeId(""),
 *           ExampleData.formTemplate
 *         ) shouldBe result
 *     }
 *   }
 *
 *   trait Test extends ExampleData {
 *     def value: String
 *
 *     override def `formField - number` = FormField(`fieldId - number`, value)
 *     override def `formField - firstName` = FormField(`fieldId - firstName`, value)
 *
 *     def `fieldId - zero` = FormComponentId("number")
 *
 *     def `fieldValue - zero` = FormComponent(
 *       `fieldId - zero`,
 *       Text(Number(), Constant("0")),
 *       toSmartString("sample label"),
 *       None,
 *       None,
 *       None,
 *       true,
 *       false,
 *       false,
 *       false,
 *       false,
 *       None
 *     )
 *   }
 * } */
