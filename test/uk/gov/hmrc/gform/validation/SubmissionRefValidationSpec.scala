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

package uk.gov.hmrc.gform.validation

/* import cats.data.Validated
 * import org.mockito.Mockito._
 * import play.api.i18n.{ Lang, Messages, MessagesApi, MessagesImpl }
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ FormComponentGen, SubmissionRefGen }
 * import uk.gov.hmrc.gform.sharedmodel.{ LangADT, VariadicFormData }
 * import uk.gov.hmrc.gform.eval.smartstring._
 * import uk.gov.hmrc.gform.Spec
 * import uk.gov.hmrc.gform.graph.RecData
 * import uk.gov.hmrc.gform.lookup.LookupRegistry
 * import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateCtx, FormTemplateProp, IsUpperCase, SubmissionRefFormat, Text }
 *
 * class SubmissionRefValidationSpec extends Spec {
 *   "A valid submission reference" should "be accepted" in {
 *     forAll(FormComponentGen.formComponentGen(1), SubmissionRefGen.submissionRefGen) {
 *       (generatedComponent, submissionRef) =>
 *         val formComponent = generatedComponent.copy(
 *           `type` = Text(
 *             SubmissionRefFormat,
 *             FormTemplateCtx(FormTemplateProp.SubmissionReference),
 *             toUpperCase = IsUpperCase
 *           ),
 *           shortName = None)
 *
 *         implicit val langADT: LangADT = LangADT.En
 *         val lang = Lang(langADT.langADTToString)
 *         val messagesApi = org.scalatest.mockito.MockitoSugar.mock[MessagesApi]
 *         implicit val messages: Messages = MessagesImpl(lang, messagesApi)
 *
 *         ComponentValidator.validateText(formComponent, SubmissionRefFormat)(
 *           FormDataRecalculated(
 *             Set.empty,
 *             RecData(VariadicFormData.ones(formComponent.id -> submissionRef.value), Map.empty)),
 *           null) shouldBe Validated.Valid(())
 *     }
 *   }
 *
 *   "An in valid submission reference" should "be rejected" in {
 *     forAll(FormComponentGen.formComponentGen(1), SubmissionRefGen.submissionRefGen) {
 *       (generatedComponent, submissionRef) =>
 *         val formComponent = generatedComponent.copy(
 *           `type` = Text(
 *             SubmissionRefFormat,
 *             FormTemplateCtx(FormTemplateProp.SubmissionReference),
 *             toUpperCase = IsUpperCase
 *           ),
 *           shortName = None)
 *
 *         implicit val langADT: LangADT = LangADT.En
 *         val lang = Lang(langADT.langADTToString)
 *         val messagesApi = org.scalatest.mockito.MockitoSugar.mock[MessagesApi]
 *
 *         when(messagesApi("helper.order", formComponent.label.value, "")(lang))
 *           .thenReturn("foo")
 *         implicit val messages: Messages = MessagesImpl(lang, messagesApi)
 *
 *         ComponentValidator.validateText(formComponent, SubmissionRefFormat)(
 *           FormDataRecalculated(
 *             Set.empty,
 *             RecData(VariadicFormData.ones(formComponent.id -> ("1" + submissionRef.value)), Map.empty)),
 *           new LookupRegistry(Map.empty)
 *         ) shouldBe Validated.Invalid(Map(formComponent.id -> Set(formComponent.errorMessage.map(_.value).orNull)))
 *     }
 *   }
 * } */
