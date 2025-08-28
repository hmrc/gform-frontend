/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models.helpers

import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import munit.FunSuite
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.mvc.{ AnyContentAsEmpty, Request }
import play.api.test.{ FakeRequest, Helpers }
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, Role }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.{ RealSmartStringEvaluatorFactory, SmartStringEvaluator }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.{ FormModelSupport, SectionSelectorType }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.sharedmodel.ExampleData.{ buildForm, buildFormComponent, buildFormTemplate, destinationList, nonRepeatingPageSection }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, FormField, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormCtx, FormTemplate, FormTemplateContext, ShortText, Text, Value }
import uk.gov.hmrc.gform.sharedmodel.graph.GraphDataCache
import uk.gov.hmrc.http.SessionId

class MiniSummaryListHelperSpec extends FunSuite with FormModelSupport {

  trait TestFixture {
    implicit val langADT: LangADT = LangADT.En
    lazy val i18nSupport: I18nSupport = new I18nSupport {
      override def messagesApi: MessagesApi =
        Helpers.stubMessagesApi(Map("en" -> Map("somthing.some.item" -> "Here is some text")))
    }
    lazy val form: Form = buildForm
    lazy val formTemplate: FormTemplate = buildFormTemplate
    implicit val request: Request[AnyContentAsEmpty.type] =
      FakeRequest().addAttr(FormTemplateKey, FormTemplateContext(formTemplate, None, None, None, None))
    implicit val messages: Messages = i18nSupport.request2Messages

    val retrievals: AnonymousRetrievals = AnonymousRetrievals(SessionId("session-id"))
    val maybeAccessCode: Option[AccessCode] = Some(AccessCode("some-access-code"))
    val cache: AuthCacheWithForm = AuthCacheWithForm(
      retrievals,
      form,
      FormTemplateContext.basicContext(formTemplate, None),
      Role.Customer,
      maybeAccessCode,
      new LookupRegistry(Map()),
      GraphDataCache.empty
    )

    lazy val formModelOptics: FormModelOptics[DataOrigin.Mongo] =
      mkFormModelOptics(formTemplate, cache.variadicFormData[SectionSelectorType.WithDeclaration])
        .asInstanceOf[FormModelOptics[DataOrigin.Mongo]]

    implicit val smartStringEvaluator: SmartStringEvaluator = new RealSmartStringEvaluatorFactory(messages)
      .apply(formModelOptics.formModelVisibilityOptics)
  }

  test("checkAndReturnSuffix should return a text component's suffix when it is present") {
    val table = Table(
      ("component", "suffix", "value"),
      (
        buildFormComponent(
          "textField",
          Text(ShortText(3, 5), Value, suffix = Some(toSmartString("kg"))),
          None
        ),
        " kg",
        "1000"
      ),
      (
        buildFormComponent(
          "textField",
          Text(ShortText(3, 5), Value),
          None
        ),
        "",
        "1001"
      )
    )
    forAll(table) { (component, suffix, value) =>
      val testFixture: TestFixture = new TestFixture {
        override lazy val formTemplate: FormTemplate = buildFormTemplate(
          destinationList,
          sections = List(nonRepeatingPageSection(title = "page1", fields = List(component)))
        )
        override lazy val form: Form =
          buildForm(
            FormData(
              List(
                FormField(component.modelComponentId, value)
              )
            )
          )
      }
      import testFixture._
      val output: String = MiniSummaryListHelper.checkAndReturnSuffix(
        FormCtx(component.id),
        formModelOptics.formModelVisibilityOptics.formModel
      )
      assertEquals(output, suffix)
    }
  }
}
