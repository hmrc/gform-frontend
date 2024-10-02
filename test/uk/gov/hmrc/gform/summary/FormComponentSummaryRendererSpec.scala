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

package uk.gov.hmrc.gform.summary

import org.mockito.ArgumentMatchersSugar
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{ Millis, Span }
import org.scalatest.wordspec.AnyWordSpecLike
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.mvc.{ AnyContentAsEmpty, Request }
import play.api.test.{ FakeRequest, Helpers }
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, Role }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.{ RealSmartStringEvaluatorFactory, SmartStringEvaluator }
import uk.gov.hmrc.gform.lookup.LocalisedLookupOptions
import uk.gov.hmrc.gform.models.{ FastForward, FormModelSupport, SectionSelectorType }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.ExampleData.{ buildForm, buildFormComponent, buildFormTemplate, destinationList, envelopeWithMapping, nonRepeatingPageSection }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, FormField, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.MiniSummaryRow.ValueRow
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormCtx, FormTemplate, FormTemplateContext, IsDisplayInSummary, MiniSummaryList, MiniSummaryListValue, SectionNumber, SectionOrSummary, SectionTitle4Ga, Value }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, NotChecked }
import uk.gov.hmrc.gform.validation.ValidationResult
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import uk.gov.hmrc.http.SessionId

class FormComponentSummaryRendererSpec
    extends AnyWordSpecLike with Matchers with ScalaFutures with ArgumentMatchersSugar with IdiomaticMockito
    with HtmlSupport with FormModelSupport {

  override implicit val patienceConfig: PatienceConfig =
    PatienceConfig(timeout = scaled(Span(15000, Millis)), interval = scaled(Span(15, Millis)))

  trait TestFixture {
    implicit val langADT: LangADT = LangADT.En
    lazy val i18nSupport: I18nSupport = new I18nSupport {
      override def messagesApi: MessagesApi =
        Helpers.stubMessagesApi(Map("en" -> Map("summary.acknowledgement.pdf" -> "Acknowledgement PDF")))
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
      LocalisedLookupOptions(Map())
    )

    lazy val formModelOptics: FormModelOptics[DataOrigin.Mongo] =
      mkFormModelOptics(formTemplate, cache.variadicFormData[SectionSelectorType.WithDeclaration])
        .asInstanceOf[FormModelOptics[DataOrigin.Mongo]]

    implicit val smartStringEvaluator: SmartStringEvaluator = new RealSmartStringEvaluatorFactory(messages)
      .apply(formModelOptics.formModelVisibilityOptics)
  }

  "FormComponentSummaryRenderer" should {
    "summaryListRows" should {
      "should return list of summary list rows from mini summary list component with DisplayInSummary case IsDisplayInSummary" in new TestFixture {
        lazy val nameField: FormComponent = buildFormComponent(
          "nameField",
          Value
        )
        lazy val minSummaryList: FormComponent = buildFormComponent(
          "miniSummaryList",
          MiniSummaryList(
            List(
              ValueRow(Option(toSmartString("Name")), MiniSummaryListValue.AnyExpr(FormCtx(nameField.id)), None, None)
            ),
            IsDisplayInSummary
          ),
          None
        )
        override lazy val form: Form =
          buildForm(
            FormData(
              List(
                FormField(nameField.modelComponentId, "nameValue")
              )
            )
          )
        override lazy val formTemplate: FormTemplate = buildFormTemplate(
          destinationList,
          sections = List(nonRepeatingPageSection(title = "page1", fields = List(nameField, minSummaryList)))
        )

        val rows: List[SummaryListRow] = FormComponentSummaryRenderer.summaryListRows[DataOrigin.Mongo, SummaryRender](
          minSummaryList,
          None,
          formTemplate._id,
          formModelOptics.formModelVisibilityOptics,
          None,
          SectionNumber.Classic(0),
          SectionTitle4Ga("page1"),
          NotChecked,
          ValidationResult.empty,
          envelopeWithMapping,
          AddressRecordLookup.from(cache.form.thirdPartyData),
          None,
          Some(List(FastForward.CYA(SectionOrSummary.FormSummary)))
        )
        rows.length shouldBe 1
        rows.head.key.content shouldBe Text("Name")
        rows.head.value.content shouldBe HtmlContent("nameValue")
      }
    }
  }

}
