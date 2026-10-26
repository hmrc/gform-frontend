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

import cats.data.NonEmptyList
import org.jsoup.Jsoup
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
import uk.gov.hmrc.gform.gform.PageLevelErrorHtml
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.{ FastForward, FormModelSupport, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.ExampleData.{ buildForm, buildFormComponent, buildFormTemplate, destinationList, envelopeWithMapping, nonRepeatingPageSection }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, FormField, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Choice, Constant, DisplayInSummary, Equals, FormComponent, FormComponentId, FormCtx, FormTemplate, FormTemplateContext, IncludeIf, InformationMessage, IsFalse, IsTrue, KeyDisplayWidth, Mandatory, MiniSummaryList, MiniSummaryListValue, NoFormat, OptionData, RevealingChoice, RevealingChoiceElement, SectionNumber, SectionOrSummary, SectionTitle4Ga, TemplateSectionIndex, Value }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.MiniSummaryRow.ValueRow
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, ExampleData, LangADT, LocalisedString, NotChecked, SmartString }
import uk.gov.hmrc.gform.validation.{ ComponentField, FieldError, FieldOk, HtmlFieldId, PageError, ValidationResult }
import uk.gov.hmrc.govukfrontend.views.Aliases.{ Empty, Text }
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import uk.gov.hmrc.http.SessionId

import scala.collection.mutable.LinkedHashSet

class FormComponentSummaryRendererSpec extends FunSuite with FormModelSupport {

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
      new LookupRegistry(Map())
    )

    lazy val formModelOptics: FormModelOptics =
      FormModelOptics
        .mkFormModelOptics[SectionSelectorType.WithDeclaration](
          cache.variadicFormData,
          cache,
          recomputeGraph = false
        )

    implicit val smartStringEvaluator: SmartStringEvaluator = new RealSmartStringEvaluatorFactory(messages)
      .apply(formModelOptics.formModelVisibilityOptics)
  }

  test("shared errors highlight every member on CYA without changing row content or unrelated fields") {
    val fields = List("first", "second", "unrelated").map(id => buildFormComponent(id, Value))
    val fixture = new TestFixture {
      override lazy val formTemplate: FormTemplate = buildFormTemplate(
        destinationList,
        sections = List(nonRepeatingPageSection(title = "page1", fields = fields))
      )
      override lazy val form: Form = buildForm(
        FormData(fields.map(field => FormField(field.modelComponentId, field.id.value + " value")))
      )
    }
    import fixture._
    val lookup = fields.map(field => field.id -> FieldOk(field, field.id.value + " value")).toMap
    val sharedError = PageError("The values must match", NonEmptyList.of(fields.head, fields(1)))
    val sharedResult = ValidationResult(lookup, None, List(sharedError))
    val previousPresentation = ValidationResult(
      lookup.updated(fields.head.id, FieldError(fields.head, "first value", LinkedHashSet(sharedError.error))),
      None,
      Nil
    )

    def render(field: FormComponent, result: ValidationResult): List[SummaryListRow] =
      FormComponentSummaryRenderer.summaryListRows[SummaryRender](
        field,
        None,
        formTemplate._id,
        formModelOptics.formModelVisibilityOptics,
        None,
        SectionNumber.Classic.NormalPage(TemplateSectionIndex(0)),
        SectionTitle4Ga("page1"),
        NotChecked,
        result,
        envelopeWithMapping,
        AddressRecordLookup.from(cache.form.thirdPartyData),
        None,
        Some(List(FastForward.CYA(SectionOrSummary.FormSummary))),
        KeyDisplayWidth.M
      )

    fields.foreach { field =>
      val previous = render(field, previousPresentation)
      val actual = render(field, sharedResult)
      assertEquals(actual.size, 1)
      val isMember = sharedError.fields.exists(_.id == field.id)
      assertEquals(actual.head.key.classes.split(" ").contains("summary--error"), isMember)
      assertEquals(actual.head.key.classes.split(" ").count(_ == "summary--error"), if (isMember) 1 else 0)
      assertEquals(actual.head.copy(key = previous.head.key), previous.head)
      assertEquals(actual.head.key.content, previous.head.key.content)
      assert(actual.head.key.classes.contains("summary-list__key_medium"))
    }
    assertEquals(render(fields(2), sharedResult), render(fields(2), previousPresentation))
  }

  List(
    ExampleData.`fieldValue - choice`.copy(id = FormComponentId("fooBarBaz")),
    buildFormComponent("revealing", Value).copy(
      `type` = RevealingChoice(
        List(
          RevealingChoiceElement(
            OptionData.IndexBased(toSmartString("Foo"), None, None, None, None),
            Nil,
            true
          )
        ),
        true
      )
    ),
    buildFormComponent("text", Value)
  ).foreach { field =>
    test(s"CYA shared error link targets the rendered inline error for ${field.id.value}") {
      val fixture = new TestFixture {
        override lazy val formTemplate: FormTemplate = buildFormTemplate(
          destinationList,
          sections = List(nonRepeatingPageSection(title = "page1", fields = List(field)))
        )
        override lazy val form: Form = buildForm(
          FormData(List(FormField(field.modelComponentId, "0")))
        )
      }
      import fixture._
      val result = field.`type` match {
        case _: Choice | _: RevealingChoice =>
          ComponentField(field, Map(HtmlFieldId.indexed(field.id, "0") -> FieldOk(field, "0")))
        case _ => FieldOk(field, "0")
      }
      val validationResult =
        ValidationResult(Map(field.id -> result), None, List(PageError("Check the answer", NonEmptyList.one(field))))
      val errorSummary = Jsoup.parse(
        PageLevelErrorHtml
          .generateSummaryLevelErrorHtml(validationResult.formFieldValidationResults, Nil, validationResult.pageErrors)
          .render
          .body
      )
      val rows = FormComponentSummaryRenderer.summaryListRows[SummaryRender](
        field,
        None,
        formTemplate._id,
        formModelOptics.formModelVisibilityOptics,
        None,
        SectionNumber.Classic.NormalPage(TemplateSectionIndex(0)),
        SectionTitle4Ga("page1"),
        NotChecked,
        validationResult,
        envelopeWithMapping,
        AddressRecordLookup.from(cache.form.thirdPartyData),
        None,
        Some(List(FastForward.CYA(SectionOrSummary.FormSummary))),
        KeyDisplayWidth.M
      )
      val links = errorSummary.select(".govuk-error-summary__list a")
      assertEquals(links.size(), 1)
      val href = links.first().attr("href")
      assert(href.startsWith("#"))
      val renderedValues = Jsoup.parse(rows.map(_.value.content.asHtml.body).mkString)
      assert(
        renderedValues.getElementById(href.drop(1)) != null,
        s"Error link $href does not target a rendered inline error"
      )
    }
  }

  test("summaryListRows should return correct list of summary list rows from mini summary list component") {
    val table = Table(
      ("miniSummaryList", "key", "value", "count"),
      (
        buildFormComponent(
          "miniSummaryList",
          MiniSummaryList(
            List(
              ValueRow(
                Option(toSmartString("Name")),
                MiniSummaryListValue.AnyExpr(FormCtx(FormComponentId("nameField"))),
                None,
                None,
                None
              )
            ),
            DisplayInSummary(IsTrue),
            None
          ),
          None
        ),
        "Name",
        "nameValue",
        1
      ),
      (
        buildFormComponent(
          "miniSummaryList",
          MiniSummaryList(
            List(
              ValueRow(
                Option(toSmartString("Name")),
                MiniSummaryListValue.AnyExpr(FormCtx(FormComponentId("nameField"))),
                None,
                None,
                None
              )
            ),
            DisplayInSummary(IsFalse),
            None
          ),
          None
        ),
        "",
        "",
        0
      ),
      (
        buildFormComponent(
          "miniSummaryList",
          MiniSummaryList(
            List(
              ValueRow(
                Option(toSmartString("Name")),
                MiniSummaryListValue.AnyExpr(FormCtx(FormComponentId("nameField"))),
                Option(IncludeIf(Equals(FormCtx(FormComponentId("nameField")), Constant("nameValue")))),
                None,
                None
              )
            ),
            DisplayInSummary(IsTrue),
            None
          ),
          None
        ),
        "Name",
        "nameValue",
        1
      ),
      (
        buildFormComponent(
          "miniSummaryList",
          MiniSummaryList(
            List(
              ValueRow(
                Option(toSmartString("Name")),
                MiniSummaryListValue.AnyExpr(FormCtx(FormComponentId("nameField"))),
                Option(IncludeIf(Equals(FormCtx(FormComponentId("nameField")), Constant("notTheValue")))),
                None,
                None
              )
            ),
            DisplayInSummary(IsTrue),
            None
          ),
          None
        ),
        "",
        "",
        0
      )
    )

    forAll(table) { (miniSummaryList, key, value, count) =>
      lazy val nameField: FormComponent = buildFormComponent(
        "nameField",
        Value
      )
      val testFixture: TestFixture = new TestFixture {
        override lazy val formTemplate: FormTemplate = buildFormTemplate(
          destinationList,
          sections = List(nonRepeatingPageSection(title = "page1", fields = List(nameField, miniSummaryList)))
        )
        override lazy val form: Form =
          buildForm(
            FormData(
              List(
                FormField(nameField.modelComponentId, "nameValue")
              )
            )
          )
      }
      import testFixture._
      val rows: List[SummaryListRow] =
        FormComponentSummaryRenderer.summaryListRows[SummaryRender](
          miniSummaryList,
          None,
          formTemplate._id,
          formModelOptics.formModelVisibilityOptics,
          None,
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(0)),
          SectionTitle4Ga("page1"),
          NotChecked,
          ValidationResult.empty,
          envelopeWithMapping,
          AddressRecordLookup.from(cache.form.thirdPartyData),
          None,
          Some(List(FastForward.CYA(SectionOrSummary.FormSummary))),
          KeyDisplayWidth.S
        )
      assertEquals(rows.length, count)

      if (rows.nonEmpty) {
        assertEquals(
          rows.head.key.content,
          if (key.isEmpty) { Empty }
          else { Text(key) }
        )
        assertEquals(
          rows.head.value.content,
          if (value.isEmpty) { Empty }
          else { HtmlContent(value) }
        )
      }
    }
  }

  test("summaryListRows should return correct list of summary list rows from informationMessage components") {
    val table = Table(
      ("infoMessage", "key", "value", "count"),
      (
        mkInfoMessageComponent(
          "id1",
          "information text",
          Some(toSmartString("summary information text")),
          toSmartString("keyLabel")
        ),
        "keyLabel",
        "summary information text",
        1
      )
    )

    forAll(table) { (infoMessage, key, value, count) =>
      val testFixture: TestFixture = new TestFixture {
        override lazy val formTemplate: FormTemplate = buildFormTemplate(
          destinationList,
          sections = List(nonRepeatingPageSection(title = "page1", fields = List(infoMessage)))
        )
      }
      import testFixture._
      val rows: List[SummaryListRow] =
        FormComponentSummaryRenderer.summaryListRows[SummaryRender](
          infoMessage,
          None,
          formTemplate._id,
          formModelOptics.formModelVisibilityOptics,
          None,
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(0)),
          SectionTitle4Ga("page1"),
          NotChecked,
          ValidationResult.empty,
          envelopeWithMapping,
          AddressRecordLookup.from(cache.form.thirdPartyData),
          None,
          Some(List(FastForward.CYA(SectionOrSummary.FormSummary))),
          KeyDisplayWidth.S
        )
      assertEquals(rows.length, count)

      if (rows.nonEmpty) {
        assertEquals(
          rows.head.key.content,
          if (key.isEmpty) { Empty }
          else { Text(key) }
        )
        assertEquals(
          rows.head.value.content,
          if (value.isEmpty) { Empty }
          else { HtmlContent(value) }
        )
      }
    }
  }

  def mkInfoMessageComponent(
    id: String,
    infoText: String,
    summaryValue: Option[SmartString] = None,
    label: SmartString = SmartString(LocalisedString(Map()), List())
  ): FormComponent =
    FormComponent(
      FormComponentId(id),
      InformationMessage(
        NoFormat,
        toSmartString(infoText),
        summaryValue
      ),
      label,
      false,
      None,
      None,
      None,
      None,
      Mandatory.True,
      false,
      false,
      false,
      false,
      None,
      None
    )

}
