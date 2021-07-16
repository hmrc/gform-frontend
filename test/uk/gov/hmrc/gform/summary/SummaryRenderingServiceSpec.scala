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

package uk.gov.hmrc.gform.summary

import cats.MonadError
import org.jsoup.Jsoup
import org.mockito.ArgumentMatchersSugar
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Span }
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.test.{ FakeRequest, Helpers }
import play.twirl.api.Html
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, MaterialisedRetrievals, Role }
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, CacheData }
import uk.gov.hmrc.gform.eval.{ EvaluationContext, FileIdsWithMapping }
import uk.gov.hmrc.gform.eval.smartstring.{ RealSmartStringEvaluatorFactory, SmartStringEvaluator }
import uk.gov.hmrc.gform.fileupload.{ Envelope, EnvelopeWithMapping, FileUploadAlgebra }
import uk.gov.hmrc.gform.gform.SummaryPagePurpose
import uk.gov.hmrc.gform.graph.{ Recalculation, RecalculationResult }
import uk.gov.hmrc.gform.models.{ FormModel, Interim, SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormData, FormField, FormModelOptics, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormPhase
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.PrintSection
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.PrintSection.PdfNotification
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, FormComponent, FormTemplate, InvisibleInSummary, InvisiblePageTitle, Value }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, ExampleData, LangADT, PdfHtml, SourceOrigin, SubmissionRef, VariadicFormData }
import uk.gov.hmrc.gform.summary.HtmlSupport._
import uk.gov.hmrc.gform.validation.HtmlFieldId.Indexed
import uk.gov.hmrc.gform.validation.{ ComponentField, FieldOk, ValidationResult, ValidationService }
import uk.gov.hmrc.http.{ HeaderCarrier, SessionId }
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }

import scala.collection.immutable.List
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SummaryRenderingServiceSpec
    extends AnyWordSpecLike with Matchers with ScalaFutures with ExampleData with ArgumentMatchersSugar
    with IdiomaticMockito with HtmlSupport {

  override implicit val patienceConfig =
    PatienceConfig(timeout = scaled(Span(10000, Millis)), interval = scaled(Span(15, Millis)))

  trait TestFixture {
    implicit val request = FakeRequest()
    implicit val headerCarrier = HeaderCarrier()
    implicit val langADT = LangADT.En
    lazy val i18nSupport: I18nSupport = new I18nSupport {
      override def messagesApi: MessagesApi =
        Helpers.stubMessagesApi(Map("en" -> Map("summary.acknowledgement.pdf" -> "Acknowledgement PDF")))
    }
    implicit val messages: Messages = i18nSupport.request2Messages
    lazy val form: Form = buildForm
    lazy val formTemplate: FormTemplate = buildFormTemplate
    lazy val addToListQuestionComponent = addToListQuestion("addToListQuestion")
    lazy val page1Field = buildFormComponent("page1Field", Value)
    lazy val page2Field = buildFormComponent("page2Field", Value)

    val submissionRef = SubmissionRef("some-submission-ref")
    val retrievals = AnonymousRetrievals(SessionId("session-id"))
    val maybeAccessCode = Some(AccessCode("some-access-code"))
    val cache = AuthCacheWithForm(retrievals, form, formTemplate, Role.Customer, maybeAccessCode)
    lazy val validationResult = ValidationResult.empty

    val mockFileUploadService = mock[FileUploadAlgebra[Future]]
    val mockValidationService = mock[ValidationService]
    val mockRecalculation = mock[Recalculation[Future, Throwable]]

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
          LangADT.En,
          messages
        )
      )
    )

    val formModelOptics: FormModelOptics[DataOrigin.Mongo] = FormModelOptics
      .mkFormModelOptics[DataOrigin.Mongo, Future, SectionSelectorType.WithDeclaration](
        cache.variadicFormData[SectionSelectorType.WithDeclaration],
        cache,
        mockRecalculation
      )
      .futureValue
    implicit val smartStringEvaluator: SmartStringEvaluator = new RealSmartStringEvaluatorFactory()
      .apply(formModelOptics.formModelVisibilityOptics, retrievals, maybeAccessCode, form, formTemplate)

    val summaryRenderingService =
      new SummaryRenderingService(i18nSupport, mockFileUploadService, mockValidationService, frontendAppConfig)
  }

  "createHtmlForPdf" should {

    "should have title with 'Form Summary' prefix and form template name" in new TestFixture {
      implicit val sectionSelectorType: SectionSelector[SectionSelectorType.Normal] = SectionSelector.normal

      val pdfHtml: PdfHtml =
        summaryRenderingService
          .createHtmlForPdf(maybeAccessCode, cache, None, SummaryPagePurpose.ForDms, formModelOptics)
          .futureValue

      Html(pdfHtml.html).title shouldBe "Acknowledgement PDF - AAA999 dev test template"
    }

    "hide/show page title based on presentationHint" in {
      implicit val sectionSelectorType: SectionSelector[SectionSelectorType.Normal] = SectionSelector.normal

      val table = Table(
        ("presentationHint", "summaryElements"),
        (
          None,
          List(
            HeaderElement("Some page title"),
            SummaryListElement(List(SummaryListRow("page1Field", "page1Field-value"))),
            HeaderElement("declaration section"),
            SummaryListElement(List(SummaryListRow("fieldInDeclarationSections", ""))),
            SummaryListElement(List())
          )
        ),
        (
          Some(InvisibleInSummary),
          List(
            HeaderElement("Some page title"),
            SummaryListElement(List(SummaryListRow("page1Field", "page1Field-value"))),
            HeaderElement("declaration section"),
            SummaryListElement(List(SummaryListRow("fieldInDeclarationSections", ""))),
            SummaryListElement(List())
          )
        ),
        (
          Some(InvisiblePageTitle),
          List(
            SummaryListElement(List(SummaryListRow("page1Field", "page1Field-value"))),
            HeaderElement("declaration section"),
            SummaryListElement(List(SummaryListRow("fieldInDeclarationSections", ""))),
            SummaryListElement(List())
          )
        )
      )

      forAll(table) { (presentationHint, expectedSummaryElements) =>
        val testFixture: TestFixture = new TestFixture {
          override lazy val formTemplate: FormTemplate = buildFormTemplate(
            destinationList,
            List(
              nonRepeatingPageSection(
                title = "Some page title",
                fields = List(page1Field),
                presentationHint = presentationHint
              )
            )
          )
          override lazy val form: Form =
            buildForm(FormData(List(FormField(page1Field.modelComponentId, "page1Field-value"))))
          override lazy val validationResult: ValidationResult = new ValidationResult(
            Map(
              page1Field.id -> FieldOk(page1Field, "page1Field-value")
            ),
            None
          )
        }
        import testFixture._
        val pdfHtml = summaryRenderingService
          .createHtmlForPdf(maybeAccessCode, cache, None, SummaryPagePurpose.ForDms, formModelOptics)
          .futureValue
        pdfHtml.summaryElements shouldBe expectedSummaryElements
      }
    }
  }

  "createHtmlForPrintPdf" should {
    "should have title with 'Form Summary' prefix and form template name" in new TestFixture {

      val pdfHtml: PdfHtml =
        summaryRenderingService
          .createHtmlForPrintPdf(
            maybeAccessCode,
            cache,
            SummaryPagePurpose.ForDms,
            PrintSection.Pdf(toSmartString("header"), toSmartString("footer")),
            formModelOptics
          )
          .futureValue

      Html(pdfHtml.html).title shouldBe "Acknowledgement PDF - AAA999 dev test template"
    }
  }

  "createHtmlForNotificationPdf" should {
    "should have title with 'Form Summary' prefix and form template name" in new TestFixture {

      val pdfHtml: PdfHtml =
        summaryRenderingService
          .createHtmlForNotificationPdf(
            maybeAccessCode,
            cache,
            SummaryPagePurpose.ForDms,
            PdfNotification(toSmartString("header"), toSmartString("footer"), List.empty),
            formModelOptics
          )
          .futureValue

      Html(pdfHtml.html).title shouldBe "Acknowledgement PDF - AAA999 dev test template"
    }
  }

  "getSummaryHTML" when {

    "summary page" should {

      "show a Button with text 'Summary ContinueLabel'" in {
        val testFixture: TestFixture = new TestFixture {
          override lazy val formTemplate: FormTemplate = buildFormTemplate(
            destinationList,
            List(
              nonRepeatingPageSection(
                title = "Some page title",
                fields = List(page1Field),
                presentationHint = None
              )
            )
          )
          override lazy val form: Form =
            buildForm(FormData(List(FormField(page1Field.modelComponentId, "page1Field-value"))))
          override lazy val validationResult: ValidationResult = new ValidationResult(
            Map(
              page1Field.id -> FieldOk(page1Field, "page1Field-value")
            ),
            None
          )
        }

        import testFixture._

        val generatedHtml = summaryRenderingService
          .getSummaryHTML(maybeAccessCode, cache, SummaryPagePurpose.ForDms, formModelOptics)
          .futureValue

        val pageButton = Jsoup.parse(generatedHtml.body).getElementsByClass("govuk-button").first

        pageButton.text shouldBe "Summary ContinueLabel"
      }
    }

    "non-repeating page" should {

      "hide/show page title based on presentationHint" in {
        val table = Table(
          ("presentationHint", "summaryElements"),
          (
            None,
            List(
              HeaderElement("Some page title"),
              SummaryListElement(List(SummaryListRow("page1Field", "page1Field-value"))),
              HeaderElement("declaration section"),
              SummaryListElement(List(SummaryListRow("fieldInDeclarationSections", "")))
            )
          ),
          (
            Some(InvisibleInSummary),
            List(
              HeaderElement("Some page title"),
              SummaryListElement(List(SummaryListRow("page1Field", "page1Field-value"))),
              HeaderElement("declaration section"),
              SummaryListElement(List(SummaryListRow("fieldInDeclarationSections", "")))
            )
          ),
          (
            Some(InvisiblePageTitle),
            List(
              SummaryListElement(List(SummaryListRow("page1Field", "page1Field-value"))),
              HeaderElement("declaration section"),
              SummaryListElement(List(SummaryListRow("fieldInDeclarationSections", "")))
            )
          )
        )

        forAll(table) { (presentationHint, expectedSummaryElements) =>
          val testFixture: TestFixture = new TestFixture {
            override lazy val formTemplate: FormTemplate = buildFormTemplate(
              destinationList,
              List(
                nonRepeatingPageSection(
                  title = "Some page title",
                  fields = List(page1Field),
                  presentationHint = presentationHint
                )
              )
            )
            override lazy val form: Form =
              buildForm(FormData(List(FormField(page1Field.modelComponentId, "page1Field-value"))))
            override lazy val validationResult: ValidationResult = new ValidationResult(
              Map(
                page1Field.id -> FieldOk(page1Field, "page1Field-value")
              ),
              None
            )
          }
          import testFixture._
          summaryRenderingService
            .getSummaryHTML(maybeAccessCode, cache, SummaryPagePurpose.ForDms, formModelOptics)
            .futureValue
            .summaryElements shouldBe expectedSummaryElements
        }
      }
    }

    "repeating page" should {

      "hide page title when presentationHint is InvisiblePageTitle" in {

        val table = Table(
          ("presentationHint", "summaryElements"),
          (
            None,
            List(
              HeaderElement("Some page title"),
              SummaryListElement(List(SummaryListRow("page1Field", "page1Field-value"))),
              HeaderElement("declaration section"),
              SummaryListElement(List(SummaryListRow("fieldInDeclarationSections", "")))
            )
          ),
          (
            Some(InvisibleInSummary),
            List(
              HeaderElement("Some page title"),
              SummaryListElement(List(SummaryListRow("page1Field", "page1Field-value"))),
              HeaderElement("declaration section"),
              SummaryListElement(List(SummaryListRow("fieldInDeclarationSections", "")))
            )
          ),
          (
            Some(InvisiblePageTitle),
            List(
              SummaryListElement(List(SummaryListRow("page1Field", "page1Field-value"))),
              HeaderElement("declaration section"),
              SummaryListElement(List(SummaryListRow("fieldInDeclarationSections", "")))
            )
          )
        )

        forAll(table) { (presentationHint, expectedSummaryElements) =>
          val testFixture: TestFixture = new TestFixture {
            override lazy val formTemplate: FormTemplate = buildFormTemplate(
              destinationList,
              List(
                repeatingSection(
                  title = "Some page title",
                  fields = List(page1Field),
                  repeatsExpr = Constant("1"),
                  presentationHint = presentationHint
                )
              )
            )
            override lazy val form: Form =
              buildForm(FormData(List(FormField(page1Field.withIndex(1).modelComponentId, "page1Field-value"))))
            override lazy val validationResult: ValidationResult = new ValidationResult(
              Map(
                page1Field.withIndex(1).id -> FieldOk(page1Field.withIndex(1), "page1Field-value")
              ),
              None
            )
          }
          import testFixture._
          summaryRenderingService
            .getSummaryHTML(maybeAccessCode, cache, SummaryPagePurpose.ForDms, formModelOptics)
            .futureValue
            .summaryElements shouldBe expectedSummaryElements
        }
      }
    }

    "add to list - presentationHint is InvisiblePageTitle" should {
      "render elements without page titles" in new TestFixture {

        override lazy val formTemplate: FormTemplate = buildFormTemplate(
          destinationList,
          List(
            addToListSection(
              "addToList",
              "addToList",
              "addToList",
              "addToListSummary",
              addToListQuestionComponent,
              None,
              List(
                toPage("page1", None, List(page1Field)),
                toPage("page2", None, List(page2Field))
              ),
              Some(InvisiblePageTitle)
            )
          )
        )

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

        val html: Html =
          summaryRenderingService
            .getSummaryHTML(maybeAccessCode, cache, SummaryPagePurpose.ForDms, formModelOptics)
            .futureValue

        html.summaryElements shouldBe List(
          HeaderElement("addToListSummary"),
          SummaryListElement(List(SummaryListRow("addToList", "addToList addToList"))),
          HeaderElement("addToList"),
          SummaryListElement(List(SummaryListRow("page1Field", "page1Field-value1"))),
          SummaryListElement(List(SummaryListRow("page2Field", "page2Field-value1"))),
          HeaderElement("addToList"),
          SummaryListElement(List(SummaryListRow("page1Field", "page1Field-value2"))),
          SummaryListElement(List(SummaryListRow("page2Field", "page2Field-value2"))),
          HeaderElement("declaration section"),
          SummaryListElement(List(SummaryListRow("fieldInDeclarationSections", "")))
        )
      }
    }

    "add to list - presentationHint is None" should {
      "render elements with page titles" in new TestFixture {

        override lazy val formTemplate: FormTemplate = buildFormTemplate(
          destinationList,
          List(
            addToListSection(
              "addToList",
              "addToListDesc",
              "addToList",
              "addToListSummary",
              addToListQuestionComponent,
              None,
              List(
                toPage("page1", None, List(page1Field)),
                toPage("page2", None, List(page2Field))
              ),
              None
            )
          )
        )

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

        val html: Html =
          summaryRenderingService
            .getSummaryHTML(maybeAccessCode, cache, SummaryPagePurpose.ForDms, formModelOptics)
            .futureValue

        html.summaryElements shouldBe List(
          HeaderElement("addToListSummary"),
          SummaryListElement(List(SummaryListRow("addToList", "addToListDesc addToListDesc"))),
          HeaderElement("addToList"),
          HeaderElement("page1"),
          SummaryListElement(List(SummaryListRow("page1Field", "page1Field-value1"))),
          HeaderElement("page2"),
          SummaryListElement(List(SummaryListRow("page2Field", "page2Field-value1"))),
          HeaderElement("addToList"),
          HeaderElement("page1"),
          SummaryListElement(List(SummaryListRow("page1Field", "page1Field-value2"))),
          HeaderElement("page2"),
          SummaryListElement(List(SummaryListRow("page2Field", "page2Field-value2"))),
          HeaderElement("declaration section"),
          SummaryListElement(List(SummaryListRow("fieldInDeclarationSections", "")))
        )
      }
    }
  }

  implicit class FormComponentOps(formComponent: FormComponent) {
    def withIndex(index: Int) =
      formComponent.copy(id = formComponent.id.copy(value = index + "_" + formComponent.id.value))
  }
}
