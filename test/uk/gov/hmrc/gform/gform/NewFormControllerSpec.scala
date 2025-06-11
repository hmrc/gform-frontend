/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.gform

import cats.data.NonEmptyList
import cats.implicits.catsSyntaxApplicativeId
import cats.{ Id, MonadError }
import org.apache.pekko.actor.ActorSystem
import org.mockito.MockitoSugar.when
import org.mockito.{ ArgumentMatchersSugar, IdiomaticMockito }
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.http.{ HttpConfiguration, Status }
import play.api.i18n._
import play.api.mvc.Results.Redirect
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers.{ contentAsString, contentType, defaultAwaitTimeout, redirectLocation, status }
import play.api.{ Configuration, Environment }
import play.mvc.Http.MimeTypes
import uk.gov.hmrc.gform.PlayStubSupport
import uk.gov.hmrc.gform.api.NinoInsightsConnector
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.auth.models.{ MaterialisedRetrievals, OperationWithForm, OperationWithoutForm }
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthCacheWithoutForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.eval.{ EvaluationContext, FileIdsWithMapping }
import uk.gov.hmrc.gform.gformbackend.{ GformBackEndAlgebra, GformConnector }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkFormComponent, mkFormTemplate, mkSection }
import uk.gov.hmrc.gform.graph.{ Recalculation, RecalculationResult }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.objectStore.{ Envelope, ObjectStoreService }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SuppressErrors.{ No, Yes }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileSizeLimit, FormComponentId, FormPhase, FormTemplate, FormTemplateId, FormTemplateVersion, Section, SectionNumber, SectionOrSummary, SectionTitle4Ga, ShortText, SuppressErrors, TemplateSectionIndex, Text, Value }
import uk.gov.hmrc.gform.submission.{ DmsMetaData, Submission, SubmissionId }
import uk.gov.hmrc.gform.typeclasses.identityThrowableMonadError
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import scala.concurrent.{ ExecutionContext, Future }

class NewFormControllerSpec
    extends AnyFlatSpecLike with Matchers with IdiomaticMockito with ArgumentMatchersSugar with FormModelSupport
    with PlayStubSupport with ExampleFrontendAppConfig with ExampleAuthConfig {

  implicit val sys: ActorSystem = ActorSystem("NewFormControllerSpec")

  val newFormUrl: String = "/form/tst1/?n=n1&se=f"

  "downloadOldOrNewForm" should "start a fresh form when no previous form detected" in new TestFixture {
    initCommonMocks()
    when(mockGformConnector.maybeForm(*[FormIdData], *[FormTemplate])(*[HeaderCarrier], *[ExecutionContext]))
      .thenReturn(
        Future.successful(Option.empty[Form]),
        Future.successful(Some(authCacheWithForm.form))
      )

    val result: Future[Result] = newFormController
      .downloadOldOrNewForm(authCacheWithForm.formTemplateId, Yes)
      .apply(request)

    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some(newFormUrl)
  }

  it should "start a fresh form when no previous submission detected" in new TestFixture {
    initCommonMocks()
    when(mockGformConnector.maybeForm(*[FormIdData], *[FormTemplate])(*[HeaderCarrier], *[ExecutionContext]))
      .thenReturn(
        Future.successful(Some(authCacheWithForm.form))
      )
    when(mockGformConnector.submissionDetails(*[FormIdData], *[EnvelopeId])(*[HeaderCarrier], *[ExecutionContext]))
      .thenReturn(Future.successful(Option.empty[Submission]))

    val result: Future[Result] = newFormController
      .downloadOldOrNewForm(authCacheWithForm.formTemplateId, Yes)
      .apply(request)

    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some(newFormUrl)
  }

  it should "start a fresh form when downloadPreviousSubmissionPdf is false" in new TestFixture {
    override lazy val authCacheWithoutForm: AuthCacheWithoutForm =
      mkAuthCacheWithForm(mkFormTemplate(sections).copy(downloadPreviousSubmissionPdf = false)).toAuthCacheWithoutForm

    initCommonMocks()

    when(mockGformConnector.maybeForm(*[FormIdData], *[FormTemplate])(*[HeaderCarrier], *[ExecutionContext]))
      .thenReturn(
        Future.successful(Some(authCacheWithForm.form.copy(status = Submitted))),
        Future.successful(Some(authCacheWithForm.form.copy(status = InProgress)))
      )

    val result: Future[Result] = newFormController
      .downloadOldOrNewForm(authCacheWithForm.formTemplateId, Yes)
      .apply(request)

    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some(newFormUrl)
  }

  it should "ask to download or start new form when previous submission detected" in new TestFixture {
    initCommonMocks()
    when(mockGformConnector.maybeForm(*[FormIdData], *[FormTemplate])(*[HeaderCarrier], *[ExecutionContext]))
      .thenReturn(
        Future.successful(Some(authCacheWithForm.form.copy(status = Submitted)))
      )
    when(mockGformConnector.submissionDetails(*[FormIdData], *[EnvelopeId])(*[HeaderCarrier], *[ExecutionContext]))
      .thenReturn(Future.successful(Some(getSubmission(LocalDateTime.now().minusHours(13)))))

    val result: Future[Result] = newFormController
      .downloadOldOrNewForm(authCacheWithForm.formTemplateId, Yes)
      .apply(request)

    status(result) shouldBe Status.OK
    contentType(result) shouldBe Some(MimeTypes.HTML)
    val html: String = contentAsString(result)
    html should include("What do you want to do?")
    html should include("Get a copy of the form that you submitted")
    html should include("Start a new form")
  }

  it should "ask to download or start new form when previous submission of an earlier form version detected" in new LegacyFormSubmissionFixture {
    initCommonMocks()

    when(mockGformConnector.maybeForm(*[FormIdData], *[FormTemplate])(*[HeaderCarrier], *[ExecutionContext]))
      .thenReturn(
        Future.successful(Some(version1Form))
      )
    when(mockGformConnector.submissionDetails(*[FormIdData], *[EnvelopeId])(*[HeaderCarrier], *[ExecutionContext]))
      .thenReturn(Future.successful(None))
    when(
      mockGformConnector.getSubmissionByLegacyIds(*[FormIdData], *[EnvelopeId])(*[NonEmptyList[FormTemplateId]])(
        *[HeaderCarrier],
        *[ExecutionContext]
      )
    )
      .thenReturn(Future.successful(Some(getSubmission(LocalDateTime.now().minusHours(13)))))

    val result: Future[Result] = newFormController
      .downloadOldOrNewForm(authCacheWithForm.formTemplateId, Yes)
      .apply(request)

    status(result) shouldBe Status.OK
    contentType(result) shouldBe Some(MimeTypes.HTML)
    val html: String = contentAsString(result)
    html should include("What do you want to do?")
    html should include("Get a copy of the form that you submitted")
    html should include("Start a new form")
  }

  it should "start a fresh form when no previous submission for legacy form detected" in new LegacyFormSubmissionFixture {
    initCommonMocks()
    when(mockGformConnector.maybeForm(*[FormIdData], *[FormTemplate])(*[HeaderCarrier], *[ExecutionContext]))
      .thenReturn(
        Future.successful(Some(authCacheWithForm.form)),
        Future.successful(Some(authCacheWithForm.form.copy(status = InProgress)))
      )
    when(mockGformConnector.submissionDetails(*[FormIdData], *[EnvelopeId])(*[HeaderCarrier], *[ExecutionContext]))
      .thenReturn(Future.successful(Option.empty[Submission]))
    when(
      mockGformConnector.getSubmissionByLegacyIds(*[FormIdData], *[EnvelopeId])(*[NonEmptyList[FormTemplateId]])(
        *[HeaderCarrier],
        *[ExecutionContext]
      )
    )
      .thenReturn(Future.successful(Option.empty[Submission]))

    val result: Future[Result] = newFormController
      .downloadOldOrNewForm(authCacheWithForm.formTemplateId, Yes)
      .apply(request)

    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some(newFormUrl)
  }

  "downloadDecision" should "redirect to previous submission page" in new TestFixture {
    override lazy val request: FakeRequest[AnyContent] =
      FakeRequest("POST", "/").withFormUrlEncodedBody("downloadOrNew" -> "download")
    initCommonMocks()

    val result: Future[Result] = newFormController
      .downloadDecision(authCacheWithForm.formTemplateId)
      .apply(request)

    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some("/new-form/tst1/previous-submission/-")
  }

  it should "start a new form" in new TestFixture {
    override lazy val request: FakeRequest[AnyContent] =
      FakeRequest("POST", "/").withFormUrlEncodedBody("downloadOrNew" -> "startNew")
    initCommonMocks()
    when(mockGformConnector.maybeForm(*[FormIdData], *[FormTemplate])(*[HeaderCarrier], *[ExecutionContext]))
      .thenReturn(
        Future.successful(Some(authCacheWithForm.form))
      )

    val result: Future[Result] = newFormController
      .downloadDecision(authCacheWithForm.formTemplateId)
      .apply(request)

    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some(newFormUrl)
  }

  it should "redirect to download or new if no input posted" in new TestFixture {
    override lazy val request: FakeRequest[AnyContent] = FakeRequest("POST", "/")
    initCommonMocks()

    val result: Future[Result] = newFormController
      .downloadDecision(authCacheWithForm.formTemplateId)
      .apply(request)

    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some("/new-form/tst1/new-or-previous?se=f")
  }

  "lastSubmission" should "display page with submission ref and download PDF button" in new TestFixture {
    initCommonMocks()
    when(
      mockGformConnector
        .maybeOneOfSubmissionDetails(*[FormIdData], *[FormIdData], *[EnvelopeId])(*[HeaderCarrier], *[ExecutionContext])
    )
      .thenReturn(
        Future.successful(Some(getSubmission(LocalDateTime.now().minusHours(13))))
      )

    val result: Future[Result] = newFormController
      .lastSubmission(authCacheWithForm.formTemplateId, noAccessCode, Yes)
      .apply(request)

    status(result) shouldBe Status.OK
    contentType(result) shouldBe Some(MimeTypes.HTML)
    val html: String = contentAsString(result)
    html should include("Download a copy of the form that you submitted")
    html should include("Submission reference")
    html should include("Submitted at")
  }

  it should "display page with submission ref and download PDF button when legacy form" in new LegacyFormSubmissionFixture {
    initCommonMocks()
    when(
      mockGformConnector
        .maybeOneOfSubmissionDetails(*[FormIdData], *[FormIdData], *[EnvelopeId])(*[HeaderCarrier], *[ExecutionContext])
    )
      .thenReturn(
        Future.successful(Some(getSubmission(LocalDateTime.now().minusHours(13))))
      )

    val result: Future[Result] = newFormController
      .lastSubmission(authCacheWithForm.formTemplateId, noAccessCode, Yes)
      .apply(request)

    status(result) shouldBe Status.OK
    contentType(result) shouldBe Some(MimeTypes.HTML)
    val html: String = contentAsString(result)
    html should include("Download a copy of the form that you submitted")
    html should include("Submission reference")
    html should include("Submitted at")
  }

  it should "start a new form when no previous form submission detected" in new TestFixture {
    initCommonMocks()
    mockAuth
      .authAndRetrieveForm[SectionSelectorType.Normal](
        *[FormTemplateId],
        *[Option[AccessCode]],
        *[OperationWithForm],
        *[Option[SectionOrSummary]]
      )(
        *[Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[
          DataOrigin.Mongo
        ] => Future[Result]]
      ) answers {
      (
        _: FormTemplateId,
        _: Option[AccessCode],
        _: OperationWithForm,
        _: Option[SectionOrSummary],
        f: Request[
          AnyContent
        ] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[DataOrigin.Mongo] => Future[
          Result
        ]
      ) =>
        messagesControllerComponents.actionBuilder.async { _ =>
          Redirect(newFormUrl).pure[Future]
        }
    }

    val result: Future[Result] = newFormController
      .lastSubmission(authCacheWithForm.formTemplateId, noAccessCode, Yes)
      .apply(request)

    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some(newFormUrl)
  }

  it should "ask to start new or continue form when downloadPreviousSubmissionPdf is false" in new TestFixture {
    override lazy val authCacheWithForm: AuthCacheWithForm =
      mkAuthCacheWithForm(mkFormTemplate(sections).copy(downloadPreviousSubmissionPdf = false))

    initCommonMocks()
    when(
      mockGformConnector
        .maybeOneOfSubmissionDetails(*[FormIdData], *[FormIdData], *[EnvelopeId])(*[HeaderCarrier], *[ExecutionContext])
    ).thenReturn(
      Future.successful(Some(getSubmission(LocalDateTime.now().minusHours(13))))
    )

    val result: Future[Result] = newFormController
      .lastSubmission(authCacheWithForm.formTemplateId, noAccessCode, Yes)
      .apply(request)

    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some("/new-form/tst1/one-per-user")
  }

  it should "ask to start new or continue form when form that's not submitted detected" in new TestFixture {
    initCommonMocks()
    when(
      mockGformConnector
        .maybeOneOfSubmissionDetails(*[FormIdData], *[FormIdData], *[EnvelopeId])(*[HeaderCarrier], *[ExecutionContext])
    ).thenReturn(
      Future.successful(Option.empty[Submission])
    )

    val result: Future[Result] = newFormController
      .lastSubmission(authCacheWithForm.formTemplateId, noAccessCode, Yes)
      .apply(request)

    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some("/new-form/tst1/one-per-user")
  }

  "newOrSignout" should "redirect to sign out page" in new TestFixture {
    override lazy val request: FakeRequest[AnyContent] =
      FakeRequest("POST", "/").withFormUrlEncodedBody("downloadThenNew" -> "signOut")
    initCommonMocks()

    val result: Future[Result] = newFormController
      .newOrSignout(authCacheWithForm.formTemplateId, noAccessCode)
      .apply(request)

    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some("/sign-out/tst1")
  }

  it should "start a new form" in new TestFixture {
    override lazy val request: FakeRequest[AnyContent] =
      FakeRequest("POST", "/").withFormUrlEncodedBody("downloadThenNew" -> "startNew")
    initCommonMocks()

    when(mockGformConnector.maybeForm(*[FormIdData], *[FormTemplate])(*[HeaderCarrier], *[ExecutionContext]))
      .thenReturn(
        Future.successful(Some(authCacheWithForm.form))
      )

    val result: Future[Result] = newFormController
      .newOrSignout(authCacheWithForm.formTemplateId, noAccessCode)
      .apply(request)

    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some(newFormUrl)
  }

  it should "redirect to last submission if no input posted" in new TestFixture {
    override lazy val request: FakeRequest[AnyContent] = FakeRequest("POST", "/")
    initCommonMocks()

    val result: Future[Result] = newFormController
      .newOrSignout(authCacheWithForm.formTemplateId, noAccessCode)
      .apply(request)

    status(result) shouldBe Status.SEE_OTHER
    redirectLocation(result) shouldBe Some("/new-form/tst1/previous-submission/-?se=f")
  }

  trait TestFixture {

    lazy val environment: Environment = Environment.simple()
    lazy val configuration: Configuration = Configuration.load(environment)
    lazy val langs: Langs = new DefaultLangs()
    lazy val httpConfiguration: HttpConfiguration = HttpConfiguration.fromConfiguration(configuration, environment)
    lazy val localMessagesApi: MessagesApi =
      new DefaultMessagesApiProvider(environment, configuration, langs, httpConfiguration).get
    lazy val i18nSupport: I18nSupport = new I18nSupport {
      override def messagesApi: MessagesApi = localMessagesApi
    }

    implicit lazy val messages: Messages = i18nSupport.messagesApi.preferred(Seq(langs.availables.head))
    implicit lazy val lang: LangADT = LangADT.En
    implicit lazy val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
    implicit lazy val hc: HeaderCarrier = HeaderCarrier()

    lazy val submissionRef: SubmissionRef = SubmissionRef("some-submission-ref")
    lazy val request: FakeRequest[AnyContent] = FakeRequest("GET", "/")
    lazy val noAccessCode = Option.empty[AccessCode]

    lazy val sections: List[Section] =
      mkSection(
        mkFormComponent("a", Text(ShortText.default, Value)) :: Nil
      ) :: mkSection(
        mkFormComponent("b", Text(ShortText.default, Value)) :: Nil
      ) :: Nil

    lazy val formTemplate: FormTemplate = mkFormTemplate(sections)
    lazy val authCacheWithForm: AuthCacheWithForm = mkAuthCacheWithForm(formTemplate)
    lazy val authCacheWithoutForm: AuthCacheWithoutForm = authCacheWithForm.toAuthCacheWithoutForm
    lazy val variadicFormData: VariadicFormData[SourceOrigin.OutOfDate] = VariadicFormData.empty[SourceOrigin.OutOfDate]
    lazy val mockRecalculation: Recalculation[Future, Throwable] = mock[Recalculation[Future, Throwable]]
    lazy val formModelOptics: FormModelOptics[DataOrigin.Mongo] =
      FormModelOptics
        .mkFormModelOptics[DataOrigin.Mongo, Id, SectionSelectorType.Normal](
          variadicFormData,
          authCacheWithForm,
          recalculation
        )

    lazy val messagesControllerComponents: MessagesControllerComponents = stubMessagesControllerComponents()

    lazy val mockAuth: AuthenticatedRequestActions = mock[AuthenticatedRequestActions]
    lazy val mockObjectStoreService: ObjectStoreService = mock[ObjectStoreService]
    lazy val mockGformConnector: GformConnector = mock[GformConnector]
    lazy val mockFastForwardService: FastForwardService = mock[FastForwardService]
    lazy val mockAuditService: AuditService = mock[AuditService]
    lazy val mockGformBackend: GformBackEndAlgebra[Future] = mock[GformBackEndAlgebra[Future]]
    lazy val mockNinoInsightsConnector: NinoInsightsConnector[Future] = mock[NinoInsightsConnector[Future]]
    lazy val mockAcknowledgementPdfService: AcknowledgementPdfService = mock[AcknowledgementPdfService]

    implicit lazy val smartStringEvaluator: SmartStringEvaluator = new SmartStringEvaluator {
      override def apply(s: SmartString, markDown: Boolean): String = s.rawDefaultValue(LangADT.En)
      override def evalEnglish(s: SmartString, markDown: Boolean): String = s.rawDefaultValue(LangADT.En)
    }

    lazy val newFormController =
      new NewFormController(
        AppConfig.loadOrThrow(),
        frontendAppConfig,
        i18nSupport,
        mockAuth,
        mockObjectStoreService,
        mockGformConnector,
        mockFastForwardService,
        mockAuditService,
        mockRecalculation,
        messagesControllerComponents,
        mockGformBackend,
        mockNinoInsightsConnector,
        messages,
        mockAcknowledgementPdfService
      )

    private def toFormFields(xs: List[(String, String)]): List[FormField] = xs.map { case (fcId, value) =>
      FormField(FormComponentId(fcId).modelComponentId, value)
    }

    lazy val formFields: List[FormField] = toFormFields(
      List(
        "a" -> "one",
        "b" -> "two"
      )
    )

    def getSubmission(submittedAt: LocalDateTime): Submission = Submission(
      _id = SubmissionId(authCacheWithForm.form._id, envelopeId),
      submittedDate = submittedAt,
      submissionRef = submissionRef,
      envelopeId = envelopeId,
      dmsMetaData = DmsMetaData(authCacheWithForm.formTemplateId)
    )

    def initCommonMocks(): Unit = {
      mockAuth
        .authAndRetrieveForm[SectionSelectorType.Normal](
          *[FormTemplateId],
          *[Option[AccessCode]],
          *[OperationWithForm],
          *[Option[SectionOrSummary]]
        )(
          *[Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[
            DataOrigin.Mongo
          ] => Future[Result]]
        ) answers {
        (
          _: FormTemplateId,
          _: Option[AccessCode],
          _: OperationWithForm,
          _: Option[SectionOrSummary],
          f: Request[
            AnyContent
          ] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[DataOrigin.Mongo] => Future[
            Result
          ]
        ) =>
          messagesControllerComponents.actionBuilder.async { request =>
            f(request)(LangADT.En)(authCacheWithForm)(smartStringEvaluator)(formModelOptics)
          }
      }

      mockAuth.authWithoutRetrievingForm(*[FormTemplateId], *[OperationWithoutForm])(
        *[Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]]
      ) answers {
        (
          _: FormTemplateId,
          _: OperationWithoutForm,
          f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]
        ) =>
          messagesControllerComponents.actionBuilder.async { request =>
            f(request)(LangADT.En)(authCacheWithoutForm)
          }
      }

      mockRecalculation.recalculateFormDataNew(
        *[VariadicFormData[SourceOrigin.OutOfDate]],
        *[FormModel[Interim]],
        *[FormTemplate],
        *[MaterialisedRetrievals],
        *[ThirdPartyData],
        *[EvaluationContext],
        *[Messages]
      )(*[MonadError[Future, Throwable]]) returns Future.successful(
        RecalculationResult.empty(
          EvaluationContext(
            authCacheWithForm.formTemplateId,
            submissionRef,
            maybeAccessCode,
            retrievals,
            ThirdPartyData.empty,
            authConfig,
            hc,
            Option.empty[FormPhase],
            FileIdsWithMapping.empty,
            Map.empty,
            Map.empty,
            Set.empty,
            Set.empty,
            Set.empty,
            Map.empty,
            LangADT.En,
            messages,
            Map.empty,
            Set.empty,
            FileSizeLimit(1),
            DataRetrieveAll.empty,
            Set.empty[ModelComponentId],
            Map.empty,
            Set.empty,
            new LookupRegistry(Map()),
            Map.empty,
            Map.empty,
            TaskIdTaskStatusMapping.empty
          )
        )
      )

      when(mockObjectStoreService.getEnvelope(*[EnvelopeId])(*[HeaderCarrier]))
        .thenReturn(Future.successful(Envelope.empty))

      when(
        mockFastForwardService.redirectFastForward[SectionSelectorType.Normal](
          *[AuthCacheWithForm],
          *[Option[AccessCode]],
          *[FormModelOptics[DataOrigin.Mongo]],
          *[Option[SectionNumber]],
          *[SuppressErrors],
          *[List[FastForward]]
        )(
          *[SectionSelector[SectionSelectorType.Normal]],
          *[Messages],
          *[HeaderCarrier],
          *[LangADT]
        )
      ).thenReturn(
        Future.successful(
          Redirect(
            routes.FormController
              .form(
                authCacheWithForm.formTemplateId,
                maybeAccessCode,
                SectionNumber.Classic.NormalPage(TemplateSectionIndex(1)),
                SectionTitle4Ga(""),
                No,
                List.empty[FastForward]
              )
          )
        )
      )

      when(
        mockGformBackend.newForm(*[FormTemplateId], *[MaterialisedRetrievals], *[QueryParams])(
          *[HeaderCarrier]
        )
      ).thenReturn(Future.successful(FormIdData.apply(authCacheWithForm, None)))

      when(
        mockAcknowledgementPdfService.getRenderedPdfSize(
          *[AuthCacheWithForm],
          *[Option[AccessCode]],
          *[FormModelOptics[DataOrigin.Mongo]]
        )(*[Request[_]], *[LangADT], *[SmartStringEvaluator])
      ).thenReturn(Future.successful(35000))

      when(
        mockGformConnector.maybeFormTemplate(*[FormTemplateId])(
          *[HeaderCarrier],
          *[ExecutionContext]
        )
      ).thenReturn(Future.successful(Some(authCacheWithForm.formTemplate)))

      ()
    }
  }

  trait LegacyFormSubmissionFixture extends TestFixture {
    override lazy val formTemplate: FormTemplate =
      mkFormTemplate(sections).copy(
        version = FormTemplateVersion(2),
        legacyFormIds = Some(NonEmptyList.of(FormTemplateId("tst1-v1")))
      )

    val version1Form: Form = mkForm(formTemplate._id).copy(
      formTemplateId = FormTemplateId("tst1"),
      status = Submitted,
      formTemplateVersion = Some(FormTemplateVersion(1))
    )
    override lazy val authCacheWithForm: AuthCacheWithForm = mkAuthCacheWithForm(formTemplate).copy(
      form = version1Form
    )

    override lazy val authCacheWithoutForm: AuthCacheWithoutForm = authCacheWithForm.toAuthCacheWithoutForm
  }
}
