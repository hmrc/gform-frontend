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

import cats.Id
import org.apache.pekko.actor.ActorSystem
import org.mockito.MockitoSugar.when
import org.mockito.{ ArgumentMatchersSugar, IdiomaticMockito }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import play.api.http.HttpConfiguration
import play.api.i18n._
import play.api.libs.json.{ Json, Reads }
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.{ Configuration, Environment }
import uk.gov.hmrc.gform.PlayStubSupport
import uk.gov.hmrc.gform.api.NinoInsightsConnector
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.auth.models.{ OperationWithForm, OperationWithoutForm }
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthCacheWithoutForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.gformbackend.{ GformBackEndAlgebra, GformConnector }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkFormComponent, mkFormTemplate, mkSection }
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ FormModelSupport, SectionSelectorType, VariadicFormDataSupport }
import uk.gov.hmrc.gform.objectStore.ObjectStoreService
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormIdData, FormModelOptics, QueryParams }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SuppressErrors.Yes
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId, Section, ShortText, Text, Value }
import uk.gov.hmrc.gform.typeclasses.identityThrowableMonadError
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ Await, ExecutionContext, Future }

class NewFormControllerSpec
    extends AnyFlatSpecLike with Matchers with IdiomaticMockito with ArgumentMatchersSugar with ScalaFutures
    with FormModelSupport with VariadicFormDataSupport with PlayStubSupport {

  implicit val sys: ActorSystem = ActorSystem("NewFormControllerSpec")

  "downloadOldOrNewForm" should "start a fresh form when no previous submission detected" in new TestFixture {

    when(mockGformConnector.maybeForm(*[FormIdData], *[FormTemplate])(*[HeaderCarrier], *[ExecutionContext]))
      .thenReturn(Future.successful(Option.empty[Form]))
    when(mockGformConnector.newForm(*[FormTemplateId], *[UserId], *[Option[AffinityGroup]], *[QueryParams]))
      .thenReturn()

    val future = newFormController
      .downloadOldOrNewForm(authCacheWithForm.formTemplateId, Yes)
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("United Kingdom", "United States")
    }
  }

  private def responseBodyAs[T](result: Result)(implicit reads: Reads[T]): T =
    Json.parse(Await.result(result.body.consumeData, 5.seconds).decodeString("utf-8")).as[T]

  trait TestFixture {

    private val environment: Environment = Environment.simple()
    private val configuration: Configuration = Configuration.load(environment)
    private val langs: Langs = new DefaultLangs()
    private val httpConfiguration: HttpConfiguration = HttpConfiguration.fromConfiguration(configuration, environment)
    private val localMessagesApi: MessagesApi =
      new DefaultMessagesApiProvider(environment, configuration, langs, httpConfiguration).get
    private val i18nSupport: I18nSupport = new I18nSupport {
      override def messagesApi: MessagesApi = localMessagesApi
    }

    implicit val messages: Messages = i18nSupport.messagesApi.preferred(Seq(langs.availables.head))
    implicit val lang: LangADT = LangADT.En

    val request: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

    lazy val sections: List[Section] =
      mkSection(
        mkFormComponent("a", Text(ShortText.default, Value)) :: Nil
      ) :: mkSection(
        mkFormComponent("b", Text(ShortText.default, Value)) :: Nil
      ) :: Nil
    lazy val authCacheWithForm: AuthCacheWithForm = mkAuthCacheWithForm(mkFormTemplate(sections))
    lazy val authCacheWithoutForm: AuthCacheWithoutForm = authCacheWithForm.toAuthCacheWithoutForm
    lazy val variadicFormData: VariadicFormData[SourceOrigin.OutOfDate] = VariadicFormData.empty[SourceOrigin.OutOfDate]
    lazy val formModelOptics: FormModelOptics[DataOrigin.Mongo] =
      FormModelOptics
        .mkFormModelOptics[DataOrigin.Mongo, Id, SectionSelectorType.Normal](
          variadicFormData,
          authCacheWithForm,
          recalculation
        )

    val messagesControllerComponents: MessagesControllerComponents = stubMessagesControllerComponents()

    val mockAuth: AuthenticatedRequestActions = mock[AuthenticatedRequestActions]
    val mockConfig: FrontendAppConfig = mock[FrontendAppConfig]
    val mockObjectStoreService: ObjectStoreService = mock[ObjectStoreService]
    val mockGformConnector: GformConnector = mock[GformConnector]
    val mockFastForwardService: FastForwardService = mock[FastForwardService]
    val mockAuditService: AuditService = mock[AuditService]
    val mockRecalculation: Recalculation[Future, Throwable] = mock[Recalculation[Future, Throwable]]
    val mockGformBackend: GformBackEndAlgebra[Future] = mock[GformBackEndAlgebra[Future]]
    val mockNinoInsightsConnector: NinoInsightsConnector[Future] = mock[NinoInsightsConnector[Future]]
    val mockAcknowledgementPdfService: AcknowledgementPdfService = mock[AcknowledgementPdfService]

    implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
    implicit val hc: HeaderCarrier = HeaderCarrier()

    implicit val smartStringEvaluator: SmartStringEvaluator = new SmartStringEvaluator {
      override def apply(s: SmartString, markDown: Boolean): String = s.rawDefaultValue(LangADT.En)
      override def evalEnglish(s: SmartString, markDown: Boolean): String = s.rawDefaultValue(LangADT.En)
    }

    mockAuth
      .authAndRetrieveForm[SectionSelectorType.Normal](*[FormTemplateId], *[Option[AccessCode]], *[OperationWithForm])(
        *[Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[
          DataOrigin.Mongo
        ] => Future[Result]]
      ) answers {
      (
        _: FormTemplateId,
        _: Option[AccessCode],
        _: OperationWithForm,
        f: Request[
          AnyContent
        ] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[DataOrigin.Mongo] => Future[Result]
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

    lazy val newFormController =
      new NewFormController(
        mockConfig,
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
  }
}
