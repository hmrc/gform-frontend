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

package uk.gov.hmrc.gform.gform

import org.apache.pekko.actor.ActorSystem
import org.mockito.{ ArgumentMatchersSugar, IdiomaticMockito }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.libs.json.{ Json, Reads }
import play.api.mvc.{ AnyContent, MessagesControllerComponents, Request, Result }
import play.api.test.{ FakeRequest, Helpers }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActionsAlgebra }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkFormComponent, mkFormTemplate, mkSection }
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ FormModelSupport, LookupQuery, SectionSelectorType, VariadicFormDataSupport }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SelectionCriteriaValue.{ SelectionCriteriaExpr, SelectionCriteriaReference, SelectionCriteriaSimpleValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ CsvColumnName, FormComponentId, FormCtx, FormTemplateId, Lookup, Register, Section, SelectionCriteria, Text, Value }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.{ LookupLoader, PlayStubSupport }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

class LookupControllerSpec
    extends AnyFlatSpecLike with Matchers with IdiomaticMockito with ArgumentMatchersSugar with ScalaFutures
    with FormModelSupport with VariadicFormDataSupport with PlayStubSupport {

  implicit val sys: ActorSystem = ActorSystem("LookupControllerSpec")

  "lookupWithSelectionCriteria - showAll enabled" should "lookup options when no query provided and no selection criteria provided" in new TestFixture {
    override lazy val showAll: ShowAll = ShowAll.Enabled

    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        FormComponentId("country"),
        Register.Country,
        None,
        LookupQuery.Empty
      )
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("United Kingdom", "United States")
    }
  }

  it should "lookup options when no query provided, but selection criteria provided" in new TestFixture {
    override lazy val showAll: ShowAll = ShowAll.Enabled
    override lazy val countryLookupSelectionCriteria: Option[List[SelectionCriteria]] =
      Some(SelectionCriteria(CsvColumnName("Region"), SelectionCriteriaSimpleValue(List("1"))) :: Nil)

    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        FormComponentId("country"),
        Register.Country,
        None,
        LookupQuery.Empty
      )
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("United Kingdom")
    }
  }

  "lookupWithSelectionCriteria - showAll disabled" should "lookup options for given query and no selection criteria" in new TestFixture {

    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        FormComponentId("country"),
        Register.Country,
        None,
        LookupQuery.Value("States")
      )
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List(
        "United States Minor outlying islands",
        "United States",
        "Virgin Islands, United States"
      )
    }
  }

  it should "lookup options for given query and empty selection criteria" in new TestFixture {

    override lazy val countryLookupSelectionCriteria: Option[List[SelectionCriteria]] = Some(List.empty)

    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        FormComponentId("country"),
        Register.Country,
        None,
        LookupQuery.Value("States")
      )
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("United States")
    }
  }

  it should "lookup options for given query and SelectionCriteriaSimpleValue" in new TestFixture {

    override lazy val countryLookupSelectionCriteria: Option[List[SelectionCriteria]] =
      Some(SelectionCriteria(CsvColumnName("Region"), SelectionCriteriaSimpleValue(List("1"))) :: Nil)

    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        FormComponentId("country"),
        Register.Country,
        None,
        LookupQuery.Value("United")
      )
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("United Kingdom")
    }
  }

  it should "lookup options for given query and SelectionCriteriaExpr" in new TestFixture {

    override lazy val variadicFormData: VariadicFormData[SourceOrigin.OutOfDate] =
      VariadicFormData[SourceOrigin.OutOfDate](
        Map(ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId("country"))) -> VariadicValue.One("US"))
      )
    override lazy val portLookupSelectionCriteria: Option[List[SelectionCriteria]] = Some(
      SelectionCriteria(CsvColumnName("CountryCode"), SelectionCriteriaExpr(FormCtx(FormComponentId("country")))) :: Nil
    )

    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        FormComponentId("port"),
        Register.Port,
        None,
        LookupQuery.Value("john")
      )
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("John F. Kennedy Apt/New York Airport - JFK")
    }
  }

  it should "lookup options for given query and SelectionCriteriaReference" in new TestFixture {

    override lazy val variadicFormData: VariadicFormData[SourceOrigin.OutOfDate] =
      VariadicFormData[SourceOrigin.OutOfDate](
        Map(
          ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId("country"))) -> VariadicValue.One(
            "United Kingdom"
          )
        )
      )
    override lazy val portLookupSelectionCriteria: Option[List[SelectionCriteria]] = Some(
      SelectionCriteria(
        CsvColumnName("Region"),
        SelectionCriteriaReference(FormCtx(FormComponentId("country")), CsvColumnName("Region"))
      ) :: Nil
    )

    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        FormComponentId("port"),
        Register.Port,
        None,
        LookupQuery.Value("dove")
      )
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("Dover Seaport - DOV")
    }
  }

  it should "lookup options for given query, sorted by priority and label" in new TestFixture {
    override lazy val showAll: ShowAll = ShowAll.Enabled
    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        FormComponentId("port"),
        Register.Port,
        None,
        LookupQuery.Value("air")
      )
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)

      responseBody shouldBe List(
        "London Heathrow Airport - LHR",
        "John F. Kennedy Apt/New York Airport - JFK"
      )
    }
  }

  private def responseBodyAs[T](result: Result)(implicit reads: Reads[T]): T =
    Json.parse(Await.result(result.body.consumeData, 5.seconds).decodeString("utf-8")).as[T]

  trait TestFixture {
    implicit val lang: LangADT = LangADT.En
    val request = FakeRequest("GET", "/")
    private val msgesApi: MessagesApi = Helpers.stubMessagesApi(
      Map(
        "en" -> Map(
          "date.January" -> "January"
        )
      )
    )
    implicit val messages: Messages = Helpers.stubMessages(
      msgesApi
    )
    lazy val i18nSupport: I18nSupport = new I18nSupport {
      override def messagesApi: MessagesApi = msgesApi
    }
    val mockAuth: AuthenticatedRequestActionsAlgebra[Future] = mock[AuthenticatedRequestActionsAlgebra[Future]]

    lazy val countryLookupSelectionCriteria: Option[List[SelectionCriteria]] = None
    lazy val portLookupSelectionCriteria: Option[List[SelectionCriteria]] = None

    lazy val countryLookupOptions: LocalisedLookupOptions = LocalisedLookupOptions(
      Map(
        LangADT.En -> LookupOptions(
          Map(
            LookupLabel("United Kingdom") -> CountryLookupInfo(
              LookupId("GB"),
              0,
              LookupPriority(1),
              LookupPriority(1),
              LookupRegion("1"),
              LookupInGibraltarEuEeaEfta("1"),
              Map()
            ),
            LookupLabel("United States") -> CountryLookupInfo(
              LookupId("US"),
              1,
              LookupPriority(1),
              LookupPriority(1),
              LookupRegion("2"),
              LookupInGibraltarEuEeaEfta("1"),
              Map()
            )
          )
        )
      )
    )

    lazy val portLookupOptions: LocalisedLookupOptions = LocalisedLookupOptions(
      Map(
        LangADT.En -> LookupOptions(
          Map(
            LookupLabel("London Heathrow Airport - LHR") -> PortLookupInfo(
              LookupId("1"),
              0,
              LookupPriority(3),
              LookupRegion("1"),
              LookupPortType("AIR"),
              LookupCountryCode("GB"),
              LookupPortCode("LHR")
            ),
            LookupLabel("Dover Seaport - DOV") -> PortLookupInfo(
              LookupId("2"),
              1,
              LookupPriority(3),
              LookupRegion("1"),
              LookupPortType("SEA"),
              LookupCountryCode("GB"),
              LookupPortCode("DOV")
            ),
            LookupLabel("John F. Kennedy Apt/New York Airport - JFK") -> PortLookupInfo(
              LookupId("3"),
              2,
              LookupPriority(2),
              LookupRegion("2"),
              LookupPortType("AIR"),
              LookupCountryCode("US"),
              LookupPortCode("NYC")
            ),
            LookupLabel("Leland Seaport - LLC") -> PortLookupInfo(
              LookupId("4"),
              3,
              LookupPriority(1),
              LookupRegion("2"),
              LookupPortType("SEA"),
              LookupCountryCode("US"),
              LookupPortCode("LLC")
            )
          )
        )
      )
    )

    lazy val showAll: ShowAll = ShowAll.Disabled
    lazy val sections: List[Section] =
      mkSection(
        mkFormComponent("country", Text(Lookup(Register.Country, countryLookupSelectionCriteria), Value)) :: Nil
      ) :: mkSection(
        mkFormComponent("port", Text(Lookup(Register.Port, portLookupSelectionCriteria), Value)) :: Nil
      ) :: Nil
    lazy val authCacheWithForm: AuthCacheWithForm = mkAuthCacheWithForm(mkFormTemplate(sections))
    lazy val variadicFormData: VariadicFormData[SourceOrigin.OutOfDate] = VariadicFormData.empty[SourceOrigin.OutOfDate]
    lazy val formModelOptics: FormModelOptics[DataOrigin.Mongo] =
      FormModelOptics
        .mkFormModelOptics[DataOrigin.Mongo, SectionSelectorType.Normal](
          variadicFormData,
          authCacheWithForm
        )

    val messagesControllerComponents: MessagesControllerComponents = stubMessagesControllerComponents()
    mockAuth
      .authAndRetrieveForm[SectionSelectorType.Normal](*[FormTemplateId], *[Option[AccessCode]], *[OperationWithForm])(
        *[Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[
          DataOrigin.Mongo
        ] => Future[Result]]
      ) answers (
      (
        _: FormTemplateId, _: Option[AccessCode], _: OperationWithForm, f: Request[
          AnyContent
        ] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[DataOrigin.Mongo] => Future[Result]
      ) =>
        messagesControllerComponents.actionBuilder.async { request =>
          f(request)(LangADT.En)(authCacheWithForm)(null)(formModelOptics)
        }
    )
    val lookupLoader = new LookupLoader("target/scala-2.13/resource_managed/main/conf/index")
    lazy val lookupRegistry = new LookupRegistry(
      Map(
        Register.Country -> AjaxLookup(
          countryLookupOptions,
          lookupLoader.mkIndexSearcher("country"),
          showAll
        ),
        Register.Port -> AjaxLookup(
          portLookupOptions,
          lookupLoader.mkIndexSearcher("port"),
          showAll
        )
      )
    )
    val choiceRuntimeIndexService: ChoiceRuntimeIndexService = new ChoiceRuntimeIndexService()
    lazy val lookupController =
      new LookupController(
        i18nSupport,
        mockAuth,
        lookupRegistry,
        choiceRuntimeIndexService,
        messagesControllerComponents
      )
  }
}
