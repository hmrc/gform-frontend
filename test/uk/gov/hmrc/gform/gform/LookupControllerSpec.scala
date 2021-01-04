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

package uk.gov.hmrc.gform.gform

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.Id
import org.mockito.{ ArgumentMatchersSugar, IdiomaticMockito }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ FlatSpecLike, Matchers }
import play.api.libs.json.{ Json, Reads }
import play.api.mvc.{ AnyContent, MessagesControllerComponents, Request, Result }
import play.api.test.FakeRequest
import uk.gov.hmrc.gform.LookupLoader.mkAutocomplete
import uk.gov.hmrc.gform.PlayStubSupport
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActionsAlgebra }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkFormComponent, mkFormTemplate, mkSection }
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ FormModelSupport, LookupQuery, SectionSelectorType, VariadicFormDataSupport }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ CsvColumnName, FormComponentId, FormCtx, FormTemplateId, Lookup, Register, Section, SelectionCriteria, Text, Value }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SelectionCriteriaValue.{ SelectionCriteriaExpr, SelectionCriteriaReference, SelectionCriteriaSimpleValue }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, SourceOrigin, VariadicFormData, VariadicValue }
import uk.gov.hmrc.gform.typeclasses.identityThrowableMonadError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

class LookupControllerSpec
    extends FlatSpecLike with Matchers with IdiomaticMockito with ArgumentMatchersSugar with ScalaFutures
    with FormModelSupport with VariadicFormDataSupport with PlayStubSupport {

  implicit val sys = ActorSystem("LookupControllerSpec")
  implicit val mat = ActorMaterializer()

  "lookupWithSelectionCriteria - showAll enabled" should "lookup options when no query provided and no selection criteria provided" in new TestFixture {
    override lazy val showAll: ShowAll = ShowAll.Enabled

    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        BaseComponentId("country"),
        Register.Country,
        None,
        LookupQuery.Empty)
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("United Kingdom", "United States")
    }
  }

  it should "lookup options when no query provided, but selection criteria provided" in new TestFixture {
    override lazy val showAll: ShowAll = ShowAll.Enabled
    override lazy val countryLookupSelectionCriteria: Option[List[SelectionCriteria]] =
      Some(SelectionCriteria(CsvColumnName("region"), SelectionCriteriaSimpleValue(List("1"))) :: Nil)

    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        BaseComponentId("country"),
        Register.Country,
        None,
        LookupQuery.Empty)
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
        BaseComponentId("country"),
        Register.Country,
        None,
        LookupQuery.Value("States"))
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("United States")
    }
  }

  it should "lookup options for given query and empty selection criteria" in new TestFixture {

    override lazy val countryLookupSelectionCriteria: Option[List[SelectionCriteria]] = Some(List.empty)

    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        BaseComponentId("country"),
        Register.Country,
        None,
        LookupQuery.Value("States"))
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("United States")
    }
  }

  it should "lookup options for given query and SelectionCriteriaSimpleValue" in new TestFixture {

    override lazy val countryLookupSelectionCriteria: Option[List[SelectionCriteria]] =
      Some(SelectionCriteria(CsvColumnName("region"), SelectionCriteriaSimpleValue(List("1"))) :: Nil)

    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        BaseComponentId("country"),
        Register.Country,
        None,
        LookupQuery.Value("United"))
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("United Kingdom")
    }
  }

  it should "lookup options for given query and SelectionCriteriaExpr" in new TestFixture {

    override lazy val variadicFormData: VariadicFormData[SourceOrigin.OutOfDate] =
      VariadicFormData[SourceOrigin.OutOfDate](
        Map(ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId("country"))) -> VariadicValue.One("US")))
    override lazy val portLookupSelectionCriteria: Option[List[SelectionCriteria]] = Some(SelectionCriteria(
      CsvColumnName("countrycode"),
      SelectionCriteriaExpr(FormCtx(FormComponentId("country")))) :: Nil)

    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        BaseComponentId("port"),
        Register.Port,
        None,
        LookupQuery.Value("Port"))
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("New York Airport", "Leland Seaport")
    }
  }

  it should "lookup options for given query and SelectionCriteriaReference" in new TestFixture {

    override lazy val variadicFormData: VariadicFormData[SourceOrigin.OutOfDate] =
      VariadicFormData[SourceOrigin.OutOfDate](
        Map(ModelComponentId.pure(IndexedComponentId.pure(BaseComponentId("country"))) -> VariadicValue.One(
          "United Kingdom")))
    override lazy val portLookupSelectionCriteria: Option[List[SelectionCriteria]] = Some(
      SelectionCriteria(
        CsvColumnName("region"),
        SelectionCriteriaReference(FormCtx(FormComponentId("country")), CsvColumnName("region"))) :: Nil)

    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        BaseComponentId("port"),
        Register.Port,
        None,
        LookupQuery.Value("Port"))
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("Dover Seaport", "London Heathrow")
    }
  }

  it should "lookup options for given query, sorted by priority and label" in new TestFixture {
    val future = lookupController
      .lookupWithSelectionCriteria(
        FormTemplateId("someTemplateId"),
        BaseComponentId("port"),
        Register.Port,
        None,
        LookupQuery.Value("Port"))
      .apply(request)

    whenReady(future) { result =>
      val responseBody = responseBodyAs[List[String]](result)
      responseBody shouldBe List("Dover Seaport", "London Heathrow", "New York Airport", "Leland Seaport")
    }
  }

  private def responseBodyAs[T](result: Result)(implicit reads: Reads[T]): T =
    Json.parse(Await.result(result.body.consumeData, 5.seconds).decodeString("utf-8")).as[T]

  trait TestFixture {
    val request = FakeRequest("GET", "/")
    val mockAuth: AuthenticatedRequestActionsAlgebra[Future] = mock[AuthenticatedRequestActionsAlgebra[Future]]

    lazy val countryLookupSelectionCriteria: Option[List[SelectionCriteria]] = None
    lazy val portLookupSelectionCriteria: Option[List[SelectionCriteria]] = None

    lazy val countryLookupOptions: LocalisedLookupOptions = LocalisedLookupOptions(
      Map(
        LangADT.En -> LookupOptions(Map(
          LookupLabel("United Kingdom") -> CountryLookupInfo(
            LookupId("GB"),
            0,
            LookupKeywords(Some("England Great Britain")),
            LookupPriority(1),
            LookupRegion("1")),
          LookupLabel("United States") -> CountryLookupInfo(
            LookupId("US"),
            1,
            LookupKeywords(Some("USA")),
            LookupPriority(1),
            LookupRegion("2"))
        ))))

    lazy val portLookupOptions: LocalisedLookupOptions = LocalisedLookupOptions(
      Map(
        LangADT.En -> LookupOptions(Map(
          LookupLabel("London Heathrow") -> PortLookupInfo(
            LookupId("1"),
            0,
            LookupKeywords(Some("London Heathrow Port")),
            LookupPriority(3),
            LookupRegion("1"),
            LookupPortType("AIR"),
            LookupCountryCode("GB"),
            LookupPortCode("LHR")
          ),
          LookupLabel("Dover Seaport") -> PortLookupInfo(
            LookupId("2"),
            1,
            LookupKeywords(Some("Dover Seaport Port")),
            LookupPriority(3),
            LookupRegion("1"),
            LookupPortType("SEA"),
            LookupCountryCode("GB"),
            LookupPortCode("DOV")
          ),
          LookupLabel("New York Airport") -> PortLookupInfo(
            LookupId("3"),
            2,
            LookupKeywords(Some("New York Airport Port")),
            LookupPriority(2),
            LookupRegion("2"),
            LookupPortType("AIR"),
            LookupCountryCode("US"),
            LookupPortCode("NYC")
          ),
          LookupLabel("Leland Seaport") -> PortLookupInfo(
            LookupId("4"),
            3,
            LookupKeywords(Some("Leland Seaport Port")),
            LookupPriority(1),
            LookupRegion("2"),
            LookupPortType("SEA"),
            LookupCountryCode("US"),
            LookupPortCode("LLC")
          )
        ))))

    lazy val lookupOptions: LocalisedLookupOptions = LocalisedLookupOptions(Map.empty)
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
        .mkFormModelOptics[DataOrigin.Mongo, Id, SectionSelectorType.Normal](
          variadicFormData,
          authCacheWithForm,
          recalculation)

    val messagesControllerComponents: MessagesControllerComponents = stubMessagesControllerComponents()
    mockAuth
      .authAndRetrieveForm[SectionSelectorType.Normal](*[FormTemplateId], *[Option[AccessCode]], *[OperationWithForm])(
        *[Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[
          DataOrigin.Mongo] => Future[Result]]) answers (
      (
        _: FormTemplateId,
        _: Option[AccessCode],
        _: OperationWithForm,
        f: Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => FormModelOptics[
          DataOrigin.Mongo] => Future[Result]) =>
        messagesControllerComponents.actionBuilder.async { request =>
          f(request)(LangADT.En)(authCacheWithForm)(null)(formModelOptics)
        })
    lazy val lookupRegistry = new LookupRegistry(
      Map(
        Register.Country -> AjaxLookup(
          countryLookupOptions,
          mkAutocomplete(countryLookupOptions),
          showAll
        ),
        Register.Port -> AjaxLookup(
          portLookupOptions,
          mkAutocomplete(portLookupOptions),
          showAll
        )
      ))
    lazy val lookupController = new LookupController(mockAuth, lookupRegistry, messagesControllerComponents)
  }
}
