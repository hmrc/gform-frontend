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

package uk.gov.hmrc.gform.gform.processor

import cats.{ Id, MonadError }
import org.mockito.scalatest.IdiomaticMockito
import cats.data.NonEmptyList
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.Tables.Table
import play.api.http.HttpConfiguration
import play.api.i18n._
import play.api.libs.json.Json
import play.api.mvc.{ AnyContentAsEmpty, Request }
import play.api.test.FakeRequest
import play.api.{ Configuration, Environment }
import uk.gov.hmrc.gform.{ FormTemplateKey, Spec }
import uk.gov.hmrc.gform.addresslookup.AddressLookupService
import uk.gov.hmrc.gform.api.{ BankAccountInsightsAsyncConnector, CompanyInformationAsyncConnector, NinoInsightsAsyncConnector }
import uk.gov.hmrc.gform.auth.models.{ MaterialisedRetrievals, Role }
import uk.gov.hmrc.gform.bars.BankAccountReputationAsyncConnector
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, CacheData }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.eval.{ EvaluationContext, FileIdsWithMapping }
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gform.{ FastForwardService, FileSystemConnector }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.ls
import uk.gov.hmrc.gform.graph.{ GraphException, Recalculation, RecalculationResult }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.email.EmailFieldId
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.objectStore.{ EnvelopeWithMapping, ObjectStoreService }
import uk.gov.hmrc.gform.sharedmodel.form.{ EmailAndCode, EnvelopeId, Form, FormData, FormField, FormModelOptics, QueryParams, TaskIdTaskStatusMapping, ThirdPartyData, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber.Classic
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, Coordinates, FileSizeLimit, FormComponent, FormComponentId, FormPhase, FormTemplate, FormTemplateContext, PageId, ShortText, TemplateSectionIndex, Text, Value }
import uk.gov.hmrc.gform.sharedmodel.{ Attr, AttributeInstruction, BooleanExprCache, ConstructAttribute, DataRetrieve, DataRetrieveId, DataRetrieveResult, Fetch, LangADT, NotChecked, RetrieveDataType, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.validation.{ ValidationResult, ValidationService }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class FormProcessorSpec extends Spec with FormModelSupport with VariadicFormDataSupport with IdiomaticMockito {

  override val envelopeId: EnvelopeId = EnvelopeId("dummy")
  private val environment: Environment = Environment.simple()
  private val configuration: Configuration = Configuration.load(environment)
  private val langs: Langs = new DefaultLangs()
  private val httpConfiguration: HttpConfiguration = HttpConfiguration.fromConfiguration(configuration, environment)
  private val localMessagesApi: MessagesApi =
    new DefaultMessagesApiProvider(environment, configuration, langs, httpConfiguration).get
  private val i18nSupport: I18nSupport = new I18nSupport {
    override def messagesApi: MessagesApi = localMessagesApi
  }

  lazy val formTemplate: FormTemplate = buildFormTemplate
  lazy val validationResult = ValidationResult.empty

  implicit val messages: Messages = i18nSupport.messagesApi.preferred(Seq(langs.availables.head))
  implicit val lang: LangADT = LangADT.En
  implicit val request: Request[AnyContentAsEmpty.type] =
    FakeRequest().addAttr(FormTemplateKey, FormTemplateContext(formTemplate, None, None, None, None))

  private val processDataService: ProcessDataService[Future] = mock[ProcessDataService[Future]]
  private val gformConnector: GformConnector = mock[GformConnector]
  private val fileSystemConnector: FileSystemConnector = mock[FileSystemConnector]
  private val validationService: ValidationService = mock[ValidationService]
  private val fastForwardService: FastForwardService = mock[FastForwardService]
  private val objectStoreService: ObjectStoreService = mock[ObjectStoreService]
  private val formControllerRequestHandler: FormControllerRequestHandler = mock[FormControllerRequestHandler]
  private val bankAccountReputationConnector: BankAccountReputationAsyncConnector =
    mock[BankAccountReputationAsyncConnector]
  private val companyInformationConnector: CompanyInformationAsyncConnector = mock[CompanyInformationAsyncConnector]
  private val ninoInsightsConnector: NinoInsightsAsyncConnector = mock[NinoInsightsAsyncConnector]
  private val addressLookupService: AddressLookupService[Future] = mock[AddressLookupService[Future]]
  private val bankAccountInsightsConnector: BankAccountInsightsAsyncConnector = mock[BankAccountInsightsAsyncConnector]
  private val localRecalculation: Recalculation[Future, Throwable] =
    new Recalculation[Future, Throwable](
      eligibilityStatusTrue,
      delegatedEnrolmentCheckStatus,
      dbLookupCheckStatus,
      (s: GraphException) => new IllegalArgumentException(s.reportProblem)
    )

  val formProcessor = new FormProcessor(
    i18nSupport,
    processDataService,
    gformConnector,
    fileSystemConnector,
    validationService,
    fastForwardService,
    localRecalculation,
    objectStoreService,
    formControllerRequestHandler,
    bankAccountReputationConnector,
    companyInformationConnector,
    ninoInsightsConnector,
    addressLookupService,
    bankAccountInsightsConnector,
    messages
  )

  val mockValidationService = mock[ValidationService]
  val mockRecalculation = mock[Recalculation[Future, Throwable]]

  "checkForRevisits" should "correctly remove page(s) from visits index" in {
    val sections = (0 to 4).map { i =>
      val pagesToRevisit = i match {
        case 0 => Some(List(PageId("page1id"), PageId("page2id"), PageId("page4id")))
        case 2 => Some(List(PageId("page3id")))
        case _ => None
      }

      nonRepeatingPageSection(
        s"Page $i",
        fields = List(
          FormComponent(
            FormComponentId(s"comp$i"),
            Text(ShortText.default, Value),
            ls,
            false,
            None,
            None,
            None,
            None,
            true,
            true,
            true,
            false,
            false,
            None,
            None,
            pageIdsToDisplayOnChange = pagesToRevisit
          )
        ),
        pageId = Some(PageId(s"page${i}id"))
      )
    }.toList

    val existingData: VariadicFormData[SourceOrigin.OutOfDate] =
      variadicFormData[SourceOrigin.OutOfDate]((0 to 4).map(i => s"comp$i" -> s"val$i"): _*)

    val fmb: FormModelBuilder[Throwable, Id] = mkFormModelFromSections(sections)

    val visibilityOpticsMongo: FormModelVisibilityOptics[DataOrigin.Mongo] =
      fmb.visibilityModel[DataOrigin.Mongo, SectionSelectorType.Normal](existingData, None)
    val formModelOpticsMongo =
      fmb.renderPageModel[DataOrigin.Mongo, SectionSelectorType.Normal](visibilityOpticsMongo, None)
    val visibilityFormModelVisibility: FormModel[Visibility] = formModelOpticsMongo.formModelVisibilityOptics.formModel
    val initialVisitsIndex = VisitIndex.Classic(
      (0 to 4).map(pageIdx => Classic.NormalPage(TemplateSectionIndex(pageIdx))).toSet
    )

    val table = Table(
      ("enteredData", "pageIdxToValidate", "expected"),
      (
        variadicFormData[SourceOrigin.OutOfDate](
          "comp0" -> "val0",
          "comp1" -> "val1",
          "comp2" -> "valUpdate",
          "comp3" -> "val3",
          "comp4" -> "val4"
        ),
        1,
        Set(0, 1, 2, 3, 4)
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate](
          "comp0" -> "val0",
          "comp1" -> "val1",
          "comp2" -> "valUpdate",
          "comp3" -> "val3",
          "comp4" -> "val4"
        ),
        2,
        Set(0, 1, 2, 4)
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate](
          "comp0" -> "valUpdate",
          "comp1" -> "val1",
          "comp2" -> "val2",
          "comp3" -> "val3",
          "comp4" -> "val4"
        ),
        0,
        Set(0, 3)
      ),
      (
        variadicFormData[SourceOrigin.OutOfDate](
          "comp0" -> "val0",
          "comp1" -> "val1",
          "comp2" -> "val2",
          "comp3" -> "val3",
          "comp4" -> "val4"
        ),
        0,
        Set(0, 1, 2, 3, 4)
      )
    )
    TableDrivenPropertyChecks.forAll(table) { (enteredFormData, pageIdxToValidate, expectedPageSet) =>
      val enteredVariadicFormData: EnteredVariadicFormData = EnteredVariadicFormData(enteredFormData)

      val expected = VisitIndex.Classic(
        expectedPageSet.map(pageIdx => Classic.NormalPage(TemplateSectionIndex(pageIdx)))
      )

      val visibilityPageModel: PageModel[Visibility] =
        visibilityFormModelVisibility(Classic.NormalPage(TemplateSectionIndex(pageIdxToValidate)))
      val formModelOptics: FormModelOptics[DataOrigin.Mongo] =
        fmb.renderPageModel[DataOrigin.Mongo, SectionSelectorType.Normal](visibilityOpticsMongo, None)

      val actual: VisitIndex = formProcessor.checkForRevisits(
        visibilityPageModel,
        initialVisitsIndex,
        formModelOptics,
        enteredVariadicFormData
      )

      expected shouldBe actual
    }
  }

  "processRemoveAddToList" should "remove and re-key data retrieves" in {
    val thirdPartyData = ThirdPartyData(
      obligations = NotChecked,
      emailVerification = Map.empty[EmailFieldId, EmailAndCode],
      queryParams = QueryParams.empty,
      reviewData = None,
      booleanExprCache = BooleanExprCache.empty,
      dataRetrieve = Some(
        Map(
          DataRetrieveId("1_bankDetails") -> getDrResultWithIndex(1),
          DataRetrieveId("2_bankDetails") -> getDrResultWithIndex(2)
        )
      ),
      postcodeLookup = None,
      selectedAddresses = None,
      enteredAddresses = None,
      confirmedAddresses = None,
      itmpRetrievals = None
    )

    lazy val form: Form = buildForm(
      FormData(
        List(
          FormField(FormComponentId("1_page1Field").modelComponentId, "value1"),
          FormField(FormComponentId("2_page1Field").modelComponentId, "value2"),
          FormField(FormComponentId("1_page2Field").modelComponentId, "value3"),
          FormField(FormComponentId("2_page2Field").modelComponentId, "value4")
        )
      ),
      thirdPartyData
    )

    lazy val formTemplate: FormTemplate = buildFormTemplate(
      destinationList,
      List(
        addToListSection(
          "addToList",
          "addToListDesc",
          "addToListSumDesc",
          "addToList",
          "addToListSummary",
          addToListQuestion("addToListQuestion"),
          None,
          List(
            mkPage(
              "page1",
              None,
              List(buildFormComponent("page1Field", Value)),
              None,
              Option(NonEmptyList.one(getDataRetrieve("bankDetails")))
            ),
            mkPage("page2", None, List(buildFormComponent("page2Field", Value)))
          ),
          None
        )
      )
    )

    val cache = AuthCacheWithForm(
      retrievals,
      form,
      FormTemplateContext.basicContext(formTemplate, None),
      Role.Customer,
      maybeAccessCode,
      new LookupRegistry(Map())
    )

    mockValidationService
      .validateFormModel(
        *[CacheData],
        *[EnvelopeWithMapping],
        *[FormModelVisibilityOptics[DataOrigin.Mongo]],
        *[Option[Coordinates]]
      )(
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
      *[EvaluationContext],
      *[Messages]
    )(*[MonadError[Future, Throwable]]) returns Future.successful(
      RecalculationResult.empty(
        EvaluationContext(
          formTemplate._id,
          submissionRef,
          maybeAccessCode,
          retrievals,
          thirdPartyData,
          authConfig,
          hc,
          Option.empty[FormPhase],
          FileIdsWithMapping.empty,
          Map.empty,
          Set.empty,
          Set.empty,
          Set.empty,
          Map.empty,
          LangADT.En,
          messages,
          List.empty,
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

    val formModelOptics: FormModelOptics[DataOrigin.Mongo] = FormModelOptics
      .mkFormModelOptics[DataOrigin.Mongo, Future, SectionSelectorType.WithDeclaration](
        cache.variadicFormData[SectionSelectorType.WithDeclaration],
        cache,
        mockRecalculation
      )
      .futureValue

    val processData: ProcessData =
      mkProcessData(mkFormModelOptics(formTemplate, cache.variadicFormData[SectionSelectorType.Normal]))

    formProcessor.processRemoveAddToList(
      cache = cache,
      maybeAccessCode = maybeAccessCode,
      formModelOptics = formModelOptics,
      processData = processData,
      templateSectionIndex = TemplateSectionIndex(0),
      idx = 0,
      addToListId = AddToListId(FormComponentId("addToListQuestion")),
      fastForward = List(FastForward.Yes)
    )(request = request, hc = hc, lang = lang, sse = smartStringEvaluator)

    form.thirdPartyData.dataRetrieve.size shouldBe 1
    form.thirdPartyData.dataRetrieve.get(DataRetrieveId("1_bankDetails")).id shouldBe DataRetrieveId("1_bankDetails")
    form.thirdPartyData.dataRetrieve.contains(DataRetrieveId("2_bankDetails")) shouldBe false
  }

  def getDrResultWithIndex(idx: Int): DataRetrieveResult = {
    val dataRetrieveId = DataRetrieveId(s"${idx}_bankDetails")

    DataRetrieveResult(
      id = dataRetrieveId,
      data = RetrieveDataType.ObjectType(
        Map(
          DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs") -> "no",
          DataRetrieve.Attribute("sortCodeSupportsDirectDebit")              -> "no",
          DataRetrieve.Attribute("isValid")                                  -> "yes",
          DataRetrieve.Attribute("sortCodeBankName")                         -> "BARCLAYS BANK UK PLC",
          DataRetrieve.Attribute("sortCodeSupportsDirectCredit")             -> "no",
          DataRetrieve.Attribute("sortCodeIsPresentOnEISCD")                 -> "yes",
          DataRetrieve.Attribute("iban")                                     -> "GB21BARC20670586473611"
        )
      ),
      requestParams = Json.obj("accountNumber" -> "86473611", "sortCode" -> "206705")
    )
  }

  def getDataRetrieve(id: String): DataRetrieve =
    DataRetrieve(
      DataRetrieve.Type("validateBankDetails"),
      DataRetrieveId(id),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("isValid"),
            ConstructAttribute.AsIs(Fetch(List("accountNumberIsWellFormatted")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeBankName"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeBankName")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
            ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit")))
          ),
          AttributeInstruction(DataRetrieve.Attribute("iban"), ConstructAttribute.AsIs(Fetch(List("iban"))))
        )
      ),
      Map.empty[DataRetrieve.Attribute, DataRetrieve.AttrType],
      List.empty[DataRetrieve.ParamExpr],
      None
    )

}
