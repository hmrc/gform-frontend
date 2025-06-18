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

import cats.Id
import org.mockito.MockitoSugar.mock
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.Tables.Table
import play.api.http.HttpConfiguration
import play.api.i18n._
import play.api.{ Configuration, Environment }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.addresslookup.AddressLookupService
import uk.gov.hmrc.gform.api.{ BankAccountInsightsAsyncConnector, CompanyInformationAsyncConnector, DelegatedAgentAuthAsyncConnector, HipConnector, NinoInsightsAsyncConnector }
import uk.gov.hmrc.gform.bars.BankAccountReputationAsyncConnector
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gform.{ FastForwardService, FileSystemConnector }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.ls
import uk.gov.hmrc.gform.graph.{ GraphException, Recalculation }
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.objectStore.ObjectStoreService
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormModelOptics, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber.Classic
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, PageId, ShortText, TemplateSectionIndex, Text, Value }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.validation.ValidationService

import scala.concurrent.Future

class FormProcessorSpec extends Spec with FormModelSupport with VariadicFormDataSupport {

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

  implicit val messages: Messages = i18nSupport.messagesApi.preferred(Seq(langs.availables.head))
  implicit val lang: LangADT = LangADT.En

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
  private val delegatedAgentAuthConnector: DelegatedAgentAuthAsyncConnector = mock[DelegatedAgentAuthAsyncConnector]
  private val hipConnector: HipConnector[Future] = mock[HipConnector[Future]]
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
    delegatedAgentAuthConnector,
    hipConnector,
    messages
  )

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

}
