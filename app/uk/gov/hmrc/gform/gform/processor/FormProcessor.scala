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

package uk.gov.hmrc.gform.gform.processor

import cats.instances.future._
import cats.instances.option._
import cats.syntax.all._
import play.api.i18n.I18nSupport
import play.api.mvc.Results.Redirect
import play.api.mvc.{ AnyContent, Request, Result }
import uk.gov.hmrc.gform.addresslookup.{ AddressLookupResult, AddressLookupService }
import uk.gov.hmrc.gform.api.{ CompanyInformationConnector, NinoInsightsConnector }
import uk.gov.hmrc.gform.bars.BankAccountReputationConnector
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.FileIdsWithMapping
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.{ EnvelopeWithMapping, FileUploadAlgebra }
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gform.{ DataRetrieveService, FastForwardService, routes }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.gform.{ FormValidationOutcome, NoSpecificAction }
import uk.gov.hmrc.gform.models.ids.ModelPageId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.DataOrigin.Mongo
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.{ BusinessBankAccountExistence, CompanyRegistrationNumber, Employments, NinoInsights, PersonalBankAccountExistence, PersonalBankAccountExistenceWithName, ValidateBankDetails }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormComponentIdToFileIdMapping, FormModelOptics, ThirdPartyData, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax

import scala.concurrent.{ ExecutionContext, Future }

class FormProcessor(
  i18nSupport: I18nSupport,
  processDataService: ProcessDataService[Future],
  gformConnector: GformConnector,
  validationService: ValidationService,
  fastForwardService: FastForwardService,
  recalculation: Recalculation[Future, Throwable],
  fileUploadService: FileUploadAlgebra[Future],
  handler: FormControllerRequestHandler,
  bankAccountReputationConnector: BankAccountReputationConnector[Future],
  companyInformationConnector: CompanyInformationConnector[Future],
  ninoInsightsConnector: NinoInsightsConnector[Future],
  addressLookupService: AddressLookupService[Future]
)(implicit ec: ExecutionContext) {

  import i18nSupport._

  def processRemoveAddToList(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    fastForward: List[FastForward],
    formModelOptics: FormModelOptics[Mongo],
    processData: ProcessData,
    idx: Int,
    addToListId: AddToListId
  )(implicit
    request: Request[AnyContent],
    hc: HeaderCarrier,
    lang: LangADT,
    sse: SmartStringEvaluator
  ): Future[Result] = {
    def computePageLink(
      formPageId: PageId,
      pageIdSectionNumberMap: Map[ModelPageId, SectionNumber]
    ) = {
      val forModelPageId = formPageId.modelPageId
      pageIdSectionNumberMap.get(forModelPageId) match {
        case Some(sectionNumber) => Some(sectionNumber)
        case None                =>
          // In case when pageIdToDisplayAfterRemove refers to ATL
          // The section number should be ATL summary section
          // And pageIdToDisplayAfterRemove should be equal to ATL summary section baseId
          pageIdSectionNumberMap.toList
            .sortBy(_._1.maybeIndex)(Ordering[Option[Int]].reverse)
            .find { case (modelPageId, _) =>
              modelPageId.baseId === forModelPageId.baseId
            }
            .fold(Option.empty[SectionNumber]) { case (_, sectionNumber) =>
              Some(sectionNumber)
            }
      }
    }
    def saveAndRedirect(
      updFormModelOptics: FormModelOptics[DataOrigin.Browser],
      componentIdToFileId: FormComponentIdToFileIdMapping,
      postcodeLookupIds: Set[FormComponentId]
    ): Future[Result] = {
      val updFormModel: FormModel[DataExpanded] = updFormModelOptics.formModelRenderPageOptics.formModel

      val sn = updFormModel.brackets.addToListBracket(addToListId).lastSectionNumber

      val addToListBracket: Bracket.AddToList[DataExpanded] =
        formModelOptics.formModelRenderPageOptics.formModel.brackets.addToListBracket(addToListId)

      val isLastIteration = addToListBracket.iterations.size === 1

      val visitsIndex: VisitIndex = VisitIndex
        .updateSectionVisits(updFormModel, processData.formModel, processData.visitsIndex)

      val visitsIndexUpd =
        if (isLastIteration) {
          val iterationForSectionNumber: Bracket.AddToListIteration[DataExpanded] = addToListBracket
            .iterationForSectionNumber(sn)

          val visitsIndexForLastIteration: List[SectionNumber] = iterationForSectionNumber.singletons
            .map(_.sectionNumber)
            .toList ++ iterationForSectionNumber.checkYourAnswers.map(_.sectionNumber)

          visitsIndex.fold[VisitIndex] { classic =>
            val toBeRemoved = visitsIndexForLastIteration.map(_.unsafeToClassic.sectionNumber)
            VisitIndex.Classic(
              classic.visitsIndex -- toBeRemoved
            )
          } { taskList =>
            val toBeRemoved = visitsIndexForLastIteration.map(_.unsafeToTaskList.sectionNumber)
            val coordinates = sn.toCoordinatesUnsafe

            val indexes =
              taskList.visitsIndex
                .getOrElse(coordinates, throw new Exception(s"No VisitIndex found for coordinates $coordinates"))

            val updated = indexes -- toBeRemoved

            VisitIndex.TaskList(
              taskList.visitsIndex + (coordinates -> updated)
            )
          }
        } else
          visitsIndex

      val processDataUpd = processData.copy(
        formModelOptics = updFormModelOptics,
        visitsIndex = visitsIndexUpd
      )

      val cacheUpd = cache.copy(
        form = cache.form.copy(
          visitsIndex = visitsIndexUpd,
          thirdPartyData = cache.form.thirdPartyData.removePostcodeData(idx, postcodeLookupIds),
          componentIdToFileId = componentIdToFileId
        )
      )

      validateAndUpdateData(
        cacheUpd,
        processDataUpd,
        sn,
        sn,
        maybeAccessCode,
        fastForward,
        formModelOptics,
        EnteredVariadicFormData.empty,
        true
      ) { _ => _ => maybeSectionNumber =>
        val pageIdToRemove = updFormModel.brackets.addToListBracket(addToListId).source.pageIdToDisplayAfterRemove
        val pageIdSectionNumberMap = updFormModel.pageIdSectionNumberMap
        val sectionNumber =
          if (isLastIteration) {
            pageIdToRemove.fold(
              maybeSectionNumber match {
                case SectionOrSummary.Section(s) => addToListBracket.iterationForSectionNumber(s).firstSectionNumber
                case _                           => sn
              }
            )(pageId =>
              computePageLink(pageId, pageIdSectionNumberMap).getOrElse(
                throw new Exception(s"Unable to find section number for pageId: ${pageIdToRemove.get}")
              )
            )
          } else
            sn

        val sectionTitle4Ga = getSectionTitle4Ga(processDataUpd, sectionNumber)

        Redirect(
          routes.FormController
            .form(
              cache.formTemplate._id,
              maybeAccessCode,
              sectionNumber,
              sectionTitle4Ga,
              SuppressErrors.Yes,
              fastForward
            )
        )
      }
    }

    val formModel = formModelOptics.formModelRenderPageOptics.formModel
    val bracket = formModel.brackets.addToListBracket(addToListId)
    val (updData, componentIdToFileIdMapping, filesToDelete) =
      AddToListUtils.removeRecord(
        processData,
        bracket,
        idx,
        FileIdsWithMapping(formModel.allFileIds, cache.form.componentIdToFileId)
      )

    val postcodeLookupIds: Set[FormComponentId] = bracket.iterations
      .toList(idx)
      .toPageModel
      .toList
      .flatMap(_.allFormComponents)
      .collect { case fc @ IsPostcodeLookup() =>
        fc.id
      }
      .toSet

    for {
      updFormModelOptics <- FormModelOptics
                              .mkFormModelOptics[DataOrigin.Browser, Future, SectionSelectorType.Normal](
                                updData.asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]],
                                cache,
                                recalculation
                              )
      redirect <- saveAndRedirect(updFormModelOptics, componentIdToFileIdMapping, postcodeLookupIds)
      _        <- fileUploadService.deleteFiles(cache.form.envelopeId, filesToDelete)(cache.formTemplate.objectStore)
    } yield redirect
  }

  def validateAndUpdateData(
    cache: AuthCacheWithForm,
    processData: ProcessData,
    sectionNumber: SectionNumber,
    validationSectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    fastForward: List[FastForward],
    formModelOptics: FormModelOptics[Mongo],
    enteredVariadicFormData: EnteredVariadicFormData,
    visitPage: Boolean
  )(
    toResult: Option[(FormComponentId, AddressLookupResult)] => Option[String] => SectionOrSummary => Result
  )(implicit hc: HeaderCarrier, request: Request[AnyContent], l: LangADT, sse: SmartStringEvaluator): Future[Result] = {

    val formModelVisibilityOptics = processData.formModelOptics.formModelVisibilityOptics
    val pageModel: PageModel[Visibility] =
      formModelVisibilityOptics.formModel(sectionNumber)

    for {
      envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)(cache.formTemplate.objectStore)
      envelopeWithMapping = EnvelopeWithMapping(envelope, cache.form)
      FormValidationOutcome(isValid, formData, validatorsResult) <- handler.handleFormValidation(
                                                                      processData.formModelOptics,
                                                                      validationSectionNumber,
                                                                      cache.toCacheData,
                                                                      envelopeWithMapping,
                                                                      validationService.validatePageModel,
                                                                      enteredVariadicFormData
                                                                    )
      dataRetrieveResult <-
        if (isValid) {
          pageModel.fold { singleton =>
            singleton.page.dataRetrieve.flatTraverse {
              case v: ValidateBankDetails =>
                val maybeRequestParams = DataRetrieve.requestParamsFromCache(cache.form, v.id)
                implicit val b: BankAccountReputationConnector[Future] = bankAccountReputationConnector
                DataRetrieveService[ValidateBankDetails, Future]
                  .retrieve(v, processData.formModelOptics.formModelVisibilityOptics, maybeRequestParams)
              case v: BusinessBankAccountExistence =>
                val maybeRequestParams = DataRetrieve.requestParamsFromCache(cache.form, v.id)
                implicit val b: BankAccountReputationConnector[Future] = bankAccountReputationConnector
                DataRetrieveService[BusinessBankAccountExistence, Future]
                  .retrieve(v, processData.formModelOptics.formModelVisibilityOptics, maybeRequestParams)
              case p: PersonalBankAccountExistence =>
                val maybeRequestParams = DataRetrieve.requestParamsFromCache(cache.form, p.id)
                implicit val b: BankAccountReputationConnector[Future] = bankAccountReputationConnector
                DataRetrieveService[PersonalBankAccountExistence, Future]
                  .retrieve(p, processData.formModelOptics.formModelVisibilityOptics, maybeRequestParams)
              case p: PersonalBankAccountExistenceWithName =>
                val maybeRequestParams = DataRetrieve.requestParamsFromCache(cache.form, p.id)
                implicit val b: BankAccountReputationConnector[Future] = bankAccountReputationConnector
                DataRetrieveService[PersonalBankAccountExistenceWithName, Future]
                  .retrieve(p, processData.formModelOptics.formModelVisibilityOptics, maybeRequestParams)
              case v: CompanyRegistrationNumber =>
                val maybeRequestParams = DataRetrieve.requestParamsFromCache(cache.form, v.id)
                implicit val b: CompanyInformationConnector[Future] = companyInformationConnector
                DataRetrieveService[CompanyRegistrationNumber, Future]
                  .retrieve(v, processData.formModelOptics.formModelVisibilityOptics, maybeRequestParams)
              case n: NinoInsights =>
                val maybeRequestParams = DataRetrieve.requestParamsFromCache(cache.form, n.id)
                implicit val ni: NinoInsightsConnector[Future] = ninoInsightsConnector
                DataRetrieveService[NinoInsights, Future]
                  .retrieve(n, processData.formModelOptics.formModelVisibilityOptics, maybeRequestParams)
              case e: Employments =>
                val maybeRequestParams = DataRetrieve.requestParamsFromCache(cache.form, e.id)
                implicit val c: GformConnector = gformConnector
                DataRetrieveService[Employments, Future]
                  .retrieve(e, processData.formModelOptics.formModelVisibilityOptics, maybeRequestParams)
            }
          }(_ => Option.empty.pure[Future])(_ => Option.empty.pure[Future])
        } else Option.empty.pure[Future]

      updatePostcodeLookup <-
        if (isValid) {
          pageModel.postcodeLookup.flatTraverse { formComponent =>
            addressLookupService
              .retrievePostcodeLookupData(formComponent, formModelVisibilityOptics)
              .map(_.map(resp => formComponent.id -> resp))
          }
        } else Option.empty.pure[Future]

      res <- {
        val oldData: VariadicFormData[SourceOrigin.Current] = processData.formModelOptics.pageOpticsData

        val formDataU = oldData.toFormData ++ formData
        val before: ThirdPartyData = cache.form.thirdPartyData
        val after: ThirdPartyData =
          before
            .updateFrom(validatorsResult)
            .updateDataRetrieve(dataRetrieveResult)
            .updatePostcodeLookup(updatePostcodeLookup)

        val needsSecondPhaseRecalculation =
          (before.desRegistrationResponse, after.desRegistrationResponse)
            .mapN(_ =!= _)
            .getOrElse(false) ||
            before.dataRetrieve != after.dataRetrieve

        val visitsIndex =
          if (visitPage)
            processData.visitsIndex.visit(sectionNumber)
          else processData.visitsIndex.unvisit(sectionNumber)

        val cacheUpd =
          cache.copy(
            form = cache.form
              .copy(
                thirdPartyData = after.copy(obligations = processData.obligations),
                formData = formDataU,
                visitsIndex = visitsIndex
              )
          )

        val redirectUrl = if (isValid && pageModel.redirects.nonEmpty) {
          pageModel.redirects.collectFirst {
            case redirect
                if processData.formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(redirect.`if`, None) =>
              redirect.redirectUrl
          }
        } else None

        if (needsSecondPhaseRecalculation && isValid) {
          val newDataRaw = cacheUpd.variadicFormData[SectionSelectorType.Normal]
          for {
            newProcessData <- processDataService
                                .getProcessData[SectionSelectorType.Normal](
                                  newDataRaw,
                                  cacheUpd,
                                  formModelOptics,
                                  gformConnector.getAllTaxPeriods,
                                  NoSpecificAction
                                )
            result <- validateAndUpdateData(
                        cacheUpd,
                        newProcessData,
                        sectionNumber,
                        validationSectionNumber,
                        maybeAccessCode,
                        fastForward,
                        formModelOptics,
                        enteredVariadicFormData,
                        visitPage
                      )(toResult) // recursive call
          } yield result
        } else {
          fastForwardService
            .updateUserData(
              cacheUpd,
              processData.copy(visitsIndex = visitsIndex),
              maybeAccessCode,
              fastForward,
              envelopeWithMapping,
              Some(sectionNumber)
            )(toResult(updatePostcodeLookup)(redirectUrl.map(_.value())))
        }
      }
    } yield res
  }

  def getSectionTitle4Ga(processData: ProcessData, sectionNumber: SectionNumber): SectionTitle4Ga =
    sectionTitle4GaFactory(processData.formModel(sectionNumber), sectionNumber)
}
