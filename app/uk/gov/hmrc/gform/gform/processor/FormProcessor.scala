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
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.Result
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.gform.addresslookup.{ AddressLookupResult, AddressLookupService }
import uk.gov.hmrc.gform.api.{ BankAccountInsightsConnector, CompanyInformationConnector, DelegatedAgentAuthConnector, NinoInsightsConnector }
import uk.gov.hmrc.gform.bars.BankAccountReputationConnector
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.FileIdsWithMapping
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluationSyntax, SmartStringEvaluator, SmartStringEvaluatorFactory }
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.gform.{ FormValidationOutcome, NoSpecificAction }
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId, ModelPageId }
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.objectStore.{ EnvelopeWithMapping, ObjectStoreAlgebra }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.tasklist.TaskListUtils
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class FormProcessor(
  i18nSupport: I18nSupport,
  processDataService: ProcessDataService[Future],
  gformConnector: GformConnector,
  fileSystemConnector: FileSystemConnector,
  validationService: ValidationService,
  fastForwardService: FastForwardService,
  objectStoreService: ObjectStoreAlgebra[Future],
  handler: FormControllerRequestHandler,
  bankAccountReputationConnector: BankAccountReputationConnector[Future],
  companyInformationConnector: CompanyInformationConnector[Future],
  ninoInsightsConnector: NinoInsightsConnector[Future],
  addressLookupService: AddressLookupService[Future],
  bankAccountInsightConnector: BankAccountInsightsConnector[Future],
  delegatedAgentAuthConnector: DelegatedAgentAuthConnector[Future],
  smartStringEvaluatorFactory: SmartStringEvaluatorFactory
)(implicit ec: ExecutionContext) {

  def processRemoveAddToList(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    fastForward: List[FastForward],
    formModelOptics: FormModelOptics,
    processData: ProcessData,
    templateSectionIndex: TemplateSectionIndex,
    idx: Int,
    addToListId: AddToListId
  )(implicit
    hc: HeaderCarrier,
    lang: LangADT,
    sse: SmartStringEvaluator,
    messages: Messages
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
      updFormModelOptics: FormModelOptics,
      componentIdToFileId: FormComponentIdToFileIdMapping,
      postcodeLookupIds: Set[FormComponentId],
      dataRetrieveIds: Set[DataRetrieveId]
    ): Future[Result] = {
      val updFormModel: FormModel = updFormModelOptics.formModelRenderPageOptics.formModel

      val sn = updFormModel.brackets.addToListBracket(addToListId).lastSectionNumber

      val addToListBracket: Bracket.AddToList =
        formModelOptics.formModelRenderPageOptics.formModel.brackets.addToListBracket(addToListId)

      val isLastIteration = addToListBracket.iterations.size === 1

      val visitsIndexUpd =
        processData.visitsIndex.removeIteration(templateSectionIndex, idx + 1, isLastIteration, sn.maybeCoordinates)

      val processDataUpd = processData.copy(
        formModelOptics = updFormModelOptics,
        visitsIndex = visitsIndexUpd
      )

      val cacheUpd = cache.copy(
        form = cache.form
          .copy(
            visitsIndex = visitsIndexUpd,
            thirdPartyData = cache.form.thirdPartyData
              .removePostcodeData(idx, postcodeLookupIds)
              .removeDataRetrieveData(idx, dataRetrieveIds),
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
      ) { _ => _ => _ => _ =>
        val pageIdToRemove = updFormModel.brackets.addToListBracket(addToListId).source.pageIdToDisplayAfterRemove
        val pageIdSectionNumberMap = updFormModel.metadata.pageIdSectionNumberMap
        val sectionNumber =
          if (isLastIteration) {
            pageIdToRemove.fold(
              addToListBracket.iterationForSectionNumber(sn).defaultPageOrFirstSectionNumber
            )(pageId =>
              computePageLink(pageId, pageIdSectionNumberMap).getOrElse(
                throw new Exception(s"Unable to find section number for pageId: $pageId")
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
        FileIdsWithMapping(formModel.allFileIds, formModel.allMultiFileIds, cache.form.componentIdToFileId)
      )

    val postcodeLookupIds: Set[FormComponentId] = bracket.iterations
      .toList(idx)
      .toPageModel
      .toList
      .flatMap(_.allFormComponents)
      .collect { case fc @ IsPostcodeLookup(_) =>
        fc.id
      }
      .toSet

    val dataRetrieveIds: Set[DataRetrieveId] = bracket.iterations
      .toList(idx)
      .toPageModel
      .toList
      .flatMap(_.dataRetrieves)
      .map(_.id)
      .toSet
    val updFormModelOptics = FormModelOptics
      .mkFormModelOptics[SectionSelectorType.Normal](
        updData,
        cache
      )
    for {
      redirect <-
        saveAndRedirect(updFormModelOptics, componentIdToFileIdMapping, postcodeLookupIds, dataRetrieveIds)
      _ <- objectStoreService.deleteFiles(cache.form.envelopeId, filesToDelete)
    } yield redirect
  }

  private def getComponentsWithUpdatedValues(
    oldMap: collection.Map[ModelComponentId, VariadicValue],
    newMap: collection.Map[ModelComponentId, VariadicValue]
  ): Set[ModelComponentId] = {
    val commonKeys = newMap.keySet.intersect(oldMap.keySet)
    val oldKeysDiff = oldMap.keySet.diff(newMap.keySet)
    val newKeysDiff = newMap.keySet.diff(oldMap.keySet)

    val removed: Map[ModelComponentId, VariadicValue] = oldKeysDiff.map(k => k -> oldMap(k)).toMap
    val added: Map[ModelComponentId, VariadicValue] = newKeysDiff.map(k => k -> newMap(k)).toMap
    val updated: Map[ModelComponentId, VariadicValue] =
      commonKeys.filter(k => oldMap(k).toSeq =!= newMap(k).toSeq).map(k => k -> newMap(k)).toMap

    removed.keySet ++ added.keySet ++ updated.keySet
  }

  def checkForRevisits(
    pageModel: PageModel,
    visitsIndex: VisitIndex,
    formModelOptics: FormModelOptics,
    enteredVariadicFormData: EnteredVariadicFormData,
    sectionNumber: SectionNumber
  ): VisitIndex = {
    val formComponentsUpdated: Set[ModelComponentId] =
      getComponentsWithUpdatedValues(formModelOptics.variadicFormData.data, enteredVariadicFormData.userData.data)

    val pageList: Set[(List[PageId], Option[Int])] = formComponentsUpdated.flatMap(updated =>
      pageModel.allFormComponents
        .filter(fc => fc.baseComponentId === updated.baseComponentId)
        .flatMap(fc => fc.pageIdsToDisplayOnChange.map(p => p -> updated.indexedComponentId.maybeIndex))
    )

    val sectionsToRevisit: Set[Option[SectionNumber]] = pageList.flatMap {
      case (pageList: List[PageId], maybeIndex: Option[Int]) =>
        pageList.map { pageId =>
          val pageIdWithMaybeIndex: PageId = maybeIndex.map(idx => pageId.withIndex(idx)).getOrElse(pageId)
          val maybeSectionNumber: Option[SectionNumber] =
            formModelOptics.formModelVisibilityOptics.formModel.metadata.pageIdSectionNumberMap
              .get(pageIdWithMaybeIndex.modelPageId)

          if (maybeSectionNumber.isDefined)
            maybeSectionNumber
          else
            formModelOptics.formModelVisibilityOptics.formModel.metadata.pageIdSectionNumberMap
              .get(pageId.modelPageId)
        }.toSet
    }

    val additionalSections =
      if (formComponentsUpdated.nonEmpty && sectionNumber.isAddToList && !sectionNumber.isAddToListTerminalPage) {
        val bracket = formModelOptics.formModelVisibilityOptics.formModel.bracket(sectionNumber)
        bracket match {
          case bracket @ Bracket.AddToList(_, _, _) =>
            val iteration = bracket.iterationForSectionNumber(sectionNumber)
            Set(iteration.checkYourAnswers.map(_.sectionNumber), iteration.declarationSection.map(_.sectionNumber))
          case _ => Set.empty[Option[SectionNumber]]
        }
      } else Set.empty[Option[SectionNumber]]

    (sectionsToRevisit ++ additionalSections).foldLeft(visitsIndex) {
      case (acc, Some(sn)) => acc.unvisit(sn)
      case (acc, None)     => acc
    }
  }

  private def retrieveWithState(
    dataRetrieve: DataRetrieve,
    formModelOptics: FormModelOptics[DataOrigin.Browser],
    cache: AuthCacheWithForm
  )(implicit
    message: Messages,
    hc: HeaderCarrier,
    langADT: LangADT
  ): Future[(Option[DataRetrieveResult], FormModelOptics[DataOrigin.Browser], Option[PopulateAtlData])] = {
    val visibilityOptics = formModelOptics.formModelVisibilityOptics
    val maybePreviousResult: Option[DataRetrieveResult] =
      cache.form.thirdPartyData.dataRetrieve.flatMap(_.get(dataRetrieve.id))
    val request: DataRetrieve.Request =
      dataRetrieve.prepareRequest(visibilityOptics, maybePreviousResult, Some(cache.form.envelopeId.value))(message)
    val maybeRetrieveResultF = DataRetrieveService.retrieveDataResult(
      dataRetrieve,
      Some(cache.form),
      request,
      Some(bankAccountReputationConnector),
      Some(companyInformationConnector),
      Some(ninoInsightsConnector),
      Some(bankAccountInsightConnector),
      Some(gformConnector),
      Some(fileSystemConnector),
      Some(delegatedAgentAuthConnector)
    )

    maybeRetrieveResultF.map { r =>
      val visOptics = visibilityOptics.addDataRetrieveResults(r.toList)
      val newFormModelOptics = formModelOptics.copy(formModelVisibilityOptics = visOptics)
      val populateAtlData = dataRetrieve.populateATL
        .map(populateAtl => PopulateAtlService.getPopulateAtlData(populateAtl, newFormModelOptics))

      val populateAtlVisOptics = populateAtlData match {
        case Some(value) =>
          FormModelOptics
            .mkFormModelOptics[DataOrigin.Browser, SectionSelectorType.Normal](
              value._2.asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]],
              cache
            )
        case None => newFormModelOptics
      }

      (r, populateAtlVisOptics, populateAtlData.map(_._1))
    }
  }

  private def getFormProcessorData(
    isValid: Boolean,
    processData: ProcessData,
    formModelOptics: FormModelOptics[Mongo],
    enteredVariadicFormData: EnteredVariadicFormData,
    pageModel: PageModel[Visibility],
    cache: AuthCacheWithForm
  )(implicit
    hc: HeaderCarrier,
    messages: Messages,
    langADT: LangADT
  ): Future[(List[DataRetrieveResult], FormModelOptics[Browser], Seq[PopulateAtlData])] =
    if (isValid) {
      val updatedComponents: Set[BaseComponentId] = {
        if (processData.formModel.dataRetrieveAll.lookup.nonEmpty)
          getComponentsWithUpdatedValues(formModelOptics.pageOpticsData.data, enteredVariadicFormData.userData.data)
            .map(_.baseComponentId)
        else Set.empty[BaseComponentId]
      }
      val dataRetrievesOnThisPage: List[DataRetrieve] = pageModel
        .fold(singleton => singleton.page.dataRetrieves())(_ => List())(_ => List())
      val alreadyPresentInList: List[DataRetrieveId] = dataRetrievesOnThisPage.map(_.id)
      val dataRetrievesRequiringReeval: List[DataRetrieve] =
        processData.formModel.dataRetrieveAll.lookup.toList
          .filterNot { case (drId, dr) =>
            dr.callOnNoChange || dr.populateATL.isDefined
          }
          .flatMap { case (drId, dataRetrieve) =>
            val ifLeafs = dataRetrieve.`if`.map(_.booleanExpr.allExpressions.flatMap(_.leafs())).toList.flatten
            val paramLeafs = dataRetrieve.params.flatMap(_.expr.leafs())

            (ifLeafs ++ paramLeafs)
              .map {
                case FormCtx(fcId)         => Some(fcId)
                case LookupColumn(fcId, _) => Some(fcId)
                case _                     => None
              }
              .collect {
                case Some(fcId)
                    if updatedComponents.contains(fcId.baseComponentId) && !alreadyPresentInList.contains(drId) =>
                  dataRetrieve
              }
          }

      (dataRetrievesOnThisPage ++ dataRetrievesRequiringReeval)
        .foldLeft(
          Future.successful(
            (
              List.empty[DataRetrieveResult],
              processData.formModelOptics,
              Seq(): Seq[PopulateAtlData]
            )
          )
        ) { case (acc, r) =>
          acc.flatMap {
            case (results, optics, populateAtlDataSeq)
                if r.`if`.forall(optics.formModelVisibilityOptics.evalIncludeIfExpr(_, None)) =>
              retrieveWithState(r, optics, cache)(messages, implicitly, implicitly).map {
                case (Some(result), updatedOptics, populateAtlData) =>
                  (results :+ result, updatedOptics, populateAtlDataSeq ++ populateAtlData)
                case (None, _, _) =>
                  (results, optics, populateAtlDataSeq)
              }
            case (results, optics, populateAtlDataSeq) => Future.successful((results, optics, populateAtlDataSeq))
          }
        }
    } else Future.successful((List(), processData.formModelOptics, Seq()))

  def validateAndUpdateData(
    cache: AuthCacheWithForm,
    processData: ProcessData,
    sectionNumber: SectionNumber,
    validationSectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    fastForward: List[FastForward],
    formModelOptics: FormModelOptics,
    enteredVariadicFormData: EnteredVariadicFormData,
    visitPage: Boolean
  )(
    toResult: Option[(FormComponentId, AddressLookupResult)] => Option[
      String
    ] => SectionOrSummary => FormModelOptics[Browser] => Result
  )(implicit hc: HeaderCarrier, l: LangADT, sse: SmartStringEvaluator, messages: Messages): Future[Result] = {

    val formModelVisibilityOptics = processData.formModelOptics.formModelVisibilityOptics
    val pageModel: PageModel =
      formModelVisibilityOptics.formModel(sectionNumber)

    val allDataRetrieves: List[(DataRetrieveId, DataRetrieve)] =
      DataRetrieveAll.from(processData.formModel, cache.formTemplateContext.formTemplate).lookup.toList

    for {
      envelope <- objectStoreService.getEnvelope(cache.form.envelopeId)
      envelopeWithMapping = EnvelopeWithMapping(envelope, cache.form)
      FormValidationOutcome(isValid, formData, validatorsResult) <- handler.handleFormValidation(
                                                                      processData.formModelOptics,
                                                                      validationSectionNumber,
                                                                      cache.toCacheData,
                                                                      envelopeWithMapping,
                                                                      validationService.validatePageModelWithoutCustomValidators,
                                                                      enteredVariadicFormData,
                                                                      cache.form,
                                                                      cache.retrievals
                                                                    )
      (dataRetrieveResult, updatedFormOptics, populateAtlData) <-
        getFormProcessorData(isValid, processData, formModelOptics, enteredVariadicFormData, pageModel, cache)
      updatedCache = updateCacheFromValidatorsResult(cache, validatorsResult)
      updatePostcodeLookup <-
        if (isValid) {
          handler
            .handleFormValidation(
              processData.formModelOptics,
              validationSectionNumber,
              updatedCache.toCacheData,
              envelopeWithMapping,
              validationService.validatePageModel,
              enteredVariadicFormData,
              updatedCache.form,
              updatedCache.retrievals
            )
            .flatMap { formValidationOutcome =>
              if (formValidationOutcome.isValid) {
                pageModel.postcodeLookup.flatTraverse { formComponent =>
                  addressLookupService
                    .retrievePostcodeLookupData(formComponent, formModelVisibilityOptics)
                    .map(_.map(resp => formComponent.id -> resp))
                }
              } else Option.empty.pure[Future]
            }
        } else Option.empty.pure[Future]

      taskIdTaskStatusMapping <- if (sectionNumber.isTaskList) {
                                   TaskListUtils.evalTaskIdTaskStatus(
                                     updatedCache,
                                     envelopeWithMapping,
                                     processData.formModelOptics,
                                     validationService
                                   )
                                 } else TaskIdTaskStatusMapping.empty.pure[Future]

      res <- {
        val oldData: VariadicFormData = maybeRemoveVerifiedCode(
          validatorsResult,
          formModelVisibilityOptics,
          processData.formModelOptics.variadicFormData
        )

        val (upToDateOptics, dataRetrievesToRemove) = processData.formModel.dataRetrieveAll.lookup.toList
          .foldLeft[(FormModelOptics[DataOrigin.Browser], List[DataRetrieveId])](
            updatedFormOptics -> List.empty[DataRetrieveId]
          ) { case ((optics, removeList), (drId, dataRetrieve)) =>
            if (!dataRetrieve.`if`.forall(optics.formModelVisibilityOptics.evalIncludeIfExpr(_, None)))
              optics.copy(formModelVisibilityOptics =
                optics.formModelVisibilityOptics.removeDataRetrieveResults(List(drId))
              )         -> (removeList :+ drId)
            else optics -> removeList
          }

        val updatedThirdPartyData: ThirdPartyData = updatedCache.form.thirdPartyData
          .updateFrom(validatorsResult)
          .updateDataRetrieve(dataRetrieveResult)
          .removeDataRetrieves(dataRetrievesToRemove)
          .updatePostcodeLookup(updatePostcodeLookup)
          .updateConfirmations(processData.confirmations)

        // These are fields typically defined like:
        //    "value": "${someCalculation orElse 0}",
        //    "presentationHint": "invisibleInSummary",
        //    "submitMode": "summaryinfoonly"
        // whose values needs to be recomputed on every data change.
        upToDateOptics.freeCalculator.updateThirdPartyData(updatedThirdPartyData) // Side effect !!!
        val recalculatedDependencies =
          upToDateOptics.freeCalculator.recalculateDependenciesWithValue(
            processData.formModelOptics.formModelRenderPageOptics.allFormComponents
          )

        val formDataU: FormData = oldData.toFormData ++ formData ++ recalculatedDependencies

        val redirectUrl = if (isValid && pageModel.redirects.nonEmpty) {
          pageModel.redirects.collectFirst {
            case redirect if upToDateOptics.formModelVisibilityOptics.evalIncludeIfExpr(redirect.`if`, None) =>
              redirect.redirectUrl
          }
        } else None

        val visitsIndex =
          if (visitPage && redirectUrl.isEmpty)
            processData.visitsIndex.visit(sectionNumber)
          else processData.visitsIndex.unvisit(sectionNumber)

        val updatedVisitsIndex =
          checkForRevisits(pageModel, visitsIndex, formModelOptics, enteredVariadicFormData, sectionNumber)

        val cacheUpd =
          PopulateAtlService.updateCache(
            cache.copy(
              form = cache.form
                .copy(
                  thirdPartyData = updatedThirdPartyData.copy(obligations = processData.obligations),
                  formData = formDataU,
                  visitsIndex = updatedVisitsIndex,
                  taskIdTaskStatus = taskIdTaskStatusMapping
                )
            ),
            populateAtlData,
            updatedFormOptics.formModelRenderPageOptics,
            updatedVisitsIndex
          )

        val newDataRaw = cacheUpd.variadicFormData

        for {
          newProcessData <- processDataService
                              .getProcessData[SectionSelectorType.Normal](
                                newDataRaw,
                                cacheUpd,
                                gformConnector.getAllTaxPeriods,
                                NoSpecificAction,
                                updatedFormOptics.asInstanceOf[FormModelOptics[Mongo]]
                              )
          res <-
            fastForwardService
              .updateUserData(
                cacheUpd,
                newProcessData,
                maybeAccessCode,
                fastForward,
                envelopeWithMapping,
                Some(sectionNumber)
              )((sectionOrSummary, _) =>
                toResult(updatePostcodeLookup)(redirectUrl.map(_.value()))(sectionOrSummary)(
                  updatedFormOptics
                )
              )
        } yield res
      }
    } yield res
  }

  private def updateCacheFromValidatorsResult(cache: AuthCacheWithForm, validatorsResult: Option[ValidatorsResult]) = {
    val updatedThirdPartyData: ThirdPartyData = cache.form.thirdPartyData
      .updateFrom(validatorsResult)
    cache.copy(
      form = cache.form
        .copy(
          thirdPartyData = updatedThirdPartyData
        )
    )
  }

  def getSectionTitle4Ga(
    formModelOptics: FormModelOptics[Browser],
    sectionNumber: SectionNumber
  )(implicit
    messages: Messages
  ): SectionTitle4Ga = {

    val smartStringEvaluatorFactory: SmartStringEvaluatorFactory = new RealSmartStringEvaluatorFactory(englishMessages)

    val sse: SmartStringEvaluator =
      smartStringEvaluatorFactory(DataOrigin.swapDataOrigin(formModelOptics.formModelVisibilityOptics))(
        messages,
        LangADT.En
      )
    sectionTitle4GaFactory(formModelOptics.formModelRenderPageOptics.formModel(sectionNumber), sectionNumber)(sse)
  }

  def getSectionTitle4Ga(processData: ProcessData, sectionNumber: SectionNumber)(implicit
    messages: Messages
  ): SectionTitle4Ga =
    getSectionTitle4Ga(processData.formModelOptics, sectionNumber)

  private def maybeRemoveVerifiedCode(
    validatorsResult: Option[ValidatorsResult],
    formModelVisibilityOptics: FormModelVisibilityOptics,
    oldData: VariadicFormData
  ): VariadicFormData = {
    val maybeVerifiedBy: Option[FormComponentId] = validatorsResult.flatMap { result =>
      result.emailVerification.keySet
        .flatMap { emailFieldId =>
          formModelVisibilityOptics.formModel.allFormComponents
            .collectFirst {
              case IsEmailVerifier(emailId, emailVerifiedBy)
                  if emailId.baseComponentId === emailFieldId.baseComponentId =>
                emailVerifiedBy.formComponentId
            }
        }
        .toList
        .headOption
    }

    maybeVerifiedBy.map(fcId => oldData - fcId.modelComponentId).getOrElse(oldData)
  }
}
