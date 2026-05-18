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
import play.api.mvc.Results.Redirect
import play.api.mvc.Result
import uk.gov.hmrc.gform.addresslookup.{ AddressLookupResult, AddressLookupService }
import uk.gov.hmrc.gform.api.{ BankAccountInsightsConnector, CompanyInformationConnector, DelegatedAgentAuthConnector, NinoInsightsConnector }
import uk.gov.hmrc.gform.bars.BankAccountReputationConnector
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.FileIdsWithMapping
import uk.gov.hmrc.gform.eval.smartstring.{ RealSmartStringEvaluatorFactory, SmartStringEvaluationSyntax, SmartStringEvaluator, SmartStringEvaluatorFactory }
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gform.{ DataRetrieveService, FastForwardService, FileSystemConnector, routes }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.gform.{ FormValidationOutcome, NoSpecificAction }
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId, ModelPageId }
import uk.gov.hmrc.gform.models.optics.DataOrigin.{ Browser, Mongo }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.objectStore.{ EnvelopeWithMapping, ObjectStoreAlgebra }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber.Classic.AddToListPage.TerminalPageKind
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
  englishMessages: Messages
)(implicit ec: ExecutionContext) {

  def processRemoveAddToList(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    fastForward: List[FastForward],
    formModelOptics: FormModelOptics[Mongo],
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
      updFormModelOptics: FormModelOptics[DataOrigin.Browser],
      componentIdToFileId: FormComponentIdToFileIdMapping,
      postcodeLookupIds: Set[FormComponentId],
      dataRetrieveIds: Set[DataRetrieveId]
    ): Future[Result] = {
      val updFormModel: FormModel[DataExpanded] = updFormModelOptics.formModelRenderPageOptics.formModel

      val sn = updFormModel.brackets.addToListBracket(addToListId).lastSectionNumber

      val addToListBracket: Bracket.AddToList[DataExpanded] =
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
        val pageIdSectionNumberMap = updFormModel.pageIdSectionNumberMap
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
      .mkFormModelOptics[DataOrigin.Browser, SectionSelectorType.Normal](
        updData.asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]],
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
    pageModel: PageModel[Visibility],
    visitsIndex: VisitIndex,
    formModelOptics: FormModelOptics[Mongo],
    enteredVariadicFormData: EnteredVariadicFormData,
    sectionNumber: SectionNumber
  ): VisitIndex = {
    val formComponentsUpdated: Set[ModelComponentId] =
      getComponentsWithUpdatedValues(formModelOptics.pageOpticsData.data, enteredVariadicFormData.userData.data)

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
            formModelOptics.formModelVisibilityOptics.formModel.pageIdSectionNumberMap
              .get(pageIdWithMaybeIndex.modelPageId)

          if (maybeSectionNumber.isDefined)
            maybeSectionNumber
          else
            formModelOptics.formModelVisibilityOptics.formModel.pageIdSectionNumberMap.get(pageId.modelPageId)
        }.toSet
    }

    val additionalSections =
      if (formComponentsUpdated.nonEmpty && sectionNumber.isAddToList && !sectionNumber.isAddToListTerminalPage) {
        val bracket = formModelOptics.formModelVisibilityOptics.formModel.bracket(sectionNumber)
        bracket match {
          case bracket @ Bracket.AddToList(_, _) =>
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

  private case class PopulateAtlData(fields: Seq[FormField], count: Int)

  private def populateAtlWithFormData(
    populateATL: PopulateATL,
    formModelVisibilityOptics: FormModelVisibilityOptics[Browser],
    authCacheWithForm: AuthCacheWithForm
  )(implicit
    messages: Messages,
    langADT: LangADT,
    hc: HeaderCarrier
  ): (PopulateAtlData, FormModelVisibilityOptics[Browser]) = {
    val seq = populateATL.mapping.toSeq

    val fields = seq.map { case (atlComponentName, expr) =>
      val bcId = BaseComponentId(atlComponentName)

      val atlValues = expr match {
        case DataRetrieveCtx(dataRetrieveId, attribute) =>
          val dataRetrieveData =
            formModelVisibilityOptics.recalculationResult.evaluationContext.thirdPartyData.dataRetrieve.get
              .getOrElse(
                dataRetrieveId,
                throw new RuntimeException("Could not retrieve dataRetrieve data for populateATL")
              )
          dataRetrieveData.data match {
            case RetrieveDataType.ObjectType(data) => Seq(data(attribute))
            case RetrieveDataType.ListType(data)   => data.map(attrToValue => attrToValue(attribute))
          }
        case _ => throw new RuntimeException(s"$expr did not match for populateATL evaluation")
      }

      val defaultPageBaseComponentId = BaseComponentId("dp1") //TODO: Lookup properly
      val addAnotherQuestionBaseComponentId = BaseComponentId("ownerFc") //TODO: Lookup properly

      val atlsToPopulateCount = atlValues.length
      val defaultAndAAQFormFields = (1 until atlsToPopulateCount).foldLeft(
        Seq(
          FormField(
            ModelComponentId.pure(IndexedComponentId.indexed(defaultPageBaseComponentId, 1)),
            ""
          )
        )
      ) { case (acc, i) =>
        acc :+ FormField(
          ModelComponentId.pure(IndexedComponentId.indexed(addAnotherQuestionBaseComponentId, i)),
          "0"
        )
      }
      defaultAndAAQFormFields ++ atlValues.zipWithIndex
        .map { case (value, i) =>
          FormField(
            ModelComponentId.pure(IndexedComponentId.indexed(bcId, i + 1)),
            value
          )
        } -> atlsToPopulateCount
    }

    val populateAtlData = fields.foldLeft(PopulateAtlData(Seq(), 0)) {
      case (PopulateAtlData(accFields, accCount), (fields, count)) =>
        PopulateAtlData(accFields ++ fields, accCount + count)
    }

    val updatedVariadicFormData = populateAtlData.fields
      .foldLeft(formModelVisibilityOptics.recData.variadicFormData) { case (acc, field) =>
        acc.addMany(field.id -> Seq(field.value))
      }

    val newVisOptics = FormModelOptics
      .mkFormModelOptics[Browser, SectionSelectorType.Normal](
        updatedVariadicFormData
          .asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]],
        authCacheWithForm
      )
      .formModelVisibilityOptics

    populateAtlData -> newVisOptics
  }

  private def retrieveWithState(
    dataRetrieve: DataRetrieve,
    visibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser],
    cache: AuthCacheWithForm
  )(implicit
    message: Messages,
    hc: HeaderCarrier,
    langADT: LangADT
  ): Future[(Option[DataRetrieveResult], FormModelVisibilityOptics[DataOrigin.Browser], Option[PopulateAtlData])] = {
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
      val populateAtlData = dataRetrieve.populateATL
        .map(populateAtl => populateAtlWithFormData(populateAtl, visOptics, cache)(message, langADT, hc))

      val populateAtlVisOptics = populateAtlData match {
        case Some(value) => value._2
        case None        => visOptics
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
  ): Future[(List[DataRetrieveResult], FormModelVisibilityOptics[Browser], Seq[PopulateAtlData])] =
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
        processData.formModel.dataRetrieveAll.lookup.toList.filterNot(_._2.callOnNoChange).flatMap {
          case (drId, dataRetrieve) =>
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
              processData.formModelOptics.formModelVisibilityOptics,
              Seq(): Seq[PopulateAtlData]
            )
          )
        ) { case (acc, r) =>
          acc.flatMap {
            case (results, optics, populateAtlDataSeq) if r.`if`.forall(optics.evalIncludeIfExpr(_, None)) =>
              retrieveWithState(r, optics, cache)(messages, implicitly, implicitly).map {
                case (Some(result), updatedOptics, populateAtlData) =>
                  (results :+ result, updatedOptics, populateAtlDataSeq ++ populateAtlData)
                case (None, _, _) =>
                  (results, optics, populateAtlDataSeq)
              }
            case (results, optics, populateAtlDataSeq) => Future.successful((results, optics, populateAtlDataSeq))
          }
        }
    } else Future.successful((List(), processData.formModelOptics.formModelVisibilityOptics, Seq()))

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
    toResult: Option[(FormComponentId, AddressLookupResult)] => Option[
      String
    ] => SectionOrSummary => FormModelVisibilityOptics[Browser] => Result
  )(implicit hc: HeaderCarrier, l: LangADT, sse: SmartStringEvaluator, messages: Messages): Future[Result] = {

    val formModelVisibilityOptics = processData.formModelOptics.formModelVisibilityOptics
    val pageModel: PageModel[Visibility] =
      formModelVisibilityOptics.formModel(sectionNumber)

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
      (dataRetrieveResult, updatedFormVisibilityOptics, populateAtlData) <-
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
                                     DataOrigin.swapDataOrigin(processData.formModelOptics),
                                     validationService
                                   )
                                 } else TaskIdTaskStatusMapping.empty.pure[Future]

      res <- {
        val oldData: VariadicFormData[SourceOrigin.Current] = maybeRemoveVerifiedCode(
          validatorsResult,
          formModelVisibilityOptics,
          processData.formModelOptics.pageOpticsData
        )

        val (upToDateOptics, dataRetrievesToRemove) = processData.formModel.dataRetrieveAll.lookup.toList
          .foldLeft[(FormModelVisibilityOptics[DataOrigin.Browser], List[DataRetrieveId])](
            updatedFormVisibilityOptics -> List.empty[DataRetrieveId]
          ) { case ((optics, removeList), (drId, dataRetrieve)) =>
            if (!dataRetrieve.`if`.forall(optics.evalIncludeIfExpr(_, None)))
              optics.removeDataRetrieveResults(List(drId)) -> (removeList :+ drId)
            else optics                                    -> removeList
          }
        val populateAtlFields = populateAtlData.flatMap(_.fields)
        val populateAtlFormData = FormData(populateAtlFields.toList)
        val formDataU = oldData.toFormData ++ formData ++ populateAtlFormData
        val updatedThirdPartyData: ThirdPartyData = updatedCache.form.thirdPartyData
          .updateFrom(validatorsResult)
          .updateDataRetrieve(dataRetrieveResult)
          .removeDataRetrieves(dataRetrievesToRemove)
          .updatePostcodeLookup(updatePostcodeLookup)
          .updateConfirmations(processData.confirmations)

        val redirectUrl = if (isValid && pageModel.redirects.nonEmpty) {
          pageModel.redirects.collectFirst {
            case redirect if upToDateOptics.evalIncludeIfExpr(redirect.`if`, None) =>
              redirect.redirectUrl
          }
        } else None

        val visitsIndex =
          if (visitPage && redirectUrl.isEmpty)
            processData.visitsIndex.visit(sectionNumber)
          else processData.visitsIndex.unvisit(sectionNumber)

        val updatedVisitsIndex =
          checkForRevisits(pageModel, visitsIndex, formModelOptics, enteredVariadicFormData, sectionNumber)

        val populateAtlDataWithTemplateIndex = populateAtlData.map { populateAtlData =>
          val fm = updatedFormVisibilityOptics.formModel
          populateAtlData -> populateAtlData.fields
            .flatMap { case FormField(mcId, value) =>
              fm.sectionNumberLookup.get(mcId.toFormComponentId).map(_.templateSectionIndex)
            }
            .headOption
            .head
        }

        val visitedPopulateAtlPagesVisitsIndex =
          populateAtlDataWithTemplateIndex.foldLeft(updatedVisitsIndex) { case (acc, (atlData, atlSection)) =>
            val visitedDefault = acc.visit(SectionNumber.Classic.AddToListPage.DefaultPage(atlSection))
            val fm = updatedFormVisibilityOptics.formModel
            val numberOfAtlPages = fm.addToListSectionNumbers.count {
              case classic: SectionNumber.Classic                     => classic.sectionIndex == atlSection
              case SectionNumber.TaskList(coordinates, sectionNumber) => sectionNumber.sectionIndex == atlSection
            }

            (0 until numberOfAtlPages - 2) //-2 excludes default and add another question pages.
              .foldLeft(visitedDefault) { case (acc, atlPageIndex) =>
                (1 to atlData.count).foldLeft(acc) { case (acc, iterationNumber) =>
                  acc
                    .visit(SectionNumber.Classic.AddToListPage.Page(atlSection, iterationNumber, atlPageIndex))
                    .visit(
                      SectionNumber.Classic.AddToListPage
                        .TerminalPage(atlSection, iterationNumber, TerminalPageKind.RepeaterPage)
                    )
                }
              }
          }

        val cacheUpd =
          cache.copy(
            form = cache.form
              .copy(
                thirdPartyData = updatedThirdPartyData.copy(obligations = processData.obligations),
                formData = formDataU,
                visitsIndex = visitedPopulateAtlPagesVisitsIndex,
                taskIdTaskStatus = taskIdTaskStatusMapping
              )
          )

        val newDataRaw = cacheUpd.variadicFormData[SectionSelectorType.Normal]
        for {
          newProcessData <- processDataService
                              .getProcessData[SectionSelectorType.Normal](
                                newDataRaw,
                                cacheUpd,
                                gformConnector.getAllTaxPeriods,
                                NoSpecificAction,
                                formModelOptics
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
                  updatedFormVisibilityOptics
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
    formModelVisibilityOptics: FormModelVisibilityOptics[Browser],
    formModel: FormModel[DataExpanded],
    sectionNumber: SectionNumber
  )(implicit
    messages: Messages
  ): SectionTitle4Ga = {

    val smartStringEvaluatorFactory: SmartStringEvaluatorFactory = new RealSmartStringEvaluatorFactory(englishMessages)

    val sse: SmartStringEvaluator =
      smartStringEvaluatorFactory(DataOrigin.swapDataOrigin(formModelVisibilityOptics))(messages, LangADT.En)
    sectionTitle4GaFactory(formModel(sectionNumber), sectionNumber)(sse)
  }

  def getSectionTitle4Ga(processData: ProcessData, sectionNumber: SectionNumber)(implicit
    messages: Messages
  ): SectionTitle4Ga =
    getSectionTitle4Ga(processData.formModelOptics.formModelVisibilityOptics, processData.formModel, sectionNumber)

  private def maybeRemoveVerifiedCode(
    validatorsResult: Option[ValidatorsResult],
    formModelVisibilityOptics: FormModelVisibilityOptics[Browser],
    oldData: VariadicFormData[SourceOrigin.Current]
  ): VariadicFormData[SourceOrigin.Current] = {
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
