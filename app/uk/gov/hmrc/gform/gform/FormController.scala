/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.instances.future._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.foldable._
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.fileupload.FileUploadAlgebra
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.lookup.LookupExtractors
import uk.gov.hmrc.gform.models.{ AddToListUtils, Bracket, DataExpanded, FastForward, FormModel, ProcessData, ProcessDataService, SectionSelectorType, Singleton }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.gform.{ FormValidationOutcome, NoSpecificAction }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ HtmlFieldId, ValidationService }
import uk.gov.hmrc.gform.views.html.hardcoded.pages._
import uk.gov.hmrc.gform.views.hardcoded.{ SaveAcknowledgement, SaveWithAccessCode }
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class FormController(
  appConfig: AppConfig,
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  fileUploadService: FileUploadAlgebra[Future],
  validationService: ValidationService,
  renderer: SectionRenderingService,
  gformConnector: GformConnector,
  processDataService: ProcessDataService[Future],
  handler: FormControllerRequestHandler,
  lookupExtractors: LookupExtractors,
  fastForwardService: FastForwardService,
  recalculation: Recalculation[Future, Throwable],
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  private val noAccessCode = Option.empty[AccessCode]

  implicit val frontendConfig: FrontendAppConfig = frontendAppConfig

  // TODO: this method should really be in the SignOutController which does not yet exist
  def keepAlive() = auth.keepAlive()

  def form(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    browserSectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    suppressErrors: SuppressErrors,
    fastForward: FastForward
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        val sectionNumber: SectionNumber =
          formModelOptics.formModelVisibilityOptics.formModel.visibleSectionNumber(browserSectionNumber)
        fileUploadService
          .getEnvelope(cache.form.envelopeId)
          .flatMap { envelope =>
            handler
              .handleSuppressErrors(
                formModelOptics,
                sectionNumber,
                cache.toCacheData,
                envelope,
                validationService.validatePageModel,
                suppressErrors
              )
          }
          .map { handlerResult =>
            def renderSingleton(singleton: Singleton[DataExpanded], sectionNumber: SectionNumber) = Ok {
              renderer.renderSection(
                maybeAccessCode,
                cache.form,
                sectionNumber,
                handlerResult,
                cache.formTemplate,
                cache.form.envelopeId,
                singleton,
                formMaxAttachmentSizeMB,
                contentTypes,
                cache.retrievals,
                cache.form.thirdPartyData.obligations,
                fastForward,
                formModelOptics
              )
            }

            val formModel = formModelOptics.formModelRenderPageOptics.formModel
            val bracket = formModel.bracket(sectionNumber)

            bracket match {
              case Bracket.NonRepeatingPage(singleton, sectionNumber, _) =>
                renderSingleton(singleton, sectionNumber)
              case bracket @ Bracket.RepeatingPage(_, _) =>
                val singleton = bracket.singletonForSectionNumber(sectionNumber)
                renderSingleton(singleton, sectionNumber)
              case bracket @ Bracket.AddToList(iterations, _) =>
                val iteration: Bracket.AddToListIteration[DataExpanded] =
                  bracket.iterationForSectionNumber(sectionNumber)
                val repeaterWithNumber = iteration.repeater
                val repeater = repeaterWithNumber.repeater
                val repeaterSectionNumber = repeaterWithNumber.sectionNumber
                val lastRepeaterWithNumber = iterations.last.repeater
                val lastRepeater = lastRepeaterWithNumber.repeater
                val lastRepeaterSectionNumber = lastRepeaterWithNumber.sectionNumber
                if (repeaterSectionNumber === sectionNumber) {
                  if (sectionNumber === lastRepeaterSectionNumber) {
                    // display current (which happens to be last) repeater
                    val html = renderer.renderAddToList(
                      repeater,
                      bracket,
                      formModel,
                      maybeAccessCode,
                      cache.form,
                      sectionNumber,
                      formModelOptics,
                      cache.formTemplate,
                      handlerResult.validationResult,
                      cache.retrievals
                    )
                    Ok(html)
                  } else {
                    // We want to display last repeater
                    val sectionTitle4Ga = sectionTitle4GaFactory(lastRepeater.expandedTitle, lastRepeaterSectionNumber)
                    Redirect(
                      routes.FormController
                        .form(
                          formTemplateId,
                          maybeAccessCode,
                          lastRepeaterSectionNumber,
                          sectionTitle4Ga,
                          suppressErrors,
                          FastForward.Yes))
                  }
                } else {
                  // display current singleton
                  val singleton = iteration.singleton(sectionNumber)
                  renderSingleton(singleton, sectionNumber)
                }

            }
          }
    }

  def deleteOnExit(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, noAccessCode, OperationWithForm.EditForm) {
      implicit request => l => cache => sse => formModelOptics =>
        fastForwardService.deleteForm(cache, QueryParams.empty)
    }

  def updateFormData(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    browserSectionNumber: SectionNumber,
    fastForward: FastForward
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        processResponseDataFromBody(request, formModelOptics.formModelRenderPageOptics.formModel) {
          requestRelatedData => variadicFormData =>
            val sectionNumber: SectionNumber =
              formModelOptics.formModelVisibilityOptics.formModel.visibleSectionNumber(browserSectionNumber)
            def getSectionTitle4Ga(processData: ProcessData, sectionNumber: SectionNumber): SectionTitle4Ga =
              sectionTitle4GaFactory(processData.formModel(sectionNumber).title, sectionNumber)

            def validateAndUpdateData(
              cache: AuthCacheWithForm,
              processData: ProcessData,
              sectionNumber: SectionNumber
            )(
              toResult: Option[SectionNumber] => Result
            ): Future[Result] =
              for {
                envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)
                FormValidationOutcome(_, formData, validatorsResult) <- handler.handleFormValidation(
                                                                         processData.formModelOptics,
                                                                         sectionNumber,
                                                                         cache.toCacheData,
                                                                         envelope,
                                                                         validationService.validatePageModel
                                                                       )
                res <- {

                  val oldData: VariadicFormData[SourceOrigin.Current] = processData.formModelOptics.pageOpticsData

                  val formDataU = oldData.toFormData ++ formData
                  val before: ThirdPartyData = cache.form.thirdPartyData
                  val after: ThirdPartyData = before.updateFrom(validatorsResult)

                  val needsSecondPhaseRecalculation =
                    (before.desRegistrationResponse, after.desRegistrationResponse).mapN(_ =!= _)

                  val visitsIndex = processData.visitsIndex.visit(sectionNumber)

                  val cacheUpd =
                    cache.copy(
                      form = cache.form
                        .copy(
                          thirdPartyData = after.copy(obligations = processData.obligations),
                          formData = formDataU,
                          visitsIndex = visitsIndex))

                  if (needsSecondPhaseRecalculation.getOrElse(false)) {
                    val newDataRaw = cache.variadicFormData[SectionSelectorType.Normal]
                    for {
                      newProcessData <- processDataService
                                         .getProcessData[SectionSelectorType.Normal](
                                           newDataRaw,
                                           cacheUpd,
                                           formModelOptics,
                                           gformConnector.getAllTaxPeriods,
                                           NoSpecificAction)
                      result <- validateAndUpdateData(cacheUpd, newProcessData, sectionNumber)(toResult) // recursive call
                    } yield result
                  } else {
                    fastForwardService
                      .updateUserData(
                        cacheUpd,
                        processData.copy(visitsIndex = visitsIndex),
                        maybeAccessCode,
                        fastForward,
                        envelope)(toResult)
                  }
                }
              } yield res

            def processSaveAndContinue(
              processData: ProcessData
            ): Future[Result] =
              validateAndUpdateData(cache, processData, sectionNumber) {
                case Some(sn) =>
                  val sectionTitle4Ga = getSectionTitle4Ga(processData, sn)
                  Redirect(
                    routes.FormController
                      .form(
                        formTemplateId,
                        maybeAccessCode,
                        sn,
                        sectionTitle4Ga,
                        SuppressErrors(sectionNumber < sn),
                        fastForward.next))
                case None =>
                  Redirect(routes.SummaryController.summaryById(formTemplateId, maybeAccessCode))
              }

            def processSaveAndExit(processData: ProcessData): Future[Result] =
              validateAndUpdateData(cache, processData, sectionNumber) { maybeSn =>
                val formTemplate = cache.formTemplate
                val envelopeExpiryDate = cache.form.envelopeExpiryDate
                maybeAccessCode match {
                  case Some(accessCode) =>
                    val saveWithAccessCode = new SaveWithAccessCode(formTemplate, accessCode)
                    Ok(save_with_access_code(saveWithAccessCode, frontendAppConfig))
                  case None =>
                    val call = maybeSn match {
                      case Some(sn) =>
                        val sectionTitle4Ga = getSectionTitle4Ga(processData, sn)
                        routes.FormController
                          .form(formTemplateId, None, sn, sectionTitle4Ga, SuppressErrors.Yes, FastForward.Yes)
                      case None => routes.SummaryController.summaryById(formTemplateId, maybeAccessCode)
                    }
                    val saveAcknowledgement = new SaveAcknowledgement(formTemplate, envelopeExpiryDate)
                    Ok(save_acknowledgement(saveAcknowledgement, call, frontendAppConfig))
                }
              }

            def processBack(processData: ProcessData, sn: SectionNumber): Future[Result] = {
              def goBack = validateAndUpdateData(cache, processData, sn) { _ =>
                val sectionTitle4Ga = getSectionTitle4Ga(processData, sn)
                Redirect(
                  routes.FormController
                    .form(formTemplateId, maybeAccessCode, sn, sectionTitle4Ga, SuppressErrors.Yes, FastForward.Yes))
              }

              val formModel = formModelOptics.formModelRenderPageOptics.formModel
              val bracket = formModel.bracket(sn)

              bracket match {
                case Bracket.NonRepeatingPage(_, _, _) => goBack
                case Bracket.RepeatingPage(_, _)       => goBack
                case bracket @ Bracket.AddToList(iterations, _) =>
                  val iteration: Bracket.AddToListIteration[DataExpanded] = bracket.iterationForSectionNumber(sn)
                  val lastIteration: Bracket.AddToListIteration[DataExpanded] = iterations.last
                  if (iteration.repeater.sectionNumber === sn && iteration.repeater.sectionNumber < lastIteration.repeater.sectionNumber) {
                    val isCommited =
                      lastIteration.isCommited(formModelOptics.formModelVisibilityOptics, processData.visitsIndex)
                    if (isCommited) {
                      goBack
                    } else {
                      processRemoveAddToList(processData, bracket.iterations.size - 1, bracket.source.id)
                    }
                  } else {
                    goBack
                  }
              }
            }

            def handleGroup(processData: ProcessData, anchor: String): Future[Result] =
              validateAndUpdateData(cache, processData, sectionNumber) { _ =>
                val sectionTitle4Ga = getSectionTitle4Ga(processData, sectionNumber)
                Redirect(
                  routes.FormController
                    .form(
                      formTemplateId,
                      maybeAccessCode,
                      sectionNumber,
                      sectionTitle4Ga,
                      SuppressErrors.Yes,
                      FastForward.Yes)
                    .url + anchor
                )
              }

            def processAddGroup(processData: ProcessData, modelComponentId: ModelComponentId): Future[Result] = {

              val incremented = modelComponentId.increment

              def anchor(formModelOptics: FormModelOptics[DataOrigin.Browser]): Option[HtmlFieldId] = {
                val childs: List[FormComponent] =
                  formModelOptics.formModelVisibilityOptics.fcLookup
                    .get(incremented.toFormComponentId)
                    .toList
                    .flatMap(_.childrenFormComponents)

                childs
                  .dropWhile {
                    case IsInformationMessage(_) => true
                    case _                       => false
                  }
                  .headOption
                  .map {
                    case fc @ IsChoice(_)          => HtmlFieldId.indexed(fc.id, 0)
                    case fc @ IsRevealingChoice(_) => HtmlFieldId.indexed(fc.id, 0)
                    case fc =>
                      HtmlFieldId.pure(fc.multiValueId.fold[ModelComponentId](_.modelComponentId)(_.atoms.head))
                  }
              }

              val variadicFormData = processData.formModelOptics.pageOpticsData
              val updatedVariadicFormData = variadicFormData.addOne(incremented -> "")
              for {
                updFormModelOptics <- FormModelOptics
                                       .mkFormModelOptics[DataOrigin.Browser, Future, SectionSelectorType.Normal](
                                         updatedVariadicFormData
                                           .asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]],
                                         cache,
                                         recalculation)
                res <- handleGroup(
                        processData.copy(formModelOptics = updFormModelOptics),
                        anchor(updFormModelOptics).map("#" + _.toHtmlId).getOrElse(""))
              } yield res

            }

            def processRemoveGroup(processData: ProcessData, modelComponentId: ModelComponentId): Future[Result] = {

              val formModelRenderPageOptics = processData.formModelOptics.formModelRenderPageOptics
              val data = processData.formModelOptics.pageOpticsData

              val maybeFormComponent = formModelRenderPageOptics.find(modelComponentId)
              val dataToRemove = maybeFormComponent.map(data.by).getOrElse(VariadicFormData.empty)
              val indexedComponentId = modelComponentId.indexedComponentId
              val toToReindexed: List[FormComponent] = formModelRenderPageOptics.findBigger(indexedComponentId)

              val variadicFormDatas: VariadicFormData[SourceOrigin.Current] = toToReindexed.foldMap(data.by)

              val decrementedVariadicFormDatas: VariadicFormData[SourceOrigin.Current] =
                variadicFormDatas.mapKeys(_.decrement)

              val updData = data -- dataToRemove -- variadicFormDatas ++ decrementedVariadicFormDatas

              for {
                updFormModelOptics <- FormModelOptics
                                       .mkFormModelOptics[DataOrigin.Browser, Future, SectionSelectorType.Normal](
                                         updData.asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]],
                                         cache,
                                         recalculation)
                res <- handleGroup(processData.copy(formModelOptics = updFormModelOptics), "")
              } yield res

            }

            def processEditAddToList(processData: ProcessData, idx: Int, addToListId: AddToListId): Future[Result] = {

              val addToListItration = processData.formModel.brackets.addToListById(addToListId, idx)

              val firstAddToListPage = addToListItration.firstSectionNumber
              val next = firstAddToListPage.increment

              val sectionTitle4Ga = getSectionTitle4Ga(processData, firstAddToListPage)
              Redirect(
                routes.FormController
                  .form(
                    formTemplateId,
                    maybeAccessCode,
                    firstAddToListPage,
                    sectionTitle4Ga,
                    SuppressErrors.Yes,
                    FastForward.StopAt(next)))
                .pure[Future]
            }

            def processRemoveAddToList(processData: ProcessData, idx: Int, addToListId: AddToListId): Future[Result] = {

              def saveAndRedirect(updFormModelOptics: FormModelOptics[DataOrigin.Browser]): Future[Result] = {
                val updFormModel: FormModel[DataExpanded] = updFormModelOptics.formModelRenderPageOptics.formModel

                val sn = updFormModel.brackets.addToListBracket(addToListId).lastSectionNumber

                val visitsIndex = VisitIndex
                  .updateSectionVisits(updFormModel, processData.formModel, processData.visitsIndex)

                val processDataUpd = processData.copy(
                  formModelOptics = updFormModelOptics,
                  visitsIndex = VisitIndex(visitsIndex)
                )

                val cacheUpd = cache.copy(form = cache.form.copy(visitsIndex = VisitIndex(visitsIndex)))

                validateAndUpdateData(cacheUpd, processDataUpd, sn) { _ =>
                  val sectionTitle4Ga = getSectionTitle4Ga(processDataUpd, sn)
                  Redirect(
                    routes.FormController
                      .form(formTemplateId, maybeAccessCode, sn, sectionTitle4Ga, SuppressErrors.Yes, FastForward.Yes))
                }
              }

              val bracket = formModelOptics.formModelRenderPageOptics.formModel.brackets.addToListBracket(addToListId)
              val updData: VariadicFormData[SourceOrigin.Current] =
                AddToListUtils.removeRecord(processData, bracket, idx)

              for {
                updFormModelOptics <- FormModelOptics
                                       .mkFormModelOptics[DataOrigin.Browser, Future, SectionSelectorType.Normal](
                                         updData.asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]],
                                         cache,
                                         recalculation)
                redirect <- saveAndRedirect(updFormModelOptics)
              } yield redirect
            }

            for {
              processData <- processDataService
                              .getProcessData[SectionSelectorType.Normal](
                                variadicFormData,
                                cache,
                                formModelOptics,
                                gformConnector.getAllTaxPeriods,
                                NoSpecificAction)
              nav = Navigator(sectionNumber, requestRelatedData, processData.formModelOptics).navigate
              res <- nav match {
                      case SaveAndContinue                   => processSaveAndContinue(processData)
                      case SaveAndExit                       => processSaveAndExit(processData)
                      case Back(sn)                          => processBack(processData, sn)
                      case AddGroup(modelComponentId)        => processAddGroup(processData, modelComponentId)
                      case RemoveGroup(modelComponentId)     => processRemoveGroup(processData, modelComponentId)
                      case RemoveAddToList(idx, addToListId) => processRemoveAddToList(processData, idx, addToListId)
                      case EditAddToList(idx, addToListId)   => processEditAddToList(processData, idx, addToListId)
                    }
            } yield res
        }
    }

  private val formMaxAttachmentSizeMB = appConfig.formMaxAttachmentSizeMB
  private val contentTypes = appConfig.contentTypes
}
