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

import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.eq._
import org.slf4j.{ Logger, LoggerFactory }
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.fileupload.{ EnvelopeWithMapping, FileUploadAlgebra }
import uk.gov.hmrc.gform.gform
import uk.gov.hmrc.gform.gform.handlers.{ FormControllerRequestHandler, FormHandlerResult }
import uk.gov.hmrc.gform.gform.processor.FormProcessor
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.lookup.LookupExtractors
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.gform.NoSpecificAction
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.DataOrigin.Browser
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ HtmlFieldId, ValidationService }
import uk.gov.hmrc.gform.views.hardcoded.{ SaveAcknowledgement, SaveWithAccessCode }
import uk.gov.hmrc.gform.views.html.hardcoded.pages._
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

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
  formProcessor: FormProcessor,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

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
            def renderSingleton(
              singleton: Singleton[DataExpanded],
              sectionNumber: SectionNumber,
              handlerResult: FormHandlerResult
            ) = Ok {
              renderer.renderSection(
                maybeAccessCode,
                sectionNumber,
                handlerResult,
                cache.formTemplate,
                cache.form.envelopeId,
                singleton,
                formMaxAttachmentSizeMB,
                contentTypes,
                restrictedFileExtensions,
                cache.retrievals,
                cache.form.thirdPartyData.obligations,
                fastForward,
                formModelOptics
              )
            }

            def validateSections(suppressErrors: SuppressErrors, sectionNumbers: SectionNumber*)(
              f: FormHandlerResult => Result
            ) =
              handler
                .handleSuppressErrors(
                  formModelOptics,
                  sectionNumbers.toList,
                  cache.toCacheData,
                  EnvelopeWithMapping(envelope, cache.form),
                  validationService.validatePageModel _,
                  suppressErrors
                )
                .map(f)

            val formModel = formModelOptics.formModelRenderPageOptics.formModel
            val bracket = formModel.bracket(sectionNumber)

            bracket match {
              case Bracket.NonRepeatingPage(singleton, sectionNumber, _) =>
                validateSections(suppressErrors, sectionNumber)(renderSingleton(singleton, sectionNumber, _))
              case bracket @ Bracket.RepeatingPage(_, _) =>
                validateSections(suppressErrors, sectionNumber)(
                  renderSingleton(bracket.singletonForSectionNumber(sectionNumber), sectionNumber, _)
                )
              case bracket @ Bracket.AddToList(iterations, _) =>
                val iteration: Bracket.AddToListIteration[DataExpanded] =
                  bracket.iterationForSectionNumber(sectionNumber)
                val (repeater, repeaterSectionNumber) = (iteration.repeater.repeater, iteration.repeater.sectionNumber)
                val (lastRepeater, lastRepeaterSectionNumber) =
                  (iterations.last.repeater.repeater, iterations.last.repeater.sectionNumber)

                iteration.checkYourAnswers match {
                  case Some(checkYourAnswers) if checkYourAnswers.sectionNumber == sectionNumber =>
                    val visibleIteration: Bracket.AddToListIteration[Visibility] =
                      formModelOptics.formModelVisibilityOptics.formModel
                        .bracket(sectionNumber)
                        .withAddToListBracket(a => a.iterationForSectionNumber(sectionNumber))
                    validateSections(
                      SuppressErrors.No,
                      visibleIteration.allSingletonSectionNumbers: _*
                    )(handlerResult =>
                      Ok(
                        renderer.renderAddToListCheckYourAnswers(
                          checkYourAnswers.checkYourAnswers,
                          cache.formTemplate,
                          maybeAccessCode,
                          sectionNumber,
                          visibleIteration,
                          formModelOptics,
                          handlerResult.validationResult,
                          cache,
                          handlerResult.envelope
                        )
                      )
                    )
                  case _ =>
                    if (repeaterSectionNumber === sectionNumber) {
                      /*
                       * When new page is added to AddToList bracket, we need to visits repeater which
                       * has not been visited before, otherwise inflight users would be stuck in a loop.
                       */
                      val hasBeenVisited: Boolean = cache.form.visitsIndex.contains(sectionNumber.value)
                      if (sectionNumber === lastRepeaterSectionNumber || !hasBeenVisited) {
                        // display current (which happens to be last) repeater
                        validateSections(suppressErrors, sectionNumber)(handlerResult =>
                          Ok(
                            renderer.renderAddToList(
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
                          )
                        )
                      } else {
                        // We want to display last repeater
                        val sectionTitle4Ga =
                          sectionTitle4GaFactory(lastRepeater.expandedTitle, lastRepeaterSectionNumber)
                        Redirect(
                          routes.FormController
                            .form(
                              formTemplateId,
                              maybeAccessCode,
                              lastRepeaterSectionNumber,
                              sectionTitle4Ga,
                              suppressErrors,
                              FastForward.Yes
                            )
                        ).pure[Future]
                      }
                    } else {
                      // display current singleton
                      validateSections(suppressErrors, sectionNumber)(
                        renderSingleton(iteration.singleton(sectionNumber), sectionNumber, _)
                      )
                    }
                }
            }
          }
    }

  def deleteOnExit(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, noAccessCode, OperationWithForm.EditForm) {
      implicit request => l => cache => sse => formModelOptics =>
        fastForwardService.deleteForm(cache, QueryParams.empty)
    }

  def formSection(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      _ => _ => _ => _ => formModelOptics =>
        val formModel = formModelOptics.formModelRenderPageOptics.formModel
        val sectionTitle4Ga = sectionTitle4GaFactory(formModel(sectionNumber).title, sectionNumber)
        Redirect(
          routes.FormController
            .form(
              formTemplateId,
              maybeAccessCode,
              sectionNumber,
              sectionTitle4Ga,
              SuppressErrors.Yes,
              FastForward.Yes
            )
        ).pure[Future]
    }

  def backAction(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    ff: FastForward
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        def goBack(toSectionNumber: SectionNumber) = {
          val formModel = formModelOptics.formModelRenderPageOptics.formModel
          val sectionTitle4Ga = sectionTitle4GaFactory(formModel(toSectionNumber).title, sectionNumber)
          Redirect(
            routes.FormController
              .form(
                formTemplateId,
                maybeAccessCode,
                toSectionNumber,
                sectionTitle4Ga,
                SuppressErrors.Yes,
                ff
              )
          ).pure[Future]
        }

        val toSectionNumber = Navigator(
          sectionNumber,
          formModelOptics.asInstanceOf[FormModelOptics[Browser]]
        ).previousOrCurrentSectionNumber
        val formModel = formModelOptics.formModelRenderPageOptics.formModel
        val bracket = formModel.bracket(toSectionNumber)

        bracket match {
          case Bracket.NonRepeatingPage(_, _, _) => goBack(toSectionNumber)
          case Bracket.RepeatingPage(_, _)       => goBack(toSectionNumber)
          case bracket @ Bracket.AddToList(iterations, _) =>
            val iteration: Bracket.AddToListIteration[DataExpanded] =
              bracket.iterationForSectionNumber(toSectionNumber)
            val lastIteration: Bracket.AddToListIteration[DataExpanded] = iterations.last
            if (
              iteration.repeater.sectionNumber === toSectionNumber && iteration.repeater.sectionNumber < lastIteration.repeater.sectionNumber
            ) {
              val isCommited =
                formModelOptics.formModelVisibilityOptics.formModel.bracket(sectionNumber).withAddToListBracket {
                  addToListBracket =>
                    addToListBracket.iterationForSectionNumber(sectionNumber).isCommited(cache.form.visitsIndex)
                }
              if (isCommited) {
                goBack(toSectionNumber)
              } else {
                for {
                  processData <- processDataService
                                   .getProcessData[SectionSelectorType.Normal](
                                     formModelOptics.formModelRenderPageOptics.recData.variadicFormData
                                       .asInstanceOf[VariadicFormData[OutOfDate]],
                                     cache,
                                     formModelOptics,
                                     gformConnector.getAllTaxPeriods,
                                     NoSpecificAction
                                   )
                  redirect <- formProcessor.processRemoveAddToList(
                                cache,
                                maybeAccessCode,
                                FastForward.Yes,
                                formModelOptics,
                                processData,
                                bracket.iterations.size - 1,
                                bracket.source.id
                              )
                } yield redirect
              }
            } else {
              goBack(toSectionNumber)
            }
        }
    }

  def addToListAction(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    ff: FastForward,
    direction: Direction
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => _ => formModelOptics =>
        def processEditAddToList(processData: ProcessData, idx: Int, addToListId: AddToListId): Future[Result] = {

          val addToListIteration = processData.formModel.brackets.addToListById(addToListId, idx)

          def defaultNavigation(): (SectionNumber, FastForward) = {
            val firstAddToListPageSN: SectionNumber = processData.formModel.brackets
              .addToListBracket(addToListId)
              .source
              .defaultPage
              .fold(addToListIteration.firstSectionNumber) { _ =>
                addToListIteration.secondSectionNumber
              }
            (firstAddToListPageSN, FastForward.StopAt(firstAddToListPageSN.increment))
          }
          def checkYourAnswersNavigation(cya: CheckYourAnswersWithNumber[DataExpanded]): (SectionNumber, FastForward) =
            (cya.sectionNumber, FastForward.Yes)
          val (gotoSectionNumber, fastForward) =
            addToListIteration.checkYourAnswers.fold(defaultNavigation())(checkYourAnswersNavigation)
          val sectionTitle4Ga =
            sectionTitle4GaFactory(processData.formModel(gotoSectionNumber).title, gotoSectionNumber)
          Redirect(
            routes.FormController
              .form(
                formTemplateId,
                maybeAccessCode,
                gotoSectionNumber,
                sectionTitle4Ga,
                SuppressErrors.Yes,
                fastForward
              )
              .url
          ).pure[Future]
        }

        for {
          processData <- processDataService
                           .getProcessData[SectionSelectorType.Normal](
                             formModelOptics.formModelVisibilityOptics.recData.variadicFormData
                               .asInstanceOf[VariadicFormData[OutOfDate]],
                             cache,
                             formModelOptics,
                             gformConnector.getAllTaxPeriods,
                             NoSpecificAction
                           )
          res <- direction match {
                   case EditAddToList(idx, addToListId) => processEditAddToList(processData, idx, addToListId)
                   case SaveAndContinue | SaveAndExit   =>
                     // This request should have been a POST, with user data. However we have sporadically seen GET requests sent instead of POST to this endpoint
                     // the cause of which is not known yet. We redirect the user the page he/she is currently on, instead of throwing an error page
                     // e.g: GET /submissions/form/XXXX/-/0?ff=t&action=SaveAndContinue
                     logger.warn(s"Received GET request with direction $direction. Doing a redirect!")
                     val sectionTitle4Ga = formProcessor.getSectionTitle4Ga(processData, sectionNumber)
                     Redirect(
                       routes.FormController
                         .form(
                           formTemplateId,
                           maybeAccessCode,
                           sectionNumber,
                           sectionTitle4Ga,
                           SuppressErrors.Yes,
                           ff
                         )
                     ).pure[Future]
                   case _ => throw new IllegalArgumentException(s"Direction $direction is not supported here")
                 }
        } yield res
    }

  def updateFormData(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    browserSectionNumber: SectionNumber,
    fastForward: FastForward,
    save: Direction
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        processResponseDataFromBody(request, formModelOptics.formModelRenderPageOptics, Some(browserSectionNumber)) {
          requestRelatedData => variadicFormData =>
            val sectionNumber: SectionNumber =
              formModelOptics.formModelVisibilityOptics.formModel.visibleSectionNumber(browserSectionNumber)

            def processSaveAndContinue(
              processData: ProcessData
            ): Future[Result] =
              formProcessor.validateAndUpdateData(
                cache,
                processData,
                sectionNumber,
                sectionNumber,
                maybeAccessCode,
                fastForward,
                formModelOptics
              ) {
                case Some(sn) =>
                  val isFirstLanding = sectionNumber < sn
                  val sectionTitle4Ga = formProcessor.getSectionTitle4Ga(processData, sn)
                  Redirect(
                    routes.FormController
                      .form(
                        formTemplateId,
                        maybeAccessCode,
                        sn,
                        sectionTitle4Ga,
                        SuppressErrors(isFirstLanding),
                        if (isFirstLanding)
                          fastForward.next(processData.formModelOptics.formModelVisibilityOptics.formModel)
                        else
                          fastForward
                      )
                  )
                case None =>
                  Redirect(routes.SummaryController.summaryById(formTemplateId, maybeAccessCode))
              }

            def processSaveAndExit(processData: ProcessData): Future[Result] =
              formProcessor.validateAndUpdateData(
                cache,
                processData,
                sectionNumber,
                sectionNumber,
                maybeAccessCode,
                fastForward,
                formModelOptics
              ) { maybeSn =>
                val formTemplate = cache.formTemplate
                val envelopeExpiryDate = cache.form.envelopeExpiryDate
                maybeAccessCode match {
                  case Some(accessCode) =>
                    val saveWithAccessCode = new SaveWithAccessCode(formTemplate, accessCode)
                    Ok(save_with_access_code(saveWithAccessCode, frontendAppConfig))
                  case None =>
                    if (formTemplate.authConfig.isEmailAuthConfig) {
                      Redirect(gform.routes.SaveAcknowledgementController.show(formTemplateId))
                    } else {
                      showAcknowledgementPage(
                        formTemplateId,
                        maybeAccessCode,
                        processData,
                        maybeSn,
                        formTemplate,
                        envelopeExpiryDate
                      )
                    }
                }
              }

            def processBack(
              processData: ProcessData,
              commingFromSn: SectionNumber,
              sn: SectionNumber
            ): Future[Result] = {

              def goBack(saveData: Boolean) = {

                val sectionTitle4Ga = formProcessor.getSectionTitle4Ga(processData, sn)
                val goBackLink = Redirect(
                  routes.FormController
                    .form(formTemplateId, maybeAccessCode, sn, sectionTitle4Ga, SuppressErrors.Yes, fastForward)
                )

                if (saveData) {
                  formProcessor.validateAndUpdateData(
                    cache,
                    processData,
                    sn,
                    browserSectionNumber,
                    maybeAccessCode,
                    fastForward,
                    formModelOptics
                  )(_ => goBackLink)
                } else {
                  goBackLink.pure[Future]
                }
              }

              val formModel = formModelOptics.formModelRenderPageOptics.formModel
              val commingFromBracket = formModel.bracket(commingFromSn)

              val bracket = formModel.bracket(sn)

              val isAddToListRepeaterBackClick = commingFromBracket.whenAddToListBracket { addToList =>
                val iteration = addToList.iterationForSectionNumber(commingFromSn)
                iteration.repeater.sectionNumber === commingFromSn
              }

              bracket match {
                case Bracket.NonRepeatingPage(_, _, _) => goBack(!isAddToListRepeaterBackClick)
                case Bracket.RepeatingPage(_, _)       => goBack(!isAddToListRepeaterBackClick)
                case bracket @ Bracket.AddToList(iterations, _) =>
                  val iteration: Bracket.AddToListIteration[DataExpanded] = bracket.iterationForSectionNumber(sn)
                  val lastIteration: Bracket.AddToListIteration[DataExpanded] = iterations.last
                  if (
                    iteration.repeater.sectionNumber === sn && iteration.repeater.sectionNumber < lastIteration.repeater.sectionNumber
                  ) {
                    val isCommited =
                      formModelOptics.formModelVisibilityOptics.formModel.bracket(sectionNumber).withAddToListBracket {
                        addToListBracket =>
                          addToListBracket.iterationForSectionNumber(sectionNumber).isCommited(processData.visitsIndex)
                      }
                    if (isCommited) {
                      goBack(true)
                    } else {
                      formProcessor.processRemoveAddToList(
                        cache,
                        maybeAccessCode,
                        fastForward,
                        formModelOptics,
                        processData,
                        bracket.iterations.size - 1,
                        bracket.source.id
                      )
                    }
                  } else {
                    goBack(true)
                  }
              }
            }

            def handleGroup(cacheUpd: AuthCacheWithForm, processData: ProcessData, anchor: String): Future[Result] =
              formProcessor.validateAndUpdateData(
                cacheUpd,
                processData,
                sectionNumber,
                sectionNumber,
                maybeAccessCode,
                fastForward,
                formModelOptics
              ) { _ =>
                val sectionTitle4Ga = formProcessor.getSectionTitle4Ga(processData, sectionNumber)
                Redirect(
                  routes.FormController
                    .form(
                      formTemplateId,
                      maybeAccessCode,
                      sectionNumber,
                      sectionTitle4Ga,
                      SuppressErrors.Yes,
                      FastForward.Yes
                    )
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
                                          recalculation
                                        )
                res <- handleGroup(
                         cache,
                         processData.copy(formModelOptics = updFormModelOptics),
                         anchor(updFormModelOptics).map("#" + _.toHtmlId).getOrElse("")
                       )
              } yield res

            }

            def processRemoveGroup(processData: ProcessData, modelComponentId: ModelComponentId): Future[Result] = {
              val (updData, componentIdToFileId, filesToDelete) =
                GroupUtils.removeRecord(processData, modelComponentId, sectionNumber, cache.form.componentIdToFileId)
              val cacheUpd = cache.copy(form = cache.form.copy(componentIdToFileId = componentIdToFileId))
              for {
                updFormModelOptics <- FormModelOptics
                                        .mkFormModelOptics[DataOrigin.Browser, Future, SectionSelectorType.Normal](
                                          updData.asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]],
                                          cache,
                                          recalculation
                                        )
                res <- handleGroup(cacheUpd, processData.copy(formModelOptics = updFormModelOptics), "")
                _   <- fileUploadService.deleteFiles(cache.form.envelopeId, filesToDelete)
              } yield res
            }

            for {
              processData <- processDataService
                               .getProcessData[SectionSelectorType.Normal](
                                 variadicFormData,
                                 cache,
                                 formModelOptics,
                                 gformConnector.getAllTaxPeriods,
                                 NoSpecificAction
                               )
              res <- save match {
                       case SaveAndContinue => processSaveAndContinue(processData)
                       case SaveAndExit     => processSaveAndExit(processData)
                       case Back =>
                         processBack(
                           processData,
                           sectionNumber,
                           Navigator(
                             sectionNumber,
                             processData.formModelOptics
                           ).previousOrCurrentSectionNumber
                         )
                       case AddGroup(modelComponentId)    => processAddGroup(processData, modelComponentId)
                       case RemoveGroup(modelComponentId) => processRemoveGroup(processData, modelComponentId)
                       case _                             => throw new IllegalArgumentException(s"Direction $save is not supported here")
                     }
            } yield res
        }
    }

  private def showAcknowledgementPage(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    processData: ProcessData,
    maybeSn: Option[SectionNumber],
    formTemplate: FormTemplate,
    envelopeExpiryDate: Option[EnvelopeExpiryDate]
  )(implicit request: Request[AnyContent], lang: LangADT) = {
    val call = maybeSn match {
      case Some(sn) =>
        val sectionTitle4Ga = formProcessor.getSectionTitle4Ga(processData, sn)
        routes.FormController
          .form(formTemplateId, None, sn, sectionTitle4Ga, SuppressErrors.Yes, FastForward.Yes)
      case None => routes.SummaryController.summaryById(formTemplateId, maybeAccessCode)
    }
    val saveAcknowledgement = new SaveAcknowledgement(formTemplate, envelopeExpiryDate)
    Ok(save_acknowledgement(saveAcknowledgement, call, frontendAppConfig))
  }

  private val formMaxAttachmentSizeMB = appConfig.formMaxAttachmentSizeMB
  private val contentTypes = appConfig.contentTypes
  private val restrictedFileExtensions = appConfig.restrictedFileExtensions
}
