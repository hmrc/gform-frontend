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

import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.eq._
import org.slf4j.{ Logger, LoggerFactory }
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.objectStore.{ EnvelopeWithMapping, ObjectStoreAlgebra }
import uk.gov.hmrc.gform.gform
import uk.gov.hmrc.gform.gform.handlers.{ FormControllerRequestHandler, FormHandlerResult }
import uk.gov.hmrc.gform.gform.processor.FormProcessor
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.gform.NoSpecificAction
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.summary.AddressRecordLookup
import uk.gov.hmrc.gform.upscan.{ UpscanAlgebra, UpscanInitiate }
import uk.gov.hmrc.gform.validation.{ HtmlFieldId, ValidationService }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class FormController(
  appConfig: AppConfig,
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  objectStoreAlgebra: ObjectStoreAlgebra[Future],
  upscanService: UpscanAlgebra[Future],
  validationService: ValidationService,
  renderer: SectionRenderingService,
  gformConnector: GformConnector,
  processDataService: ProcessDataService[Future],
  handler: FormControllerRequestHandler,
  fastForwardService: FastForwardService,
  recalculation: Recalculation[Future, Throwable],
  formProcessor: FormProcessor,
  confirmationService: ConfirmationService,
  messagesControllerComponents: MessagesControllerComponents,
  auditService: AuditService
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  import i18nSupport._

  private val noAccessCode = Option.empty[AccessCode]

  implicit val frontendConfig: FrontendAppConfig = frontendAppConfig

  def keepAlive() = Action(_ => Ok)
  // TODO: this method should really be in the SignOutController which does not yet exist
  def refreshSession(formTemplateId: FormTemplateId) = auth.refreshSession(formTemplateId)

  def form(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    browserSectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    suppressErrors: SuppressErrors,
    rawFastForward: List[FastForward]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        val formModel = formModelOptics.formModelVisibilityOptics.formModel
        val fastForward = filterFastForward(browserSectionNumber, rawFastForward, formModel)
        val sectionNumber: SectionNumber = formModel.visibleSectionNumber(browserSectionNumber)
        objectStoreAlgebra
          .getEnvelope(cache.form.envelopeId)
          .flatMap { envelope =>
            def renderSingleton(
              singleton: Singleton[DataExpanded],
              sectionNumber: SectionNumber,
              handlerResult: FormHandlerResult
            ) = {
              val defaultMaxFileSize = cache.formTemplate.fileSizeLimit.getOrElse(formMaxAttachmentSizeMB)
              val fileUploadIds = singleton.upscanInitiateRequests(defaultMaxFileSize)
              for {
                upscanInitiate <- if (fileUploadIds.isEmpty) UpscanInitiate.empty.pure[Future]
                                  else
                                    upscanService.upscanInitiate(
                                      fileUploadIds,
                                      cache.formTemplateId,
                                      sectionNumber,
                                      cache.form,
                                      FormIdData(cache, maybeAccessCode)
                                    )
              } yield Ok(
                renderer.renderSection(
                  maybeAccessCode,
                  sectionNumber,
                  handlerResult,
                  cache.formTemplate,
                  cache.formTemplateContext.specimenSource,
                  cache.form.envelopeId,
                  singleton,
                  cache.formTemplate.fileSizeLimit.getOrElse(formMaxAttachmentSizeMB),
                  cache.formTemplate.allowedFileTypes,
                  restrictedFileExtensions,
                  cache.retrievals,
                  cache.form.thirdPartyData.obligations,
                  fastForward,
                  formModelOptics,
                  upscanInitiate,
                  AddressRecordLookup.from(cache.form.thirdPartyData)
                )
              )
            }

            def validateSections(suppressErrors: SuppressErrors, sectionNumbers: SectionNumber*)(
              f: FormHandlerResult => Future[Result]
            ): Future[Result] =
              handler
                .handleSuppressErrors(
                  formModelOptics,
                  sectionNumbers.toList,
                  cache.toCacheData,
                  EnvelopeWithMapping(envelope, cache.form),
                  validationService.validatePageModel _,
                  suppressErrors
                )
                .flatMap(f)

            val formModel = formModelOptics.formModelRenderPageOptics.formModel
            val bracket = formModel.bracket(sectionNumber)

            bracket match {
              case Bracket.NonRepeatingPage(SingletonWithNumber(singleton, sectionNumber), _) =>
                val formModel = formModelOptics.formModelVisibilityOptics.formModel
                // section numbers with form components that other components may refer to
                val sns = formModel.pageModelLookup
                  .get(sectionNumber)
                  .map { pageModel =>
                    pageModel.allFormComponents
                      .collect {
                        case IsMiniSummaryList(MiniSummaryList(rows, _, _)) =>
                          rows.collect {
                            case MiniSummaryRow.ValueRow(
                                  _,
                                  MiniSummaryListValue.Reference(FormCtx(r)),
                                  _,
                                  _
                                ) =>
                              List(r)
                            case MiniSummaryRow.ATLRow(atlId, _, rs) =>
                              atlId ::
                                rs collect {
                                  case MiniSummaryRow.ValueRow(
                                        _,
                                        MiniSummaryListValue.Reference(FormCtx(r)),
                                        _,
                                        _
                                      ) =>
                                    r
                                }
                          }
                        case IsOverseasAddress(OverseasAddress(_, _, _, Some(FormCtx(r)), _, _)) => List(List(r))
                        case IsAddress(Address(_, _, _, Some(FormCtx(r))))                       => List(List(r))
                      }
                      .flatten
                      .flatten
                      .flatMap(fcId =>
                        formModel.sectionNumberLookup.view.filterKeys(_.baseComponentId === fcId.baseComponentId).values
                      )
                  }
                  .getOrElse(List())

                validateSections(suppressErrors, (sectionNumber :: sns): _*)(
                  renderSingleton(singleton, sectionNumber, _)
                )
              case bracket @ Bracket.RepeatingPage(_, _) =>
                validateSections(suppressErrors, sectionNumber)(
                  renderSingleton(bracket.singletonForSectionNumber(sectionNumber), sectionNumber, _)
                )
              case bracket @ Bracket.AddToList(iterations, _) =>
                val iteration: Bracket.AddToListIteration[DataExpanded] =
                  bracket.iterationForSectionNumber(sectionNumber)
                val (repeater, repeaterSectionNumber) =
                  (iteration.repeater.repeater, iteration.repeater.sectionNumber)
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
                      Future.successful(
                        Ok(
                          renderer
                            .renderAddToListCheckYourAnswers(
                              checkYourAnswers.checkYourAnswers,
                              cache.formTemplate,
                              cache.formTemplateContext.specimenSource,
                              maybeAccessCode,
                              sectionNumber,
                              visibleIteration,
                              formModelOptics,
                              handlerResult.validationResult,
                              cache,
                              handlerResult.envelope,
                              AddressRecordLookup.from(cache.form.thirdPartyData),
                              fastForward
                            )
                        )
                      )
                    )
                  case _ =>
                    if (repeaterSectionNumber === sectionNumber) {
                      /*
                       * When new page is added to AddToList bracket, we need to visits repeater which
                       * has not been visited before, otherwise inflight users would be stuck in a loop.
                       */
                      val hasBeenVisited: Boolean = cache.form.visitsIndex.contains(sectionNumber)
                      if (sectionNumber === lastRepeaterSectionNumber || !hasBeenVisited) {
                        // display current (which happens to be last) repeater
                        validateSections(suppressErrors, sectionNumber)(handlerResult =>
                          Future.successful(
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
                                cache.formTemplateContext.specimenSource,
                                handlerResult.validationResult,
                                cache.retrievals,
                                fastForward
                              )
                            )
                          )
                        )
                      } else {
                        // We want to display last repeater
                        val sectionTitle4Ga =
                          sectionTitle4GaFactory(lastRepeater, lastRepeaterSectionNumber)
                        Redirect(
                          routes.FormController
                            .form(
                              cache.formTemplateId,
                              maybeAccessCode,
                              lastRepeaterSectionNumber,
                              sectionTitle4Ga,
                              suppressErrors,
                              fastForward match {
                                case Nil => Nil
                                case x :: xs =>
                                  x
                                    .next(
                                      formModelOptics.formModelVisibilityOptics.formModel,
                                      lastRepeaterSectionNumber
                                    ) :: xs
                              }
                            )
                        ).pure[Future]
                      }
                    } else {
                      // display current singleton
                      val visibleIteration: Bracket.AddToListIteration[Visibility] =
                        formModelOptics.formModelVisibilityOptics.formModel
                          .bracket(sectionNumber)
                          .withAddToListBracket(a => a.iterationForSectionNumber(sectionNumber))
                      validateSections(suppressErrors, visibleIteration.allSingletonSectionNumbers: _*)(
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
        fastForwardService.deleteForm(formTemplateId, cache, QueryParams.empty)
    }

  def formSection(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    ff: Option[List[FastForward]] = None
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      _ => _ => cache => implicit sse => formModelOptics =>
        val formModel = formModelOptics.formModelRenderPageOptics.formModel
        val sectionTitle4Ga = sectionTitle4GaFactory(formModel(sectionNumber), sectionNumber)
        val fastForwardYes: List[FastForward] = List(FastForward.Yes)
        Redirect(
          routes.FormController
            .form(
              cache.formTemplate._id,
              maybeAccessCode,
              sectionNumber,
              sectionTitle4Ga,
              SuppressErrors.Yes,
              ff.fold(fastForwardYes)(f => f)
            )
        ).pure[Future]
    }

  def backAction(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    rawFastForward: List[FastForward]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        sectionNumber match {
          case SectionNumber.Legacy(_) =>
            // Redirect users on first page of the form if they are currently submitting some page data. Current page data are lost
            redirectOnLegacy(formModelOptics, cache.formTemplateId, maybeAccessCode)
          case _ =>
            val formVisibilityModel = formModelOptics.formModelVisibilityOptics
            val fastForward = filterTerminatedFastForward(sectionNumber, rawFastForward, formVisibilityModel)
            val navigator = Navigator(sectionNumber, formModelOptics.formModelVisibilityOptics.formModel)

            def callSelector(call1: => Call, call2: => Call, lastSectionNumber: Option[SectionNumber]): Future[Call] =
              for {
                invalidSectionNumber <-
                  fastForwardService.maybeInvalidSectionNumber(lastSectionNumber, cache, formModelOptics)
              } yield if (invalidSectionNumber.isEmpty) call1 else call2

            def goBack(toSectionNumber: SectionNumber) = {
              val formModel = formModelOptics.formModelRenderPageOptics.formModel
              val sectionTitle4Ga = sectionTitle4GaFactory(formModel(toSectionNumber), sectionNumber)

              def createBackUrl(sectionNumber: SectionNumber, fastForward: List[FastForward]) =
                routes.FormController
                  .form(
                    cache.formTemplateId,
                    maybeAccessCode,
                    sectionNumber,
                    sectionTitle4Ga,
                    SuppressErrors.Yes,
                    fastForward
                  )

              def backCallF() = {
                val firstCYA = fastForward.find {
                  case _: FastForward.CYA => true
                  case _                  => false
                }
                firstCYA match {
                  case ff @ Some(FastForward.CYA(SectionOrSummary.FormSummary)) =>
                    fastForward.last match {
                      case FastForward.BackUntil(sn) if sectionNumber.compare(sn) <= 0 =>
                        routes.SummaryController
                          .summaryById(
                            cache.formTemplateId,
                            maybeAccessCode,
                            sectionNumber.maybeCoordinates,
                            None,
                            ff = ff
                          )
                          .pure[Future]
                      case _ =>
                        callSelector(
                          routes.SummaryController
                            .summaryById(
                              cache.formTemplateId,
                              maybeAccessCode,
                              sectionNumber.maybeCoordinates,
                              None,
                              ff = ff
                            ),
                          createBackUrl(toSectionNumber, fastForward),
                          None
                        )
                    }
                  case Some(FastForward.CYA(SectionOrSummary.Section(to))) =>
                    fastForward.last match {
                      case FastForward.BackUntil(sn) if sectionNumber.compare(sn) <= 0 =>
                        createBackUrl(to, fastForward).pure[Future]
                      case _ =>
                        callSelector(
                          createBackUrl(to, fastForward),
                          createBackUrl(toSectionNumber, fastForward),
                          Some(to)
                        )
                    }
                  case Some(FastForward.StopAt(sn)) =>
                    sectionNumber.fold { classic =>
                      createBackUrl(toSectionNumber, FastForward.StopAt(sectionNumber) :: fastForward).pure[Future]
                    } { taskList =>
                      val maybePreviousPage = navigator.previousSectionNumber
                      if (maybePreviousPage.isEmpty) {
                        uk.gov.hmrc.gform.tasklist.routes.TaskListController
                          .landingPage(cache.formTemplateId, maybeAccessCode)
                          .pure[Future]
                      } else {
                        createBackUrl(toSectionNumber, fastForward).pure[Future]
                      }

                    }
                  case _ =>
                    sectionNumber.fold { classic =>
                      createBackUrl(toSectionNumber, fastForward).pure[Future]
                    } { taskList =>
                      fastForward match {
                        case FastForward.CYA(SectionOrSummary.TaskSummary) :: xs =>
                          xs.lastOption match {
                            case Some(FastForward.BackUntil(sn)) if sectionNumber.compare(sn) <= 0 =>
                              Future.successful(
                                routes.SummaryController
                                  .summaryById(
                                    cache.formTemplateId,
                                    maybeAccessCode,
                                    sectionNumber.maybeCoordinates,
                                    None,
                                    ff = fastForward.headOption
                                  )
                              )
                            case _ =>
                              navigator.previousSectionNumber.fold(
                                Future.successful(
                                  routes.SummaryController
                                    .summaryById(
                                      cache.formTemplateId,
                                      maybeAccessCode,
                                      sectionNumber.maybeCoordinates,
                                      None,
                                      ff = fastForward.headOption
                                    )
                                )
                              )(previousSectionNumber => createBackUrl(previousSectionNumber, fastForward).pure[Future])
                          }
                        case _ =>
                          val maybePreviousPage = navigator.previousSectionNumber

                          if (maybePreviousPage.isEmpty) {
                            callSelector(
                              uk.gov.hmrc.gform.tasklist.routes.TaskListController
                                .landingPage(cache.formTemplateId, maybeAccessCode),
                              uk.gov.hmrc.gform.tasklist.routes.TaskListController
                                .landingPage(cache.formTemplateId, maybeAccessCode),
                              None
                            )
                          } else {
                            createBackUrl(toSectionNumber, fastForward).pure[Future]
                          }
                      }
                    }
                }
              }
              backCallF().map(Redirect(_))
            }

            val toSectionNumber = navigator.previousSectionNumber.getOrElse(sectionNumber)
            val formModel = formModelOptics.formModelRenderPageOptics.formModel
            val bracket = formModel.bracket(toSectionNumber)

            bracket match {
              case Bracket.NonRepeatingPage(_, _) => goBack(toSectionNumber)
              case Bracket.RepeatingPage(_, _)    => goBack(toSectionNumber)
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
                                    List(FastForward.Yes),
                                    formModelOptics,
                                    processData,
                                    sectionNumber.templateSectionIndex,
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

    }

  def addToListAction(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    rawFastForward: List[FastForward],
    direction: Direction
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => _ => formModelOptics =>
        sectionNumber match {
          case SectionNumber.Legacy(_) =>
            // Redirect users on first page of the form if they are currently submitting some page data. Current page data are lost
            redirectOnLegacy(formModelOptics, cache.formTemplateId, maybeAccessCode)
          case _ =>
            val fastForward = removeDuplications(rawFastForward)
            def processEditAddToList(processData: ProcessData, idx: Int, addToListId: AddToListId): Future[Result] = {

              val addToListIteration =
                processData.formModelOptics.formModelVisibilityOptics.formModel.brackets.addToListById(addToListId, idx)

              def defaultNavigation(): (SectionNumber, List[FastForward]) = {
                val sectionNumber = addToListIteration.firstSectionNumber
                (sectionNumber, fastForward)
              }

              def checkYourAnswersNavigation(
                cya: CheckYourAnswersWithNumber[Visibility]
              ): (SectionNumber, List[FastForward]) =
                (cya.sectionNumber, FastForward.CYA(SectionOrSummary.Section(sectionNumber)) :: fastForward)

              val (gotoSectionNumber, ff) =
                addToListIteration.checkYourAnswers.fold(defaultNavigation())(checkYourAnswersNavigation)

              val sectionTitle4Ga = formProcessor.getSectionTitle4Ga(processData, sectionNumber)

              Redirect(
                routes.FormController
                  .form(
                    cache.formTemplateId,
                    maybeAccessCode,
                    gotoSectionNumber,
                    sectionTitle4Ga,
                    SuppressErrors.Yes,
                    ff
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
                               cache.formTemplateId,
                               maybeAccessCode,
                               sectionNumber,
                               sectionTitle4Ga,
                               SuppressErrors.Yes,
                               fastForward
                             )
                         ).pure[Future]
                       case _ => throw new IllegalArgumentException(s"Direction $direction is not supported here")
                     }
            } yield res
        }

    }

  private def redirectOnLegacy(
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) = {
    val sn =
      formModelOptics.formModelVisibilityOptics.formModel.availableSectionNumbers.head // In production this should always return some section
    Redirect(
      routes.FormController
        .form(
          formTemplateId,
          maybeAccessCode,
          sn,
          SectionTitle4Ga("welcome"),
          SuppressErrors.Yes,
          List(FastForward.Yes)
        )
    ).pure[Future]
  }

  def updateFormData(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    browserSectionNumber: SectionNumber,
    rawFastForward: List[FastForward],
    save: Direction
  ) = {
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        val formModel = formModelOptics.formModelVisibilityOptics.formModel
        val fastForward = filterFastForward(browserSectionNumber, rawFastForward, formModel)
        browserSectionNumber match {
          case SectionNumber.Legacy(_) =>
            // Redirect users on first page of the form if they are currently submitting some page data. Current page data are lost
            redirectOnLegacy(formModelOptics, cache.formTemplateId, maybeAccessCode)
          case _ =>
            processResponseDataFromBody(
              request,
              formModelOptics.formModelRenderPageOptics,
              Some(browserSectionNumber)
            ) { requestRelatedData => variadicFormData => enteredVariadicFormData =>
              val sectionNumber: SectionNumber =
                formModelOptics.formModelVisibilityOptics.formModel.visibleSectionNumber(browserSectionNumber)

              def processSaveAndContinue(
                processData: ProcessData
              ): Future[Result] = {
                val status = confirmationService.processConfirmation(
                  sectionNumber,
                  processData,
                  cache.formTemplateId,
                  maybeAccessCode,
                  formModelOptics,
                  fastForward
                )
                status match {
                  case ConfirmationAction.NotConfirmed(redirect) => redirect.pure[Future]
                  case ConfirmationAction.UpdateConfirmation(processDataUpdater, isConfirmationPage) =>
                    val processDataUpd = processDataUpdater(processData)
                    formProcessor.validateAndUpdateData(
                      cache,
                      processDataUpd,
                      sectionNumber,
                      sectionNumber,
                      maybeAccessCode,
                      fastForward,
                      formModelOptics,
                      enteredVariadicFormData,
                      true
                    ) { updatePostcodeLookup => maybeRedirectUrl => maybeSn =>
                      def processRemoveItemIf() = {
                        val formModel = formModelOptics.formModelRenderPageOptics.formModel
                        val bracket = formModel.bracket(sectionNumber)

                        bracket match {
                          case bracket @ Bracket.AddToList(_, _) =>
                            val iteration = bracket.iterationForSectionNumber(sectionNumber)
                            val sectionNumbers = iteration.allSingletonSectionNumbers

                            sectionNumbers.flatMap { sectionNumber =>
                              maybeAtlRemoveIteration(bracket, sectionNumber)
                            }.headOption
                          case _ => None
                        }
                      }

                      def maybeAtlRemoveIteration(
                        bracket: Bracket[DataExpanded],
                        sectionNumber: SectionNumber
                      ): Option[Result] =
                        bracket
                          .atlIterationToRemove(
                            sectionNumber,
                            processDataUpd.formModelOptics.formModelVisibilityOptics
                          )
                          .map { case (atlId, index) =>
                            Redirect(
                              routes.FormAddToListController
                                .removeItem(
                                  formTemplateId,
                                  maybeAccessCode,
                                  sectionNumber,
                                  index,
                                  atlId,
                                  fastForward
                                )
                            )
                          }

                      def continueJourney: Result =
                        maybeSn match {
                          case SectionOrSummary.Section(sn) =>
                            val endOfTask: Option[Coordinates] =
                              sn.fold[Option[Coordinates]] { classic =>
                                None
                              } { taskList =>
                                val cur = sectionNumber.unsafeToTaskList.coordinates
                                if (taskList.coordinates === cur) {
                                  None
                                } else {
                                  Some(cur)
                                }
                              }
                            if (endOfTask.isDefined) {
                              Redirect(
                                routes.SummaryController
                                  .summaryById(
                                    cache.formTemplateId,
                                    maybeAccessCode,
                                    endOfTask,
                                    None,
                                    ff = fastForward.headOption
                                  )
                              )
                            } else {
                              val isFirstLanding = sectionNumber < sn
                              logger.info(
                                s"envelope id: ${cache.form.envelopeId.value}, form id: ${cache.form._id.value}, user-agent: ${request.headers.get("User-Agent").getOrElse("")}"
                              )
                              val sectionTitle4Ga = formProcessor.getSectionTitle4Ga(processDataUpd, sn)

                              val formModel = formModelOptics.formModelRenderPageOptics.formModel
                              val bracket = formModel.bracket(sectionNumber)

                              val isLastBracketIteration = bracket match {
                                case bracket @ Bracket.AddToList(_, _) =>
                                  val iteration = bracket.iterationForSectionNumber(sectionNumber)
                                  sectionNumber === iteration.lastSectionNumber
                                case _ => false
                              }

                              maybeAtlRemoveIteration(bracket, sectionNumber)
                                .getOrElse(
                                  Redirect(
                                    routes.FormController
                                      .form(
                                        cache.formTemplateId,
                                        maybeAccessCode,
                                        sn,
                                        sectionTitle4Ga,
                                        SuppressErrors(isFirstLanding),
                                        if (isLastBracketIteration) fastForward
                                        else if (isFirstLanding || sectionNumber.isTaskList) {
                                          fastForward match {
                                            case Nil =>
                                              Nil
                                            case x :: FastForward.StopAt(s) :: xs
                                                if formModel.availableSectionNumbers.contains(s) =>
                                              FastForward.StopAt(s) :: xs
                                            case x :: xs =>
                                              x.next(
                                                processDataUpd.formModelOptics.formModelVisibilityOptics.formModel,
                                                sn
                                              ) :: xs
                                          }
                                        } else
                                          fastForward
                                      )
                                  )
                                )
                            }
                          case SectionOrSummary.FormSummary =>
                            processRemoveItemIf().getOrElse(
                              Redirect(
                                routes.SummaryController
                                  .summaryById(
                                    cache.formTemplateId,
                                    maybeAccessCode,
                                    sectionNumber.maybeCoordinates,
                                    None,
                                    true
                                  )
                              )
                            )
                          case SectionOrSummary.TaskSummary =>
                            processRemoveItemIf().getOrElse(
                              Redirect(
                                routes.SummaryController
                                  .summaryById(
                                    cache.formTemplateId,
                                    maybeAccessCode,
                                    sectionNumber.maybeCoordinates,
                                    None
                                  )
                              )
                            )
                        }

                      maybeRedirectUrl match {
                        case Some(r) => Redirect(r)
                        case None =>
                          updatePostcodeLookup.fold(continueJourney) { case (formComponentId, _) =>
                            Redirect(
                              uk.gov.hmrc.gform.addresslookup.routes.AddressLookupController
                                .chooseAddress(
                                  cache.formTemplate._id,
                                  maybeAccessCode,
                                  formComponentId,
                                  sectionNumber,
                                  fastForward
                                )
                            )
                          }
                      }
                    }
                }
              }

              def processSaveAndExit(processData: ProcessData): Future[Result] = {

                val purgeConfirmationData: PurgeConfirmationData =
                  confirmationService.purgeConfirmationData(sectionNumber, processData, enteredVariadicFormData)

                formProcessor.validateAndUpdateData(
                  cache,
                  purgeConfirmationData.f(processData),
                  sectionNumber,
                  sectionNumber,
                  maybeAccessCode,
                  fastForward,
                  formModelOptics,
                  purgeConfirmationData.enteredVariadicFormData,
                  false
                ) { _ => _ => maybeSn =>
                  val (sectionTitle4Ga, sectionNumber) = maybeSn match {
                    case SectionOrSummary.Section(sn) =>
                      (
                        Some(formProcessor.getSectionTitle4Ga(processData, sn)),
                        Some(sn)
                      )
                    case _ => (None, None)
                  }

                  Redirect(
                    gform.routes.SaveAcknowledgementController
                      .saveAndExit(formTemplateId, maybeAccessCode, sectionNumber, sectionTitle4Ga)
                  )
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
                  formModelOptics,
                  enteredVariadicFormData,
                  true
                ) { _ => _ => _ =>
                  val sectionTitle4Ga = formProcessor.getSectionTitle4Ga(processData, sectionNumber)
                  Redirect(
                    routes.FormController
                      .form(
                        cache.formTemplateId,
                        maybeAccessCode,
                        sectionNumber,
                        sectionTitle4Ga,
                        SuppressErrors.Yes,
                        List(FastForward.Yes)
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
                      case fc @ IsChoice(_)          => HtmlFieldId.indexed(fc.id, "0")
                      case fc @ IsRevealingChoice(_) => HtmlFieldId.indexed(fc.id, "0")
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
                                            recalculation,
                                            currentPage = formModel.pageModelLookup(sectionNumber) match {
                                              case Singleton(page) => Some(page)
                                              case _               => None
                                            }
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
                                            recalculation,
                                            currentPage = formModel.pageModelLookup(sectionNumber) match {
                                              case Singleton(page) => Some(page)
                                              case _               => None
                                            }
                                          )
                  res <- handleGroup(cacheUpd, processData.copy(formModelOptics = updFormModelOptics), "")
                  _   <- objectStoreAlgebra.deleteFiles(cache.form.envelopeId, filesToDelete)
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
                         case SaveAndExit =>
                           processSaveAndExit(processData).map { result =>
                             auditService.formSavedEvent(
                               cache.form,
                               cache.retrievals
                             )
                             result
                           }
                         case AddGroup(modelComponentId)    => processAddGroup(processData, modelComponentId)
                         case RemoveGroup(modelComponentId) => processRemoveGroup(processData, modelComponentId)
                         case _                             => throw new IllegalArgumentException(s"Direction $save is not supported here")
                       }
              } yield res
            }
        }

    }
  }

  private val formMaxAttachmentSizeMB = appConfig.formMaxAttachmentSizeMB
  private val restrictedFileExtensions = appConfig.restrictedFileExtensions

  /*
  We separate the process of adding the required FastForward to the
  List[FastForward] from the process of removing it. So when we add
  the new FastForward we are not concerned with what is in the list
  already.
  The removal happens at the beginning of processing of 4 endpoints
  that takes List[FastForward]. It does the following:
   - removes duplications
   - removes FastForward with destination section number higher than
   the current section number
   */
  private def filterFastForward(
    browserSectionNumber: SectionNumber,
    rawFastForward: List[FastForward],
    formModel: FormModel[Visibility]
  ): List[FastForward] = {
    val sectionNumber = formModel.visibleSectionNumber(browserSectionNumber)
    val ff = rawFastForward
      .filterNot {
        case FastForward.CYA(SectionOrSummary.Section(sn)) => sectionNumber >= sn
        case FastForward.StopAt(sn)                        => sectionNumber >= sn
        case _                                             => false
      }
      .filter {
        case FastForward.CYA(SectionOrSummary.Section(sn)) => formModel.availableSectionNumbers.contains(sn)
        case FastForward.StopAt(sn)                        => formModel.availableSectionNumbers.contains(sn)
        case _                                             => true
      }
    removeDuplications(ff)
  }

  private def removeDuplications(
    fastForward: List[FastForward]
  ): List[FastForward] = {
    val ff = fastForward
      .foldLeft[List[FastForward]](List())((ls, a) =>
        ls match {
          case Nil               => List(a)
          case x :: xs if x == a => x :: xs
          case xs                => a :: xs
        }
      )
      .reverse
    if (ff.nonEmpty) ff else List(FastForward.Yes)
  }

  private def filterTerminatedFastForward(
    browserSectionNumber: SectionNumber,
    rawFastForward: List[FastForward],
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo]
  ): List[FastForward] = {
    val formModel: FormModel[Visibility] = formModelVisibilityOptics.formModel
    def hasTerminationIn(fromSn: SectionNumber, toSn: Option[SectionNumber]): Boolean =
      formModel.availableSectionNumbers
        .filter(sn =>
          (fromSn, sn) match {
            case (
                  SectionNumber.TaskList(Coordinates(_, thisTaskNumber), _),
                  SectionNumber.TaskList(Coordinates(_, otherTaskNumber), _)
                ) =>
              thisTaskNumber === otherTaskNumber
            case _ => true
          }
        )
        .filter(sn => fromSn <= sn)
        .filter(sn =>
          toSn match {
            case Some(to) => sn <= to
            case None     => true
          }
        )
        .exists(sn => formModel(sn).isTerminationPage(formModelVisibilityOptics.booleanExprResolver))

    val sectionNumber = formModel.visibleSectionNumber(browserSectionNumber)

    val ff = rawFastForward
      .filterNot {
        case FastForward.CYA(SectionOrSummary.Section(sn)) => hasTerminationIn(sectionNumber, Some(sn))
        case FastForward.CYA(_)                            => hasTerminationIn(sectionNumber, None)
        case FastForward.StopAt(sn)                        => hasTerminationIn(sectionNumber, Some(sn))
        case _                                             => false
      }
    removeDuplications(ff)
  }

}
