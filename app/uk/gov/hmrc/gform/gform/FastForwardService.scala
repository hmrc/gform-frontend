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
import cats.syntax.eq._
import cats.syntax.traverse._
import com.softwaremill.quicklens._
import org.slf4j.{ Logger, LoggerFactory }
import play.api.i18n.Messages
import play.api.mvc.Result
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.objectStore.{ EnvelopeWithMapping, ObjectStoreService }
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.{ FastForward, ProcessData, ProcessDataService, SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.gform.ForceReload
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ FormIdData, FormModelOptics, InProgress, QueryParams, Summary, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, SectionNumber, SuppressErrors }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionOrSummary
import uk.gov.hmrc.gform.models.gform.NoSpecificAction

class FastForwardService(
  objectStoreService: ObjectStoreService,
  validationService: ValidationService,
  gformConnector: GformConnector,
  processDataService: ProcessDataService[Future],
  handler: FormControllerRequestHandler,
  smartStringEvaluatorFactory: SmartStringEvaluatorFactory
)(implicit ec: ExecutionContext) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def redirectFastForward[U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    maybeSectionNumber: Option[SectionNumber],
    fastForward: List[FastForward] = Nil,
    suppressErrors: SuppressErrors = SuppressErrors.Yes
  )(implicit
    messages: Messages,
    hc: HeaderCarrier,
    l: LangADT
  ): Future[Result] =
    redirectWithRecalculation(
      cache,
      maybeAccessCode,
      if (fastForward.nonEmpty) fastForward else maybeSectionNumber.map(FastForward.StopAt).toList,
      formModelOptics,
      maybeSectionNumber,
      suppressErrors
    )

  def redirectStopAt[U <: SectionSelectorType: SectionSelector](
    sectionNumber: SectionNumber,
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    suppressErrors: SuppressErrors = SuppressErrors.Yes
  )(implicit
    messages: Messages,
    hc: HeaderCarrier,
    l: LangADT
  ): Future[Result] =
    redirectWithRecalculation(
      cache,
      maybeAccessCode,
      List(FastForward.StopAt(sectionNumber)),
      formModelOptics,
      Some(sectionNumber),
      suppressErrors
    )

  private def redirectWithRecalculation[U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    fastForward: List[FastForward],
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    maybeSectionNumber: Option[SectionNumber],
    suppressErrors: SuppressErrors
  )(implicit
    messages: Messages,
    hc: HeaderCarrier,
    l: LangADT
  ): Future[Result] =
    processDataService
      .getProcessData(cache.variadicFormData, cache, formModelOptics, gformConnector.getAllTaxPeriods, ForceReload)
      .flatMap { processData =>
        // This formModelVisibilityOptics comes from Mongo, not from Browser
        val formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser] =
          processData.formModelOptics.formModelVisibilityOptics
        implicit val sse: SmartStringEvaluator = smartStringEvaluatorFactory(
          DataOrigin.swapDataOrigin(formModelVisibilityOptics)
        )
        for {
          envelope <- objectStoreService.getEnvelope(cache.form.envelopeId)
          res <-
            updateUserData(
              cache,
              processData,
              maybeAccessCode,
              fastForward,
              EnvelopeWithMapping(envelope, cache.form),
              maybeSectionNumber
            )(
              redirectResult(cache, maybeAccessCode, processData, _, fastForward, _, suppressErrors)
            )
        } yield res

      }

  private def redirectResult(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    processData: ProcessData,
    sectionOrSummary: SectionOrSummary,
    fastForward: List[FastForward],
    maybeSectionNumber: Option[SectionNumber],
    suppressErrors: SuppressErrors
  )(implicit sse: SmartStringEvaluator): Result =
    sectionOrSummary match {
      case SectionOrSummary.Section(sn) =>
        val pageModel = processData.formModel(sn)
        val sectionTitle4Ga = sectionTitle4GaFactory(pageModel, sn)

        Redirect(
          routes.FormController
            .form(
              cache.formTemplate._id,
              maybeAccessCode,
              sn,
              sectionTitle4Ga,
              suppressErrors,
              fastForward
            )
        )
      case _ =>
        val maybeCoordinates = maybeSectionNumber.flatMap(_.toCoordinates)
        Redirect(
          routes.SummaryController
            .summaryById(cache.formTemplate._id, maybeAccessCode, maybeCoordinates, None, ff = fastForward.headOption)
        )

    }

  def deleteForm(
    formTemplateId: FormTemplateId,
    cache: AuthCacheWithForm,
    queryParams: QueryParams
  )(implicit
    hc: HeaderCarrier
  ): Future[Result] =
    for {
      latestFormTemplate <- gformConnector.getLatestFormTemplate(formTemplateId)
      formsToDelete <- {
        val formsToDelete =
          if (cache.formTemplateId === formTemplateId)
            List(cache.form._id)
          else {
            List(cache.form._id, FormIdData.apply(cache, None).withTemplateId(formTemplateId).toFormId)
          }
        Future.successful(
          FormIdData.apply(cache, None).withTemplateId(latestFormTemplate._id).toFormId :: formsToDelete
        )
      }
      res <- {
        logger.info("User decided not to continue in his form, deleting form(s): " + formsToDelete.mkString(", "))
        formsToDelete
          .traverse(formId => gformConnector.deleteForm(formId))
          .map(_ =>
            Redirect(routes.NewFormController.dashboard(latestFormTemplate._id).url, queryParams.toPlayQueryParams)
          )
      }
    } yield res

  def updateUserData(
    cache: AuthCacheWithForm,
    processData: ProcessData,
    maybeAccessCode: Option[AccessCode],
    fastForward: List[FastForward],
    envelope: EnvelopeWithMapping,
    maybeSectionNumber: Option[SectionNumber]
  )(
    toResult: (SectionOrSummary, Option[SectionNumber]) => Result
  )(implicit
    messages: Messages,
    hc: HeaderCarrier,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[Result] =
    for {
      maybeSn <- handler.handleFastForwardValidate(
                   processData,
                   cache.toCacheData,
                   envelope,
                   validationService.validatePageModel,
                   fastForward,
                   maybeSectionNumber
                 )

      formStatus = maybeSn match {
                     case SectionOrSummary.Section(_) => InProgress
                     case _                           => Summary
                   }
      userData = UserData(
                   cache.form.formData,
                   formStatus,
                   processData.visitsIndex,
                   cache.form.thirdPartyData
                     .modify(_.obligations)
                     .setTo(processData.obligations)
                     .modify(_.booleanExprCache)
                     .setTo(processData.booleanExprCache),
                   cache.form.componentIdToFileId
                 )
      res <- gformConnector
               .updateUserData(FormIdData.fromForm(cache.form, maybeAccessCode), userData)
               .map(_ => toResult(maybeSn, maybeSectionNumber))
    } yield res

  def maybeInvalidSectionNumber(
    lastSectionNumber: Option[SectionNumber],
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    messages: Messages,
    hc: HeaderCarrier,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[Option[SectionNumber]] = for {
    envelope <- objectStoreService.getEnvelope(cache.form.envelopeId)
    processData <- processDataService
                     .getProcessData[SectionSelectorType.Normal](
                       formModelOptics.formModelRenderPageOptics.recData.variadicFormData
                         .asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]],
                       cache,
                       formModelOptics,
                       gformConnector.getAllTaxPeriods,
                       NoSpecificAction
                     )
    maybeInvalidSectionNumber <- handler.handleMaybeGetInvalidSectionNumber(
                                   processData,
                                   cache.toCacheData,
                                   EnvelopeWithMapping(envelope, cache.form),
                                   validationService.validatePageModel,
                                   lastSectionNumber
                                 )
  } yield maybeInvalidSectionNumber.filter(sn => lastSectionNumber.forall(sn < _))
}
