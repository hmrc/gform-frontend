/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.gform.fileupload.{ EnvelopeWithMapping, FileUploadService }
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.{ Coordinates, FastForward, ProcessData, ProcessDataService, SectionSelector, SectionSelectorType }
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

class FastForwardService(
  fileUploadService: FileUploadService,
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
    maybeCoordinates: Option[Coordinates]
  )(implicit
    messages: Messages,
    hc: HeaderCarrier,
    l: LangADT
  ): Future[Result] =
    redirectWithRecalculation(cache, maybeAccessCode, FastForward.NextNew, formModelOptics, maybeCoordinates)

  def redirectStopAt[U <: SectionSelectorType: SectionSelector](
    sectionNumber: SectionNumber,
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    messages: Messages,
    hc: HeaderCarrier,
    l: LangADT
  ): Future[Result] =
    redirectWithRecalculation(
      cache,
      maybeAccessCode,
      FastForward.StopAt(sectionNumber),
      formModelOptics,
      sectionNumber.toCoordinates
    )

  private def redirectWithRecalculation[U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    fastForward: FastForward,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    maybeCoordinates: Option[Coordinates]
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
          DataOrigin.swapDataOrigin(formModelVisibilityOptics),
          cache.retrievals,
          cache.form.thirdPartyData,
          maybeAccessCode,
          cache.form.envelopeId,
          cache.formTemplate
        )
        for {
          envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)(cache.formTemplate.objectStore)
          res <-
            updateUserData(
              cache,
              processData,
              maybeAccessCode,
              fastForward,
              EnvelopeWithMapping(envelope, cache.form),
              maybeCoordinates
            )(
              redirectResult(cache, maybeAccessCode, processData, _)
            )
        } yield res

      }

  private def redirectResult(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    processData: ProcessData,
    maybeSectionNumber: SectionOrSummary
  ): Result =
    maybeSectionNumber match {
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
              SuppressErrors.Yes,
              FastForward.NextNew
            )
        )
      case _ =>
        Redirect(routes.SummaryController.summaryById(cache.formTemplate._id, maybeAccessCode, None, None))
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
    fastForward: FastForward,
    envelope: EnvelopeWithMapping,
    maybeCoordinates: Option[Coordinates]
  )(
    toResult: SectionOrSummary => Result
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
                   maybeCoordinates
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
               .map(_ => toResult(maybeSn))
    } yield res

}
