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
import com.softwaremill.quicklens._
import play.api.i18n.Messages
import play.api.mvc.Result
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadService }
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.{ FastForward, ProcessData, ProcessDataService, SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.gform.ForceReload
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ FormIdData, FormModelOptics, FormStatus, InProgress, QueryParams, Summary, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ SectionNumber, SuppressErrors }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class FastForwardService(
  fileUploadService: FileUploadService,
  validationService: ValidationService,
  gformConnector: GformConnector,
  processDataService: ProcessDataService[Future],
  handler: FormControllerRequestHandler,
  smartStringEvaluatorFactory: SmartStringEvaluatorFactory
)(implicit ec: ExecutionContext) {

  def redirectContinue2[U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(
    implicit
    messages: Messages,
    hc: HeaderCarrier,
    l: LangADT
  ): Future[Result] =
    redirectWithRecalculation(cache, maybeAccessCode, FastForward.Yes, formModelOptics)

  private def redirectWithRecalculation[U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    fastForward: FastForward,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(
    implicit
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
          envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)
          res <- updateUserData(cache, processData, maybeAccessCode, fastForward, envelope)(
                  redirectResult(cache, maybeAccessCode, processData, _))
        } yield res

      }

  private def redirectResult(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    processData: ProcessData,
    maybeSectionNumber: Option[SectionNumber]
  )(
    implicit
    l: LangADT,
    sse: SmartStringEvaluator
  ): Result =
    maybeSectionNumber match {
      case Some(sn) =>
        val pageModel = processData.formModel(sn)
        val sectionTitle4Ga = sectionTitle4GaFactory(pageModel.title, sn)
        Redirect(
          routes.FormController
            .form(cache.formTemplate._id, maybeAccessCode, sn, sectionTitle4Ga, SuppressErrors.Yes, FastForward.Yes))
      case None =>
        Redirect(routes.SummaryController.summaryById(cache.formTemplate._id, maybeAccessCode))
    }

  def deleteForm(
    cache: AuthCacheWithForm,
    queryParams: QueryParams
  )(
    implicit
    hc: HeaderCarrier
  ): Future[Result] = {
    val formTemplateId = cache.formTemplate._id
    gformConnector
      .deleteForm(cache.form._id)
      .map(_ => Redirect(routes.NewFormController.dashboard(formTemplateId).url, queryParams.toPlayQueryParams))
  }

  def updateUserData(
    cache: AuthCacheWithForm,
    processData: ProcessData,
    maybeAccessCode: Option[AccessCode],
    fastForward: FastForward,
    envelope: Envelope
  )(
    toResult: Option[SectionNumber] => Result
  )(
    implicit
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
                  fastForward
                )
      userData = UserData(
        cache.form.formData,
        maybeSn.fold(Summary: FormStatus)(_ => InProgress),
        processData.visitsIndex,
        cache.form.thirdPartyData
          .modify(_.obligations)
          .setTo(processData.obligations)
          .modify(_.booleanExprCache)
          .setTo(processData.booleanExprCache)
      )
      res <- gformConnector
              .updateUserData(FormIdData.fromForm(cache.form, maybeAccessCode), userData)
              .map(_ => toResult(maybeSn))
    } yield res

}
