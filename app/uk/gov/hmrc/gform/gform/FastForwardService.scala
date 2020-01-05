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
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.gform.ForceReload
import uk.gov.hmrc.gform.models.{ ProcessData, ProcessDataService }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ FormIdData, FormStatus, InProgress, Summary, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ SeYes, SectionNumber }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class FastForwardService(
  fileUploadService: FileUploadService,
  validationService: ValidationService,
  gformConnector: GformConnector,
  processDataService: ProcessDataService[Future, Throwable],
  handler: FormControllerRequestHandler,
  smartStringEvaluatorFactory: SmartStringEvaluatorFactory
)(implicit ec: ExecutionContext) {

  def redirectContinue(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode])(implicit messages: Messages, hc: HeaderCarrier, l: LangADT) = {
    val dataRaw = cache.variadicFormData
    redirectWithRecalculation(cache, dataRaw, maybeAccessCode)
  }

  private def redirectWithRecalculation(
    cache: AuthCacheWithForm,
    dataRaw: VariadicFormData,
    maybeAccessCode: Option[AccessCode])(implicit messages: Messages, hc: HeaderCarrier, l: LangADT): Future[Result] =
    processDataService.getProcessData(dataRaw, cache, gformConnector.getAllTaxPeriods, ForceReload).flatMap {
      processData =>
        implicit val sse = smartStringEvaluatorFactory(
          processData.data,
          cache.retrievals,
          cache.form.thirdPartyData,
          cache.form.envelopeId,
          cache.formTemplate)
        updateUserData(cache, processData, maybeAccessCode)(redirectResult(cache, maybeAccessCode, processData, _))
    }

  private def redirectResult(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    processData: ProcessData,
    maybeSectionNumber: Option[SectionNumber])(implicit l: LangADT, sse: SmartStringEvaluator): Result =
    maybeSectionNumber match {
      case Some(sn) =>
        val section = processData.sections(sn.value)
        val sectionTitle4Ga = sectionTitle4GaFactory(section.title.value)
        Redirect(
          routes.FormController
            .form(cache.formTemplate._id, maybeAccessCode, sn, sectionTitle4Ga, SeYes))
      case None =>
        Redirect(routes.SummaryController.summaryById(cache.formTemplate._id, maybeAccessCode))
    }

  def deleteForm(cache: AuthCacheWithForm)(implicit hc: HeaderCarrier): Future[Result] = {
    val formTemplateId = cache.formTemplate._id
    gformConnector
      .deleteForm(cache.form._id)
      .map(_ => Redirect(routes.NewFormController.dashboard(formTemplateId)))
  }

  def updateUserData(cache: AuthCacheWithForm, processData: ProcessData, maybeAccessCode: Option[AccessCode])(
    toResult: Option[SectionNumber] => Result)(
    implicit messages: Messages,
    hc: HeaderCarrier,
    l: LangADT,
    sse: SmartStringEvaluator): Future[Result] =
    for {
      envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)
      maybeSn <- handler.handleFastForwardValidate(
                  processData,
                  cache,
                  envelope,
                  FormService.extractedValidateFormHelper,
                  validationService.validateFormComponents,
                  validationService.evaluateValidation
                )
      userData = UserData(
        cache.form.formData,
        maybeSn.fold(Summary: FormStatus)(_ => InProgress),
        processData.visitsIndex,
        cache.form.thirdPartyData.modify(_.obligations).setTo(processData.obligations)
      )
      res <- gformConnector
              .updateUserData(FormIdData.fromForm(cache.form, maybeAccessCode), userData)
              .map(_ => toResult(maybeSn))
    } yield res

}
