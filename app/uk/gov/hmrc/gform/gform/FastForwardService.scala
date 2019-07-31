/*
 * Copyright 2019 HM Revenue & Customs
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
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthCacheWithoutForm }
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Data
import uk.gov.hmrc.gform.models.gform.ObligationsAction
import uk.gov.hmrc.gform.models.{ ProcessData, ProcessDataService }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormStatus, InProgress, Summary, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, SeYes, SectionNumber }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class FastForwardService(
  fileUploadService: FileUploadService,
  validationService: ValidationService,
  gformConnector: GformConnector,
  processDataService: ProcessDataService[Future, Throwable],
  handler: FormControllerRequestHandler
)(implicit ec: ExecutionContext) {

  def redirectFromEmpty(
    cache: AuthCacheWithoutForm,
    form: Form,
    maybeAccessCode: Option[AccessCode],
    obligationsAction: ObligationsAction)(implicit messages: Messages, hc: HeaderCarrier, l: LangADT) = {
    val dataRaw = Map.empty[FormComponentId, Seq[String]]
    val cacheWithForm = cache.toAuthCacheWithForm(form)
    redirectWithRecalculation(cacheWithForm, dataRaw, maybeAccessCode, obligationsAction)
  }

  def redirectWithRecalculation(
    cache: AuthCacheWithForm,
    dataRaw: Data,
    maybeAccessCode: Option[AccessCode],
    obligationsAction: ObligationsAction)(implicit messages: Messages, hc: HeaderCarrier, l: LangADT): Future[Result] =
    for {
      processData <- processDataService
                      .getProcessData(dataRaw, cache, gformConnector.getAllTaxPeriods, obligationsAction)
      res <- updateUserData(cache, processData)(redirectResult(cache, maybeAccessCode, processData, _))
    } yield res

  private def redirectResult(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    processData: ProcessData,
    maybeSectionNumber: Option[SectionNumber])(implicit l: LangADT): Result =
    maybeSectionNumber match {
      case Some(sn) =>
        val section = processData.sections(sn.value)
        val sectionTitle4Ga = sectionTitle4GaFactory(section.title.value)
        Redirect(
          routes.FormController
            .form(cache.formTemplate._id, maybeAccessCode, sn, sectionTitle4Ga, SeYes))
      case None => Redirect(routes.SummaryController.summaryById(cache.formTemplate._id, maybeAccessCode))
    }

  def deleteForm(cache: AuthCacheWithForm)(implicit hc: HeaderCarrier): Future[Result] = {
    val formTemplateId = cache.formTemplate._id
    gformConnector
      .deleteForm(cache.form._id)
      .map(_ => Redirect(routes.NewFormController.dashboard(formTemplateId)))
  }

  def updateUserData(cache: AuthCacheWithForm, processData: ProcessData)(toResult: Option[SectionNumber] => Result)(
    implicit messages: Messages,
    hc: HeaderCarrier,
    l: LangADT): Future[Result] =
    for {
      maybeSn <- handler.handleFastForwardValidate(
                  processData,
                  cache,
                  FormService.extractedValidateFormHelper,
                  fileUploadService.getEnvelope,
                  validationService.validateFormComponents,
                  validationService.evaluateValidation
                )
      userData = UserData(
        cache.form.formData,
        maybeSn.fold(Summary: FormStatus)(_ => InProgress),
        processData.visitIndex,
        cache.form.thirdPartyData.modify(_.obligations).setTo(processData.obligations)
      )
      res <- gformConnector.updateUserData(cache.form._id, userData).map(_ => toResult(maybeSn))
    } yield res

}
