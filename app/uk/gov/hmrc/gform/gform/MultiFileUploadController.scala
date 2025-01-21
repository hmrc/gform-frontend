/*
 * Copyright 2024 HM Revenue & Customs
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
import play.api.i18n.I18nSupport
import play.api.mvc.MessagesControllerComponents
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.models.{ Bracket, DataExpanded, FastForward, ProcessDataService, SectionSelectorType, Singleton, SingletonWithNumber }
import uk.gov.hmrc.gform.objectStore.{ EnvelopeWithMapping, ObjectStoreAlgebra }
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.FormIdData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, SectionNumber, SectionTitle4Ga, SuppressErrors }
import uk.gov.hmrc.gform.summary.AddressRecordLookup
import uk.gov.hmrc.gform.upscan.UpscanAlgebra
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class MultiFileUploadController(
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
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  def multiPost(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    browserSectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    suppressErrors: SuppressErrors,
    rawFastForward: List[FastForward],
    showFileUpload: Boolean
  ): play.api.mvc.Action[play.api.mvc.AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      request => l => cache => sse => formModelOptics =>
        Redirect(
          uk.gov.hmrc.gform.gform.routes.MultiFileUploadController
            .multi(
              formTemplateId,
              maybeAccessCode,
              browserSectionNumber,
              sectionTitle4Ga,
              suppressErrors,
              rawFastForward,
              showFileUpload
            )
        ).pure[Future]
    }

  def multi(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    browserSectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    suppressErrors: SuppressErrors,
    rawFastForward: List[FastForward],
    showFileUpload: Boolean
  ): play.api.mvc.Action[play.api.mvc.AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        val formModel = formModelOptics.formModelRenderPageOptics.formModel
        val bracket = formModel.bracket(browserSectionNumber)

        def renderMultiFileUploadPage(singleton: Singleton[DataExpanded]) =
          objectStoreAlgebra
            .getEnvelope(cache.form.envelopeId)
            .flatMap { envelope =>
              upscanService
                .upscanInitiate(
                  singleton.upscanInitiateRequests,
                  cache.formTemplateId,
                  browserSectionNumber,
                  cache.form,
                  FormIdData(cache, maybeAccessCode)
                )
                .flatMap(upscanInitiate =>
                  handler
                    .handleSuppressErrors(
                      formModelOptics,
                      List(browserSectionNumber),
                      cache.toCacheData,
                      EnvelopeWithMapping(envelope, cache.form),
                      validationService.validatePageModel _,
                      suppressErrors
                    )
                    .map { handlerResult =>
                      Ok(
                        renderer.renderSection(
                          maybeAccessCode,
                          browserSectionNumber,
                          handlerResult,
                          cache.formTemplate,
                          cache.formTemplateContext.specimenSource,
                          cache.form.envelopeId,
                          singleton,
                          cache.formTemplate.fileSizeLimit.getOrElse(appConfig.formMaxAttachmentSizeMB),
                          cache.formTemplate.allowedFileTypes,
                          appConfig.restrictedFileExtensions,
                          cache.retrievals,
                          cache.form.thirdPartyData.obligations,
                          rawFastForward,
                          formModelOptics,
                          upscanInitiate,
                          AddressRecordLookup.from(cache.form.thirdPartyData)
                        )
                      )
                    }
                )
            }

        bracket match {
          case Bracket.NonRepeatingPage(SingletonWithNumber(singleton, sectionNumber), _) =>
            renderMultiFileUploadPage(singleton)
          case bracket @ Bracket.RepeatingPage(_, _) =>
            throw new Exception("Multi file upload is not supported in repeated page")
          case bracket @ Bracket.AddToList(iterations, _) =>
            val iteration: Bracket.AddToListIteration[DataExpanded] =
              bracket.iterationForSectionNumber(browserSectionNumber)
            val singleton = iteration.singleton(browserSectionNumber)
            renderMultiFileUploadPage(singleton)
        }

    }

}
