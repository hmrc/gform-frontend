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
import cats.syntax.flatMap._
import play.api.http.HttpEntity
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents, Request, ResponseHeader, Result }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.controllers.{ AuthenticatedRequestActionsAlgebra, Direction, ErrResponder, Exit, SummaryContinue }
import uk.gov.hmrc.gform.fileupload.{ EnvelopeWithMapping, FileUploadService }
import uk.gov.hmrc.gform.gform
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.{ DestinationList, DestinationPrint }
import uk.gov.hmrc.gform.summary.SummaryRenderingService
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.gform.views.hardcoded.{ SaveAcknowledgement, SaveWithAccessCode }
import uk.gov.hmrc.gform.views.html.hardcoded.pages.{ save_acknowledgement, save_with_access_code }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class SummaryController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  fileUploadService: FileUploadService,
  validationService: ValidationService,
  pdfService: PdfGeneratorService,
  gformConnector: GformConnector,
  frontendAppConfig: FrontendAppConfig,
  errResponder: ErrResponder,
  summaryRenderingService: SummaryRenderingService,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  def summaryById(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth
      .authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.ViewSummary) {
        implicit request => implicit l => cache => implicit sse => formModelOptics =>
          summaryRenderingService
            .getSummaryHTML(maybeAccessCode, cache, SummaryPagePurpose.ForUser, formModelOptics)
            .map(Ok(_))
      }

  def submit(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode], save: Direction): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.AcceptSummary
    ) { implicit request: Request[AnyContent] => implicit l => cache => implicit sse => formModelOptics =>
      processResponseDataFromBody(request, None, formModelOptics.formModelRenderPageOptics) {
        requestRelatedData => variadicFormData =>
          val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)

          val isFormValidF = for {
            envelope <- envelopeF
            validationResult <- validationService
                                  .validateFormModel(
                                    cache.toCacheData,
                                    EnvelopeWithMapping(envelope, cache.form),
                                    formModelOptics.formModelVisibilityOptics
                                  )
          } yield validationResult.isFormValid

          val redirectToDeclarationOrPrint = gformConnector
            .updateUserData(
              FormIdData(cache.retrievals, formTemplateId, maybeAccessCode),
              UserData(
                cache.form.formData,
                Validated,
                cache.form.visitsIndex,
                cache.form.thirdPartyData,
                cache.form.componentIdToFileId
              )
            )
            .map { _ =>
              cache.formTemplate.destinations match {
                case _: DestinationList =>
                  Redirect(
                    routes.DeclarationController
                      .showDeclaration(maybeAccessCode, formTemplateId, SuppressErrors.Yes)
                  )

                case _: DestinationPrint =>
                  Redirect(
                    routes.PrintSectionController
                      .showPrintSection(formTemplateId, maybeAccessCode)
                  )
              }

            }

          val redirectToSummary =
            Redirect(routes.SummaryController.summaryById(formTemplateId, maybeAccessCode))
          val handleSummaryContinue = for {
            result <- isFormValidF.ifM(
                        redirectToDeclarationOrPrint,
                        redirectToSummary.pure[Future]
                      )
          } yield result

          def handleExit(formTemplate: FormTemplate): Result =
            maybeAccessCode match {
              case Some(accessCode) =>
                val saveWithAccessCode = new SaveWithAccessCode(cache.formTemplate, accessCode)
                Ok(save_with_access_code(saveWithAccessCode, frontendAppConfig))
              case _ =>
                if (formTemplate.authConfig.isEmailAuthConfig) {
                  Redirect(gform.routes.SaveAcknowledgementController.show(formTemplateId))
                } else {
                  val call = routes.SummaryController.summaryById(cache.formTemplate._id, maybeAccessCode)
                  val saveAcknowledgement = new SaveAcknowledgement(cache.formTemplate, cache.form.envelopeExpiryDate)
                  Ok(save_acknowledgement(saveAcknowledgement, call, frontendAppConfig))
                }
            }

          save match {
            case Exit            => handleExit(cache.formTemplate).pure[Future]
            case SummaryContinue => handleSummaryContinue
            case _               => BadRequest("Cannot determine action").pure[Future]
          }
      }
    }

  def downloadPDF(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.DownloadSummaryPdf
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      pdfService
        .generateSummaryPDF(maybeAccessCode, cache, SummaryPagePurpose.ForUser, formModelOptics)
        .map { pdfStream =>
          Result(
            header = ResponseHeader(200, Map.empty),
            body = HttpEntity.Streamed(pdfStream, None, Some("application/pdf"))
          )
        }
    }
}
