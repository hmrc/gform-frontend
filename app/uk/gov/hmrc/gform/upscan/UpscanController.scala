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

package uk.gov.hmrc.gform.upscan

import akka.actor.Scheduler
import org.slf4j.{ Logger, LoggerFactory }
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.{ Action, AnyContent, Flash, MessagesControllerComponents }
import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.{ OperationWithForm, OperationWithoutForm }
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.controllers.{ AuthenticatedRequestActions, GformFlashKeys }
import uk.gov.hmrc.gform.gform.FastForwardService
import uk.gov.hmrc.gform.gformbackend.GformBackEndAlgebra
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.core.Retrying
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormIdData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId, SectionNumber }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax

class UpscanController(
  auth: AuthenticatedRequestActions,
  fastForwardService: FastForwardService,
  gformBackEndAlgebra: GformBackEndAlgebra[Future],
  upscanService: UpscanAlgebra[Future],
  i18nSupport: I18nSupport,
  messagesControllerComponents: MessagesControllerComponents,
  appConfig: AppConfig
)(implicit ec: ExecutionContext, s: Scheduler)
    extends FrontendController(messagesControllerComponents) with Retrying {

  import i18nSupport._

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def success(
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => _ => implicit formModelOptics =>
        val maybeKey = request.getQueryString("key")

        maybeKey match {
          case None => throw new Exception("Key missing in success callback from upscan. Possible bug in upscan!")
          case Some(key) =>
            val reference = UpscanReference(key)

            val formIdData: FormIdData = FormIdData(cache, maybeAccessCode)

            retry(upscanService.retrieveConfirmationOrFail(reference), 2.seconds, 30).flatMap { confirmation =>
              confirmation.status match {
                case UpscanFileStatus.Ready =>
                  gformBackEndAlgebra.getForm(formIdData).flatMap { form =>
                    fastForwardService
                      .redirectStopAt[SectionSelectorType.Normal](
                        sectionNumber,
                        cache.copy(form = form),
                        maybeAccessCode,
                        formModelOptics
                      )
                  }
                case UpscanFileStatus.Failed =>
                  val fileId = FileId(formComponentId.value)

                  logger.info(
                    s"Upscan failed - status: ${confirmation.status}, failureReason: ${confirmation.failureDetails.failureReason}, message: ${confirmation.failureDetails.message}"
                  )
                  val flash = confirmation.failureDetails match {
                    case FailureDetails("EntityTooLarge", _) =>
                      mkFlash("file.error.size", appConfig.formMaxAttachmentSizeMB.toString)
                    case FailureDetails("EntityTooSmall", _) => mkFlash("file.error.empty")
                    case FailureDetails("InvalidFileType", _) | FailureDetails("REJECTED", _) =>
                      mkFlash(
                        "file.error.type",
                        "",
                        "PDF, JPEG, XLSX, ODS, DOCX, ODT, PPTX, ODP"
                      )
                    case _ => mkFlash("file.error.upload.one.only")
                  }

                  fastForwardService
                    .redirectStopAt[SectionSelectorType.Normal](sectionNumber, cache, maybeAccessCode, formModelOptics)
                    .map(_.flashing(flashWithFileId(flash, fileId)))
              }
            }
        }
    }

  def check(formTemplateId: FormTemplateId, upscanReference: UpscanReference): Action[AnyContent] =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) { implicit request => l => cache =>
      upscanService.retrieveConfirmation(upscanReference).flatMap { confirmation =>
        // We need to delete confirmation, since if user uploads another file
        // waitForConfirmation in success callback would see old confirmation, which may not be correct
        // Note. This is not an issue for non-js journey as non-js users have never a chance to reuse upscan reference
        upscanService.deleteConfirmation(upscanReference).map { _ =>
          confirmation match {
            case Some(UpscanConfirmation(_, UpscanFileStatus.Ready, _)) | None => NoContent
            case Some(UpscanConfirmation(_, UpscanFileStatus.Failed, failureDetails)) =>
              Ok(failureDetails.failureReason)
          }
        }
      }
    }

  def error(
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => implicit formModelOptics =>
        val errorMessage = request.getQueryString("errorMessage")
        val key = request.getQueryString("key")
        val errorCode = request.getQueryString("errorCode")
        val errorRequestId = request.getQueryString("errorRequestId")
        val errorResource = request.getQueryString("errorResource")
        logger.info(
          s"Upscan error callback - errorMessage: $errorMessage, errorCode: $errorCode, errorRequestId: $errorRequestId. errorResource: $errorResource, key: $key"
        )

        val fileId = FileId(formComponentId.value)

        val flash = errorCode match {
          case Some("InvalidArgument") =>
            mkFlash(
              "generic.error.upload",
              formModelOptics.formModelVisibilityOptics.fcLookup
                .get(formComponentId)
                .map(_.label.value)
                .getOrElse("")
            )
          case Some("EntityTooLarge") => mkFlash("file.error.size", appConfig.formMaxAttachmentSizeMB.toString)
          case Some("EntityTooSmall") => mkFlash("file.error.empty")
          case _                      => mkFlash("file.error.upload.one.only")
        }

        fastForwardService
          .redirectStopAt[SectionSelectorType.Normal](sectionNumber, cache, maybeAccessCode, formModelOptics)
          .map(_.flashing(flashWithFileId(flash, fileId)))
    }

  private def flashWithFileId(flash: Flash, fileId: FileId): Flash =
    flash + (GformFlashKeys.FileUploadFileId -> fileId.value)

  private def mkFlash(s: String, params: String*)(implicit messages: Messages): Flash = Flash(
    Map(GformFlashKeys.FileUploadError -> messages(s, params: _*))
  )
}
