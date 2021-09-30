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

package uk.gov.hmrc.gform.upscan

import akka.actor.Scheduler
import cats.instances.future._
import cats.syntax.either._
import cats.syntax.applicative._
import org.slf4j.{ Logger, LoggerFactory }
import play.api.i18n.{ I18nSupport, Messages }
import play.api.libs.json.{ JsResult, JsValue, Json }
import play.api.mvc.{ Action, AnyContent, Flash, MessagesControllerComponents, Result }
import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.crypto.{ Crypted, CryptoWithKeysFromConfig, PlainText }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.{ AuthenticatedRequestActions, GformFlashKeys, NonAuthenticatedRequestActionsAlgebra }
import uk.gov.hmrc.gform.fileupload.FileUploadFrontendAlgebra
import uk.gov.hmrc.gform.gform.FastForwardService
import uk.gov.hmrc.gform.gformbackend.GformBackEndAlgebra
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.core.Retrying
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, Form, FormIdData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId, SectionNumber }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class UpscanController(
  auth: AuthenticatedRequestActions,
  nonAuth: NonAuthenticatedRequestActionsAlgebra[Future],
  fastForwardService: FastForwardService,
  fileUploadFrontendAlgebra: FileUploadFrontendAlgebra[Future],
  gformBackEndAlgebra: GformBackEndAlgebra[Future],
  upscanService: UpscanAlgebra[Future],
  queryParameterCrypto: CryptoWithKeysFromConfig,
  i18nSupport: I18nSupport,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext, s: Scheduler)
    extends FrontendController(messagesControllerComponents) with Retrying {

  import i18nSupport._

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private def setTransferred(
    form: Form,
    formComponentId: FormComponentId,
    fileId: FileId
  ): Form =
    form.copy(
      componentIdToFileId = form.componentIdToFileId + (formComponentId, fileId)
    )

  def callback(formComponentId: FormComponentId, envelopeId: EnvelopeId, formIdDataCrypted: Crypted): Action[JsValue] =
    nonAuth.json { implicit request => l =>
      implicit val hc = HeaderCarrier() // This is called by upscan service, this call has no session data

      val upscanCallbackPayload: JsResult[UpscanCallback] = request.body.validate[UpscanCallback]

      val decrypted: PlainText = queryParameterCrypto.decrypt(formIdDataCrypted)

      val formIdDataJsResult: JsResult[FormIdData] = Json.parse(decrypted.value).validate[FormIdData]
      val fileId = FileId(formComponentId.value)

      val callbackResult: Either[Unit, Future[Result]] = for {
        formIdData <-
          formIdDataJsResult.asEither.leftMap(error =>
            logger.error(s"Upscan callback - formIdData failed to parse: $formIdDataJsResult, reported error: $error")
          )
        upscanCallback <-
          upscanCallbackPayload.asEither.leftMap(error =>
            logger.error(s"Upscan callback payload failed to parse: ${request.body}, reported error: $error")
          )

      } yield upscanCallback match {
        case upscanCallbackSuccess: UpscanCallback.Success =>
          for {
            file <- upscanService.download(upscanCallbackSuccess.downloadUrl)
            _    <- fileUploadFrontendAlgebra.uploadFile(envelopeId, fileId, upscanCallbackSuccess.uploadDetails, file)
            form <- gformBackEndAlgebra.getForm(formIdData)
            formUpd = setTransferred(form, formComponentId, fileId)
            _                  <- gformBackEndAlgebra.updateUserData(formUpd, formIdData.maybeAccessCode)
            upscanConfirmation <- upscanService.confirm(upscanCallbackSuccess)
          } yield NoContent
        case upscanCallbackFailure: UpscanCallback.Failure =>
          val ref = upscanCallbackFailure.reference.value
          val fileStatus = upscanCallbackFailure.fileStatus
          val failureReason = upscanCallbackFailure.failureDetails.failureReason
          val message = upscanCallbackFailure.failureDetails.message
          logger.info(
            s"Upscan callback failed. Reference $ref, status: $fileStatus, failureReason: $failureReason, message: $message"
          )
          for {
            upscanConfirmation <- upscanService.reject(upscanCallbackFailure)
          } yield NoContent
      }

      callbackResult.getOrElse(NoContent.pure[Future])

    }

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

            retry(upscanService.waitForConfirmation(reference), 2.seconds, 30).flatMap { upscanFileStatus =>
              upscanFileStatus match {
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
                  val flash = mkFlash("file.error.upload.one.only")
                  val fileId = FileId(formComponentId.value)

                  fastForwardService
                    .redirectStopAt[SectionSelectorType.Normal](sectionNumber, cache, maybeAccessCode, formModelOptics)
                    .map(_.flashing(flashWithFileId(flash, fileId)))
              }
            }
        }
    }

  def check(upscanReference: UpscanReference): Action[AnyContent] = nonAuth.async { request => l =>
    upscanService.retrieveConfirmation(upscanReference).flatMap { confirmation =>
      // We need to delete confirmation, since if user uploads another file
      // waitForConfirmation in success callback would see old confirmation, which may not be correct
      // Note. This is not an issue for non-js journey as non-js users have never a chance to reuse upscan reference
      upscanService.deleteConfirmation(upscanReference).map { _ =>
        confirmation match {
          case Some(UpscanFileStatus.Ready) | None => NoContent
          case Some(UpscanFileStatus.Failed)       => Ok("error")
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
      implicit request => implicit l => cache => _ => implicit formModelOptics =>
        val errorMessage = request.getQueryString("errorMessage")
        val key = request.getQueryString("key")
        val errorCode = request.getQueryString("errorCode")
        val errorRequestId = request.getQueryString("errorRequestId")
        val errorResource = request.getQueryString("errorResource")
        logger.info(
          s"Upscan error callback - errorMessage: $errorMessage, errorCode: $errorCode, errorRequestId: $errorRequestId. errorResource: $errorResource, key: $key"
        )

        val flash = mkFlash("file.error.upload.one.only")
        val fileId = FileId(formComponentId.value)

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
