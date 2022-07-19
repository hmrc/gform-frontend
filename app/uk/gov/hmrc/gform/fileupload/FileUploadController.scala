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

package uk.gov.hmrc.gform.fileupload

import cats.Show
import cats.implicits._
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import com.softwaremill.quicklens._
import org.slf4j.LoggerFactory
import org.typelevel.ci.CIString
import play.api.data
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.{ Flash, MessagesControllerComponents }
import uk.gov.hmrc.gform.config.{ AppConfig, FileInfoConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.{ FormTemplateKey, gform }

import scala.concurrent.Future
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.auth.models.OperationWithForm.EditForm
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions, GformFlashKeys }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.{ FastForward, FileUploadUtils, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, FormData, FormField, FormIdData, FormModelOptics, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AllowedFileTypes, FormComponentId, FormTemplateId, SectionNumber, SuppressErrors }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.gform.gform.{ Errors, FastForwardService, HasErrors, NoErrors }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.html.components
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.govukfrontend.views.viewmodels.errormessage.ErrorMessage
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.{ ErrorLink, ErrorSummary }

import scala.concurrent.ExecutionContext

class FileUploadController(
  appConfig: AppConfig,
  fileUploadService: FileUploadService,
  auth: AuthenticatedRequestActions,
  gformConnector: GformConnector,
  fastForwardService: FastForwardService,
  i18nSupport: I18nSupport,
  messagesControllerComponents: MessagesControllerComponents,
  frontendAppConfig: FrontendAppConfig
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  private val logger = LoggerFactory.getLogger(getClass)

  implicit val showFlash: Show[Flash] = Show.show(_.data.show)

  def noJsSuccessCallback(
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    fileId: FileId
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => _ => implicit formModelOptics =>
        val formTemplateWithRedirects = request.attrs(FormTemplateKey)
        val formTemplate = formTemplateWithRedirects.formTemplate
        for {
          envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)
          flash    <- checkFile(fileId, envelope, cache.form.envelopeId, formTemplate.allowedFileTypes)
          cacheUpd = cache
                       .modify(_.form.componentIdToFileId)
                       .using(_ + (formComponentId, fileId))
                       .modify(_.form.formData)
                       .using(updateFormDataWithFileUploadMetadata(_, sectionNumber, fileId, envelope))
          res <- fastForwardService
                   .redirectStopAt[SectionSelectorType.Normal](
                     sectionNumber,
                     cacheUpd,
                     maybeAccessCode,
                     formModelOptics
                   )
        } yield res.flashing(flash)

    }

  private def updateFormDataWithFileUploadMetadata(
    formData: FormData,
    sectionNumber: SectionNumber,
    fileId: FileId,
    envelope: Envelope
  )(implicit formModelOptics: FormModelOptics[DataOrigin.Mongo]): FormData =
    envelope
      .find(fileId)
      .map { file =>
        val indexedComponentIds = formModelOptics.formModelRenderPageOptics
          .formModel(sectionNumber)
          .allFormComponentIds
          .map(_.modelComponentId.indexedComponentId)
        val fileMetadataParams = file.metadata.collect {
          case (key, values)
              if indexedComponentIds
                .contains(FormComponentId(key).modelComponentId.indexedComponentId) =>
            FormField(FormComponentId(key).modelComponentId, values.mkString(","))
        }
        formData ++ FormData(fileMetadataParams.toList)
      }
      .getOrElse(formData)

  private def checkFile(fileId: FileId, envelope: Envelope, envelopeId: EnvelopeId, allowedFileTypes: AllowedFileTypes)(
    implicit
    messages: Messages,
    hc: HeaderCarrier
  ): Future[Flash] = {

    val validated: Validated[Flash, Unit] = validateFile(fileId, envelope, allowedFileTypes)

    validated match {
      case Invalid(flash) =>
        logger.warn(show"Attemp to upload invalid file. Deleting FileId: $fileId, flash: $flash")
        fileUploadService
          .deleteFile(envelopeId, fileId)
          .map(_ => flashWithFileId(flash, fileId))
      case Valid(_) => Flash().pure[Future]
    }
  }

  private def flashWithFileId(flash: Flash, fileId: FileId): Flash =
    flash + (GformFlashKeys.FileUploadFileId -> fileId.value)

  private def validateFile(fileId: FileId, envelope: Envelope, allowedFileTypes: AllowedFileTypes)(implicit
    messages: Messages
  ): Validated[Flash, Unit] =
    envelope.find(fileId).fold[Validated[Flash, Unit]](Valid(())) { file =>
      Valid(file)
        .ensure(mkFlash("file.error.empty"))(_.length =!= 0)
        .ensure(
          mkFlash(
            "file.error.type",
            FileInfoConfig.reverseLookup.getOrElse(file.contentType, "").toUpperCase,
            allowedFileTypes.fileExtensions.toList.map(_.toUpperCase).mkString(", ")
          )
        )(_ => validateFileExtension(file) && validateFileType(file, allowedFileTypes))
        .map(_ => ())
    }

  private def mkFlash(s: String, params: String*)(implicit messages: Messages): Flash = Flash(
    Map(GformFlashKeys.FileUploadError -> messages(s, params: _*))
  )

  private def getFileExtension(fileName: String): Option[String] =
    fileName.split("\\.").tail.lastOption

  private def validateFileExtension(file: File): Boolean =
    getFileExtension(file.fileName).fold(false) { v =>
      !appConfig.restrictedFileExtensions.map(_.value).contains(CIString(v))
    }

  private def validateFileType(file: File, allowedFileTypes: AllowedFileTypes): Boolean =
    allowedFileTypes.contentTypes.exists(_ === file.contentType)

  case class FileUploadError(
    errorCode: String,
    reason: String
  )

  def noJsErrorCallback(
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    fileId: FileId
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => sse => formModelOptics =>
        val maybeErrorCode: Option[FileUploadError] =
          for {
            errorCode <- request.getQueryString("errorCode")
            reason    <- request.getQueryString("reason")
          } yield FileUploadError(errorCode, reason)

        val fileUploadError = maybeErrorCode.getOrElse(FileUploadError("Unknown error data", request.rawQueryString))

        // Let's just print the reason why this is being called
        logger.warn(fileUploadError.toString)

        val flash = fileUploadError.errorCode match {
          case "413" =>
            mkFlash(
              "file.error.size",
              cache.formTemplate.fileSizeLimit.getOrElse(appConfig.formMaxAttachmentSizeMB).toString
            )
          case _ => mkFlash("file.error.generic")
        }

        fastForwardService
          .redirectStopAt[SectionSelectorType.Normal](sectionNumber, cache, maybeAccessCode, formModelOptics)
          .map(_.flashing(flashWithFileId(flash, fileId)))
    }

  def addFileId(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    fileId: FileId
  ) = auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, EditForm) {
    implicit request => l => cache => _ => formModelOptics =>
      updateUserData(cache, formComponentId, fileId, maybeAccessCode).map(_ => NoContent)
  }

  private def updateUserData(
    cache: AuthCacheWithForm,
    formComponentId: FormComponentId,
    fileId: FileId,
    maybeAccessCode: Option[AccessCode]
  )(implicit
    hc: HeaderCarrier
  ): Future[Unit] = {
    val userData = FileUploadUtils.updateMapping(formComponentId, fileId, cache.form)
    for {
      _ <- gformConnector.updateUserData(FormIdData.fromForm(cache.form, maybeAccessCode), userData)
    } yield ()
  }

  def requestRemoval(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    formComponentId: FormComponentId
  ) = auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, EditForm) {
    implicit request => implicit lang => _ => implicit sse => _ =>
      val formTemplateWithRedirects = request.attrs(FormTemplateKey)
      val formTemplate = formTemplateWithRedirects.formTemplate

      val deleteUrl =
        routes.FileUploadController.confirmRemoval(
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          formComponentId
        )
      val heading = request.messages.messages("file.delete.confirm")
      val (pageError, fieldErrors) =
        request.flash.get("removeParamMissing").fold((NoErrors: HasErrors, Map.empty[String, ErrorMessage])) { _ =>
          (
            Errors(
              new components.GovukErrorSummary()(
                ErrorSummary(
                  errorList = List(
                    ErrorLink(
                      href = Some("#remove"),
                      content = content.Text(request.messages.messages("generic.error.selectOption"))
                    )
                  ),
                  title = content.Text(request.messages.messages("error.summary.heading"))
                )
              )
            ),
            Map(
              "remove" -> ErrorMessage(
                content = Text(request.messages.messages("generic.error.selectOption"))
              )
            )
          )
        }
      Ok(
        html.form.snippets.confirmation(
          formTemplate,
          maybeAccessCode,
          sectionNumber,
          frontendAppConfig,
          deleteUrl,
          heading,
          pageError,
          fieldErrors
        )
      ).pure[Future]

  }

  private val form: data.Form[String] = play.api.data.Form(
    play.api.data.Forms.single(
      "remove" -> play.api.data.Forms.nonEmptyText
    )
  )

  def confirmRemoval(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    formComponentId: FormComponentId
  ) = auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, EditForm) {
    implicit request => _ => _ => _ => _ =>
      val deleteUrl =
        routes.FileUploadController.deleteFile(
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          formComponentId
        )

      form
        .bindFromRequest()
        .fold(
          _ =>
            Redirect(
              routes.FileUploadController
                .requestRemoval(formTemplateId, maybeAccessCode, sectionNumber, formComponentId)
            )
              .flashing("removeParamMissing" -> "true")
              .pure[Future],
          {
            case "Yes" =>
              Redirect(deleteUrl).pure[Future]
            case "No" =>
              Redirect(gform.routes.FormController.formSection(formTemplateId, maybeAccessCode, sectionNumber))
                .pure[Future]
          }
        )
  }

  def deleteFile(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    formComponentId: FormComponentId
  ) = auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, EditForm) {
    implicit request => implicit l => cache => _ => formModelOptics =>
      processResponseDataFromBody(request, formModelOptics.formModelRenderPageOptics) { _ => variadicFormData => _ =>
        val cacheU = cache
          .modify(_.form.formData)
          .using(_ ++ variadicFormData.toFormData)
        val data = FileUploadUtils.prepareDeleteFile(formComponentId, cacheU.form)

        data match {
          case None =>
            logger.warn(
              s"Attempt to delete file associated with component $formComponentId from envelope. But file is not registered in mapping: ${cacheU.form.componentIdToFileId.mapping}"
            )
            fastForwardService
              .redirectFastForward[SectionSelectorType.Normal](
                cacheU,
                maybeAccessCode,
                formModelOptics,
                sectionNumber.toCoordinates
              )
          case Some((fileToDelete, formDataUpd, mappingUpd)) =>
            logger.info(
              s"Deleting file ${fileToDelete.value} from envelope associated with component $formComponentId."
            )
            val cacheWithFileRemoved = cacheU
              .modify(_.form.formData)
              .setTo(formDataUpd)
              .modify(_.form.componentIdToFileId)
              .setTo(mappingUpd)

            for {
              _ <- fileUploadService.deleteFile(cacheWithFileRemoved.form.envelopeId, fileToDelete)
              _ <- gformConnector
                     .updateUserData(
                       FormIdData.fromForm(cacheWithFileRemoved.form, maybeAccessCode),
                       UserData(
                         cacheWithFileRemoved.form.formData,
                         cacheWithFileRemoved.form.status,
                         cacheWithFileRemoved.form.visitsIndex,
                         cacheWithFileRemoved.form.thirdPartyData,
                         cacheWithFileRemoved.form.componentIdToFileId
                       )
                     )
            } yield {
              val sectionTitle4Ga = sectionTitle4GaFactory(
                formModelOptics.formModelRenderPageOptics.formModel(sectionNumber),
                sectionNumber
              )
              Redirect(
                gform.routes.FormController
                  .form(
                    formTemplateId,
                    maybeAccessCode,
                    sectionNumber,
                    sectionTitle4Ga,
                    SuppressErrors.Yes,
                    FastForward.Yes
                  )
              )
            } // This value will be used only by non-js journey, ajax calls should ignore it.
        }
      }
  }
}
