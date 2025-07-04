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

package uk.gov.hmrc.gform.upscan

import org.apache.pekko.actor.Scheduler
import org.slf4j.{ Logger, LoggerFactory }
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.{ Action, AnyContent, Flash, MessagesControllerComponents }
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.gform.auth.models.{ OperationWithForm, OperationWithoutForm }
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions, GformFlashKeys }
import uk.gov.hmrc.gform.core.Retrying
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.gform.gform.FastForwardService
import uk.gov.hmrc.gform.gformbackend.GformBackEndAlgebra
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormIdData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SmartString }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future }

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
      implicit request => implicit l => cache => implicit sse => implicit formModelOptics =>
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
                    for {
                      res <- fastForwardService
                               .redirectStopAt[SectionSelectorType.Normal](
                                 sectionNumber,
                                 cache.copy(form = form),
                                 maybeAccessCode,
                                 formModelOptics,
                                 SuppressErrors.No
                               )
                    } yield {
                      val header = request.messages.messages("file.fileuploaded")
                      val filename =
                        confirmation.filename.getOrElse(SmartString.blank.transform(_ => "File", _ => "Ffeil").value())

                      val content = request.messages.messages("file.upload.success", HtmlFormat.escape(filename))
                      res.flashing(
                        "success" -> s"$header|$content"
                      )
                    }
                  }
                case UpscanFileStatus.Failed =>
                  val fileId = FileId(formComponentId.value)
                  val maybeFormComponent = formModelOptics.formModelVisibilityOptics.fcLookup.get(formComponentId)
                  val (maybeFileTypes, maxFileSize) = getFileUploadProperties(maybeFormComponent, cache)

                  logger.info(
                    s"Upscan failed - status: ${confirmation.status}, failureReason: ${confirmation.confirmationFailure}"
                  )
                  val flash = confirmation.confirmationFailure match {
                    case ConfirmationFailure.GformValidationFailure(UpscanValidationFailure.EntityTooLarge) =>
                      mkFlash(
                        "file.error.size",
                        maxFileSize.toString
                      )
                    case ConfirmationFailure.GformValidationFailure(UpscanValidationFailure.EntityTooSmall) =>
                      mkFlash("file.error.empty")
                    case ConfirmationFailure.GformValidationFailure(UpscanValidationFailure.FileNameTooLong) =>
                      mkFlash("file.error.filename.too.long")
                    case ConfirmationFailure.GformValidationFailure(UpscanValidationFailure.FileNameInvalid) =>
                      mkFlash("file.error.filename.invalid")
                    case ConfirmationFailure.GformValidationFailure(
                          UpscanValidationFailure.InvalidFileExtension(extension)
                        ) =>
                      mkFlash("file.error.invalid.extension", extension)
                    case ConfirmationFailure.GformValidationFailure(
                          UpscanValidationFailure.InvalidFileType(_, fileMimeType)
                        ) =>
                      mkFlash(
                        "file.error.type",
                        allowedFileExtensions(maybeFileTypes, cache)
                      )
                    case ConfirmationFailure.UpscanFailure(FailureDetails("REJECTED", "EntityTooLarge")) =>
                      mkFlash(
                        "file.error.size",
                        maxFileSize.toString
                      )
                    case ConfirmationFailure.UpscanFailure(FailureDetails("REJECTED", _)) =>
                      mkFlash(
                        "file.error.rejected",
                        allowedFileExtensions(maybeFileTypes, cache)
                      )
                    case ConfirmationFailure.UpscanFailure(FailureDetails("QUARANTINE", _)) =>
                      mkFlash(
                        "generic.error.virus",
                        maybeFormComponent
                          .map(fc => fc.errorPlaceholder.map(_.value()).getOrElse(fc.label.value()))
                          .getOrElse("")
                      )
                    case _ => mkFlash("file.error.generic")
                  }

                  fastForwardService
                    .redirectStopAt[SectionSelectorType.Normal](
                      sectionNumber,
                      cache,
                      maybeAccessCode,
                      formModelOptics,
                      SuppressErrors.Yes
                    )
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
            case Some(UpscanConfirmation(_, UpscanFileStatus.Ready, _, _)) | None => NoContent
            case Some(
                  UpscanConfirmation(
                    _,
                    UpscanFileStatus.Failed,
                    ConfirmationFailure.GformValidationFailure(failureDetails),
                    _
                  )
                ) =>
              Ok(failureDetails.toJsCode)
            case Some(
                  UpscanConfirmation(_, UpscanFileStatus.Failed, ConfirmationFailure.UpscanFailure(failureDetails), _)
                ) =>
              Ok(failureDetails.failureReason)

            case Some(
                  UpscanConfirmation(_, UpscanFileStatus.Failed, ConfirmationFailure.AllOk, _)
                ) =>
              throw new Exception("Upscan problem - 'Failed' status cannot have ConfirmationFailure.AllOk")
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
        val maybeFormComponent = formModelOptics.formModelVisibilityOptics.fcLookup.get(formComponentId)
        val (maybeFileTypes, maxFileSize) = getFileUploadProperties(maybeFormComponent, cache)

        val flash = errorCode match {
          case Some("InvalidArgument") =>
            mkFlash(
              "file.error.invalid.argument",
              allowedFileExtensions(maybeFileTypes, cache),
              maybeFormComponent
                .map(fc =>
                  fc.errorShortName.getOrElse(SmartString.blank.transform(_ => "a file", _ => "ffeil")).value()
                )
                .getOrElse("")
            )
          case Some("EntityTooLarge") =>
            mkFlash(
              "file.error.size",
              maxFileSize.toString
            )
          case Some("EntityTooSmall") => mkFlash("file.error.empty")
          case Some("MissingFile") =>
            mkFlash(
              "file.error.missing",
              formModelOptics.formModelVisibilityOptics.fcLookup
                .get(formComponentId)
                .map(fc =>
                  fc.errorShortName.getOrElse(SmartString.blank.transform(_ => "a file", _ => "ffeil")).value()
                )
                .getOrElse("")
            )
          case _ => mkFlash("file.error.generic")
        }

        fastForwardService
          .redirectStopAt[SectionSelectorType.Normal](
            sectionNumber,
            cache,
            maybeAccessCode,
            formModelOptics,
            SuppressErrors.Yes
          )
          .map(_.flashing(flashWithFileId(flash, fileId)))
    }

  private def flashWithFileId(flash: Flash, fileId: FileId): Flash =
    flash + (GformFlashKeys.FileUploadFileId -> fileId.value)

  private def mkFlash(s: String, params: String*)(implicit messages: Messages): Flash =
    Flash(
      Map(GformFlashKeys.FileUploadError -> messages(s, params: _*))
    )

  private def getFileUploadProperties(
    maybeFormComponent: Option[FormComponent],
    cache: AuthCacheWithForm
  ): (Option[AllowedFileTypes], Int) = {
    val defaultSize = cache.formTemplate.fileSizeLimit.getOrElse(appConfig.formMaxAttachmentSizeMB)
    maybeFormComponent
      .collect {
        case IsFileUpload(f)      => f.allowedFileTypes -> f.fileSizeLimit.getOrElse(defaultSize)
        case IsMultiFileUpload(f) => f.allowedFileTypes -> f.fileSizeLimit.getOrElse(defaultSize)
      }
      .getOrElse(Option.empty[AllowedFileTypes] -> defaultSize)
  }

  private def allowedFileExtensions(maybeAllowedFileTypes: Option[AllowedFileTypes], cache: AuthCacheWithForm)(implicit
    messages: Messages
  ): String = {
    val orSeparator = messages("global.or")
    val fileExtensions = maybeAllowedFileTypes
      .getOrElse(cache.formTemplate.allowedFileTypes)
      .fileExtensions
      .toList
      .map(_.toUpperCase)

    orMkString(fileExtensions, orSeparator)
  }

  private def orMkString(list: List[String], lastSeparator: String): String =
    list match {
      case Nil         => ""
      case head :: Nil => head
      case _           => list.init.mkString(", ") + s" $lastSeparator " + list.last
    }

}
