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

package uk.gov.hmrc.gform.objectStore

import cats.implicits._
import com.softwaremill.quicklens._
import org.slf4j.LoggerFactory
import play.api.data
import play.api.i18n.I18nSupport
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FileComponentId
import uk.gov.hmrc.gform.{ FormTemplateKey, gform }

import scala.concurrent.Future
import uk.gov.hmrc.gform.auth.models.OperationWithForm.EditForm
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.{ FastForward, FileUploadUtils, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.{ FormIdData, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, SectionNumber, SuppressErrors }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.gform.gform.{ Errors, FastForwardService, HasErrors, NoErrors }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.html.components
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.govukfrontend.views.viewmodels.errormessage.ErrorMessage
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.{ ErrorLink, ErrorSummary }

import scala.concurrent.ExecutionContext

class ObjectStoreController(
  objectStoreService: ObjectStoreService,
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

  def requestRemoval(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    fileComponentId: FileComponentId
  ) = auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, EditForm) {
    implicit request => implicit lang => _ => implicit sse => _ =>
      val formTemplateContext = request.attrs(FormTemplateKey)
      val formTemplate = formTemplateContext.formTemplate

      val deleteUrl =
        routes.ObjectStoreController.confirmRemoval(
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          fileComponentId
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
              "remove" -> ErrorMessage.errorMessageWithDefaultStringsTranslated(
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
    fileComponentId: FileComponentId
  ) = auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, EditForm) {
    implicit request => _ => _ => _ => _ =>
      val deleteUrl =
        routes.ObjectStoreController.deleteFile(
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          fileComponentId
        )

      form
        .bindFromRequest()
        .fold(
          _ =>
            Redirect(
              routes.ObjectStoreController
                .requestRemoval(formTemplateId, maybeAccessCode, sectionNumber, fileComponentId)
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
    fileComponentId: FileComponentId
  ) = auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, EditForm) {
    implicit request => implicit l => cache => implicit sse => formModelOptics =>
      processResponseDataFromBody(request, formModelOptics.formModelRenderPageOptics) { _ => variadicFormData => _ =>
        val cacheU = cache
          .modify(_.form.formData)
          .using(_ ++ variadicFormData.toFormData)
        val data =
          FileUploadUtils.prepareDeleteFile(fileComponentId, cacheU.form)

        data match {
          case None =>
            logger.warn(
              s"Attempt to delete file associated with component $fileComponentId from envelope. But file is not registered in mapping: ${cacheU.form.componentIdToFileId.mapping}"
            )
            fastForwardService
              .redirectFastForward[SectionSelectorType.Normal](
                cacheU,
                maybeAccessCode,
                formModelOptics,
                Some(sectionNumber),
                SuppressErrors.Yes
              )
          case Some((fileToDelete, formDataUpd, mappingUpd)) =>
            logger.info(
              s"Deleting file ${fileToDelete.value} from envelope associated with component $fileComponentId."
            )
            val cacheWithFileRemoved = cacheU
              .modify(_.form.formData)
              .setTo(formDataUpd)
              .modify(_.form.componentIdToFileId)
              .setTo(mappingUpd)

            for {
              _ <- objectStoreService.deleteFile(cacheWithFileRemoved.form.envelopeId, fileToDelete)
              _ <- gformConnector
                     .updateUserData(
                       FormIdData.fromForm(cacheWithFileRemoved.form, maybeAccessCode),
                       UserData(
                         cacheWithFileRemoved.form.formData,
                         cacheWithFileRemoved.form.status,
                         cacheWithFileRemoved.form.visitsIndex,
                         cacheWithFileRemoved.form.thirdPartyData,
                         cacheWithFileRemoved.form.componentIdToFileId,
                         cacheWithFileRemoved.form.taskIdTaskStatus
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
                    List(FastForward.Yes)
                  )
              )
            } // This value will be used only by non-js journey, ajax calls should ignore it.
        }
      }
  }
}
