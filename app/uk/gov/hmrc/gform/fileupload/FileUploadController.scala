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

package uk.gov.hmrc.gform.fileupload

import cats.syntax.eq._
import cats.syntax.show._
import play.api.libs.json.Json
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.gform.auth.models.OperationWithForm.EditForm
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormData, FormIdData, InProgress, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

class FileUploadController(
  fileUploadService: FileUploadService,
  auth: AuthenticatedRequestActions,
  gformConnector: GformConnector,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  def addFileId(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    fileId: FileId
  ) = auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, EditForm) {
    implicit request => l => cache => _ => formModelOptics =>
      val mapping = cache.form.componentIdToFileId
      val modelComponentId = formComponentId.modelComponentId

      def resolveFileId(envelope: Envelope): (FileId, UserData) = {
        val resolvedFileId =
          if (fileId === FileId.empty) {
            mapping.fileIdFor(formComponentId)
          } else {
            EnvelopeWithMapping(envelope, cache.form)
              .find(modelComponentId)
              .map(_.fileId)
              .getOrElse(throw new IllegalArgumentException(s"No associated FileId found for $modelComponentId"))
          }

        val mappingUpd = mapping + (formComponentId, resolvedFileId)

        val userData = UserData(
          cache.form.formData, // FormData are updated by Submit button from the browser
          InProgress,
          cache.form.visitsIndex,
          cache.form.thirdPartyData,
          mappingUpd
        )
        (resolvedFileId, userData)
      }

      for {
        envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)
        (resolvedFileId, userData) = resolveFileId(envelope)
        _ <- gformConnector.updateUserData(FormIdData.fromForm(cache.form, maybeAccessCode), userData)
      } yield {
        Ok(Json.toJson(resolvedFileId))
      }

  }

  def deleteFile(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId
  ) = auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, EditForm) {
    implicit request => l => cache => _ => formModelOptics =>
      val mapping = cache.form.componentIdToFileId
      val fileToDelete = mapping
        .find(formComponentId)
        .getOrElse(throw new IllegalArgumentException(show"No file found for $formComponentId."))

      val mappingUpd = mapping - formComponentId
      val formDataUpd =
        FormData(cache.form.formData.fields.filterNot(formField => formField.id === formComponentId.modelComponentId))

      val userData = UserData(
        formDataUpd,
        InProgress,
        cache.form.visitsIndex,
        cache.form.thirdPartyData,
        mappingUpd
      )

      for {
        _ <- fileUploadService.deleteFile(cache.form.envelopeId, fileToDelete)
        _ <- gformConnector.updateUserData(FormIdData.fromForm(cache.form, maybeAccessCode), userData)
      } yield {
        NoContent
      }
  }

}
