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

package uk.gov.hmrc.gform.fileupload

import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.gform.auth.models.OperationWithForm.EditForm
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormIdData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.ExecutionContext

class FileUploadController(
  fileUploadService: FileUploadService,
  auth: AuthenticatedRequestActions,
  gformConnector: GformConnector,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  def deleteFile(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    fileId: FileId
  ) = auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, EditForm) {
    implicit request => implicit l => cache => _ => formModelOptics =>
      for {
        form <- gformConnector.getForm(FormIdData(cache.retrievals, formTemplateId, maybeAccessCode))
        _    <- fileUploadService.deleteFile(form.envelopeId, fileId)
      } yield NoContent
  }

}
