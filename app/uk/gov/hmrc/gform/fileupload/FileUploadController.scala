/*
 * Copyright 2017 HM Revenue & Customs
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

import javax.inject.Inject

import play.api.libs.json.Json
import play.api.mvc.Action
import uk.gov.hmrc.gform.controllers.ControllersModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormId }
import uk.gov.hmrc.play.frontend.controller.FrontendController

class FileUploadController @Inject() (
  gformBackendModule: GformBackendModule,
  controllersModule: ControllersModule,
  fileUploadModule: FileUploadModule
)
    extends FrontendController {
  import uk.gov.hmrc.gform.controllers.AuthenticatedRequest._

  def deleteFile(formId: FormId, fileId: FileId) = authentication.async(formIdOpt = Some(formId)) { implicit c =>
    for {
      form <- gformConnector.getForm(formId)
      _ <- fileUploadService.deleteFile(form.envelopeId, fileId)
    } yield NoContent
  }

  private lazy val gformConnector = gformBackendModule.gformConnector
  private lazy val authentication = controllersModule.authenticatedRequestActions
  private lazy val fileUploadService = fileUploadModule.fileUploadService
}
