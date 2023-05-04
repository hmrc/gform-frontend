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

package uk.gov.hmrc.gform.gform

import play.api.Environment
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileExtension, FormTemplateId, InternalLink, LinkCtx }
import uk.gov.hmrc.http.NotFoundException
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class DownloadController(
  messagesControllerComponents: MessagesControllerComponents,
  auth: AuthenticatedRequestActions,
  environment: Environment
)(implicit
  ec: ExecutionContext
) extends FrontendController(messagesControllerComponents) {

  def downloadFile(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    fileName: String,
    fileExt: FileExtension
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.DownloadFile
    ) { _ => _ => _ => _ => formModelOptics =>
      val formModel = formModelOptics.formModelRenderPageOptics.formModel
      val allExprs = formModel.brackets.toBracketsPlains.toList.flatMap(_.allExprs(formModel))

      if (!allExprs.contains(LinkCtx(InternalLink.DownloadFile(fileName, fileExt)))) {
        Future.failed(
          new NotFoundException(
            s"link.download.$fileName.${fileExt.value} expr does not exist in $formTemplateId form"
          )
        )
      } else {
        val fileFullName = s"$fileName.${fileExt.value}"
        val file = environment.getFile(s"conf/resources/$fileFullName")
        if (file.exists()) {
          Future.successful(
            Ok.sendFile(
              content = file,
              fileName = _ => Some(fileFullName)
            ).withHeaders(
              CONTENT_DISPOSITION -> s"inline; filename=$fileFullName",
              CONTENT_TYPE -> FileInfoConfig.lookup
                .get(fileExt.value)
                .getOrElse("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
                .toString,
              CONTENT_LENGTH -> file.length.toString
            )
          )
        } else {
          Future.failed(new NotFoundException(s"File: $fileName.$fileExt does not exist"))
        }
      }
    }
}
