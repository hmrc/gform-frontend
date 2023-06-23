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
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, InternalLink, LinkCtx }
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

  private val allowedFileInfo: Map[String, String] = Map(
    ("xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
    ("ods", "application/vnd.oasis.opendocument.spreadsheet")
  )

  def downloadFile(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    fileName: String
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.DownloadFileByInternalLink
    ) { _ => _ => _ => _ => formModelOptics =>
      val formModel = formModelOptics.formModelRenderPageOptics.formModel
      val allExprs = formModel.brackets.toBracketsPlains.toList.flatMap(_.allExprs(formModel))
      val extension = fileName.substring(fileName.lastIndexOf('.') + 1)

      if (!allExprs.contains(LinkCtx(InternalLink.Download(fileName)))) {
        Future.failed(
          new NotFoundException(
            s"link.download.$fileName expr does not exist in $formTemplateId form"
          )
        )
      } else {
        val file = environment.getFile(s"conf/resources/$fileName")
        if (file.exists()) {
          Future.successful(
            Ok.sendFile(
              content = file,
              fileName = _ => Some(fileName)
            ).withHeaders(
              CONTENT_DISPOSITION -> s"inline; filename=$fileName",
              CONTENT_TYPE -> allowedFileInfo.getOrElse(
                extension,
                throw new IllegalArgumentException(s"File $fileName is not supported by this operation")
              ),
              CONTENT_LENGTH -> file.length.toString
            )
          )
        } else {
          Future.failed(new NotFoundException(s"File $fileName does not exist"))
        }
      }
    }
}
