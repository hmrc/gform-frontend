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
import uk.gov.hmrc.http.NotFoundException
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class DownloadController(
  messagesControllerComponents: MessagesControllerComponents,
  environment: Environment
)(implicit
  ec: ExecutionContext
) extends FrontendController(messagesControllerComponents) {

  private val allowedFileInfo: Map[String, String] = Map(
    ("xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
    ("ods", "application/vnd.oasis.opendocument.spreadsheet")
  )

  def downloadFile(
    filename: String
  ): Action[AnyContent] =
    messagesControllerComponents.actionBuilder.async { _ =>
      val extension = filename.substring(filename.lastIndexOf('.') + 1)
      val file = environment.getFile(s"conf/resources/$filename")
      if (file.exists()) {
        Future.successful(
          Ok.sendFile(
            content = file,
            fileName = _ => Some(filename)
          ).withHeaders(
            CONTENT_DISPOSITION -> s"inline; filename=$filename",
            CONTENT_TYPE -> allowedFileInfo.getOrElse(
              extension,
              throw new IllegalArgumentException(s"File $filename is not supported by this operation")
            ),
            CONTENT_LENGTH -> file.length.toString
          )
        )
      } else {
        Future.failed(new NotFoundException(s"File $filename does not exist"))
      }
    }

}
