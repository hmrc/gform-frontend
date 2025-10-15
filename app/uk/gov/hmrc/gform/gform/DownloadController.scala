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
import uk.gov.hmrc.http.{ BadRequestException, NotFoundException }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.io.File
import scala.concurrent.{ ExecutionContext, Future }

class DownloadController(
  messagesControllerComponents: MessagesControllerComponents,
  environment: Environment
)(implicit
  ec: ExecutionContext
) extends FrontendController(messagesControllerComponents) {

  private val allowedFileInfo: Map[String, String] = Map(
    ("xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
    ("ods", "application/vnd.oasis.opendocument.spreadsheet"),
    ("docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document"),
    ("odt", "application/vnd.oasis.opendocument.text")
  )

  private def isValidFilename(filename: String): Boolean =
    filename.nonEmpty &&
      !filename.contains("..") &&
      !filename.contains("/") &&
      !filename.contains("\\") &&
      filename.matches("[a-zA-Z0-9._-]+") &&
      filename.length <= 255

  private def isAllowedExtension(filename: String): Boolean = {
    val extension = filename.substring(filename.lastIndexOf('.') + 1).toLowerCase
    allowedFileInfo.contains(extension)
  }

  def downloadFile(
    filename: String
  ): Action[AnyContent] =
    messagesControllerComponents.actionBuilder.async { _ =>
      if (!isValidFilename(filename)) {
        Future.failed(new IllegalArgumentException(s"Download file: Invalid filename: $filename"))
      } else if (!isAllowedExtension(filename)) {
        Future.failed(new BadRequestException(s"Download file: File type not supported: $filename"))
      } else {
        val extension = filename.substring(filename.lastIndexOf('.') + 1).toLowerCase
        val resourcesDir = environment.getFile("conf/resources")
        val file = new File(resourcesDir, filename)

        val canonicalFile = file.getCanonicalPath
        val canonicalDir = resourcesDir.getCanonicalPath

        if (!canonicalFile.startsWith(canonicalDir)) {
          Future.failed(new IllegalArgumentException("Download file: Access denied"))
        } else if (!file.exists()) {
          Future.failed(new NotFoundException(s"Download file: File $filename does not exist"))
        } else {
          Future.successful(
            Ok.sendFile(
              content = file,
              fileName = _ => Some(filename)
            ).withHeaders(
              CONTENT_DISPOSITION -> s"""inline; filename="$filename"""",
              CONTENT_TYPE        -> allowedFileInfo(extension),
              CONTENT_LENGTH      -> file.length.toString
            )
          )
        }
      }
    }

}
