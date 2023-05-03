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
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

class DownloadController(messagesControllerComponents: MessagesControllerComponents, environment: Environment)(implicit
  ec: ExecutionContext
) extends FrontendController(messagesControllerComponents) {

  def downloadNipTemplate(fileExt: String): Action[AnyContent] = Action { _ =>
    val fileName = s"nipClaimScheduleTemplate.$fileExt"
    val file = environment.getFile(s"conf/resources/$fileName")
    if (file.exists()) {
      Ok.sendFile(
        content = file,
        fileName = _ => Some(fileName)
      ).withHeaders(
        CONTENT_DISPOSITION -> s"inline; filename=$fileName",
        CONTENT_TYPE -> FileInfoConfig.lookup
          .get(fileExt)
          .getOrElse("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
          .toString,
        CONTENT_LENGTH -> file.length.toString
      )
    } else {
      NotFound
    }
  }

}
