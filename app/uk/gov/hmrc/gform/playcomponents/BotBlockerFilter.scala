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

package uk.gov.hmrc.gform.playcomponents

import akka.stream.Materializer
import org.slf4j.{ Logger, LoggerFactory }
import play.api.mvc.Results.Redirect
import play.api.mvc.{ Filter, RequestHeader, Result }
import uk.gov.hmrc.gform.FormTemplateKey

import scala.concurrent.Future

class BotBlockerFilter(implicit override val mat: Materializer) extends Filter {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private def isBot(userAgent: String): Boolean =
    userAgent.contains("GPTBot")

  override def apply(next: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] = {
    val userAgent: String = rh.headers.get("User-Agent").getOrElse("")

    if (isBot(userAgent)) {

      val formTemplateContext = rh.attrs(FormTemplateKey)
      val formTemplate = formTemplateContext.formTemplate
      logger.info(
        s"Bot detected while trying to access ${formTemplate._id.value}. User agent used: $userAgent"
      )
      Future.successful(
        Redirect(uk.gov.hmrc.gform.auth.routes.ErrorController.botForbidden(formTemplate._id).url)
      )
    } else {
      next(rh)
    }
  }
}
