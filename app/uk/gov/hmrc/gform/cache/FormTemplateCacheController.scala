/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.cache

import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateCache, FormTemplateId }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.time.Instant
import scala.concurrent.ExecutionContext

class FormTemplateCacheController(
  messagesControllerComponents: MessagesControllerComponents,
  formTemplateCacheService: FormTemplateCacheService
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  def save(formTemplateId: FormTemplateId): Action[AnyContent] =
    messagesControllerComponents.actionBuilder.async { _ =>
      formTemplateCacheService
        .save(FormTemplateCache(formTemplateId, Instant.now))
        .fold(
          _.asBadRequest,
          _ => Ok
        )
    }

  def delete(formTemplateId: FormTemplateId): Action[AnyContent] =
    messagesControllerComponents.actionBuilder.async { _ =>
      formTemplateCacheService
        .delete(formTemplateId)
        .fold(
          _.asBadRequest,
          res => Ok(Json.toJson(res))
        )
    }
}
