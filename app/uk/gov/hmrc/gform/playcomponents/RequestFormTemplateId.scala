/*
 * Copyright 2024 HM Revenue & Customs
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

import cats.syntax.eq._
import play.api.mvc.RequestHeader
import play.api.routing.Router.RequestImplicits._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

object RequestFormTemplateId {
  def formTemplateId(rh: RequestHeader): Option[FormTemplateId] = {
    val formTemplateIdParamIndex: Option[Int] = {
      val mayContainsFormTemplateId: Option[Array[Boolean]] =
        rh.handlerDef.map(_.path.split("/")).map(_.map(_.containsSlice("$formTemplateId")))
      mayContainsFormTemplateId.map(_.indexOf(true))
    }

    formTemplateIdParamIndex match {
      case Some(i) if i =!= -1 =>
        val templateId = rh.uri.split("\\?")(0).split("/")(i)
        Some(FormTemplateId(templateId))
      case _ =>
        None
    }
  }
}
