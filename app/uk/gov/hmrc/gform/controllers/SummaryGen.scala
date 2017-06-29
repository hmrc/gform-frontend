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

package uk.gov.hmrc.gform.controllers

import javax.inject.{Inject, Singleton}

import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.Json
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.components.FieldId
import uk.gov.hmrc.gform.models.form._
import uk.gov.hmrc.gform.service.SaveService
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SummaryGen @Inject()(val messagesApi: MessagesApi, val sec: SecuredActions)(implicit ec: ExecutionContext)
  extends FrontendController with I18nSupport {

  def summaryById(formTypeId: FormTypeId, version: Version, formId: FormId) =
    sec.SecureWithTemplateAsync(formTypeId, version) { authContext =>
      implicit request =>
        SaveService.getFormById(formTypeId, version, formId).map( formData =>
          Summary(request.formTemplate).renderSummary(formDataMap(formData), formId)
        )
    }

  def submit(formTypeId: FormTypeId, version: Version) = sec.SecureWithTemplateAsync(formTypeId, version) { authContext =>
    implicit request =>
      processResponseDataFromBody(request) { data =>
        get(data, FieldId("save")) match {
            case "Exit" :: Nil =>
              Future.successful(Ok)
            case "Continue" :: Nil =>
              anyFormId(data) match {
               case Some(formId) =>
                  SaveService.sendSubmission(formTypeId, formId).
                    map( r => Ok(Json.obj("envelope" -> r.body, "formId" -> Json.toJson(formId))))
                case None =>
                  Future.successful(BadRequest("No formId"))
              }
            case _ =>
              Future.successful(BadRequest("Cannot determine action"))
          }
      }
  }

}
