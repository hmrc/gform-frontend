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

package uk.gov.hmrc.gform.service

import play.api.libs.json._
import uk.gov.hmrc.gform.connectors.GformConnector
import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.UserId
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object RetrieveService {

  private def formTemplateFromJson(formTemplate: JsObject): Either[String, FormTemplate] = {
    formTemplate.validate[FormTemplate] match {
      case JsSuccess(formTemplate, _) => Right(formTemplate)
      case JsError(error) => Left(error.toString)
    }
  }

  def getFormTemplate(formTypeId: FormTypeId, version: Version)(implicit hc: HeaderCarrier): Future[Either[String, FormTemplate]] = {
    val templateF = GformConnector.formTemplate(formTypeId, version)

    templateF.map(_ match {
      case Some(jsonTemplate) => formTemplateFromJson(jsonTemplate)
      case None => Left(s"No template for formTypeId $formTypeId version $version")
    })
  }

  def getStartedForm(userId: UserId, formTypeId: FormTypeId, version: Version)(implicit hc: HeaderCarrier): Future[Option[Index]] = {
    GformConnector.getByUserId(userId, formTypeId, version)
  }
}
