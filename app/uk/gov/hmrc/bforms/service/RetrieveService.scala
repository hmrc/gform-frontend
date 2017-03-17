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

package uk.gov.hmrc.bforms.service

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale

import uk.gov.hmrc.bforms.connectors.BformsConnector
import uk.gov.hmrc.bforms.models.{ EnvironmentalBody, FormTypeId, FormField }
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future
import play.api.libs.json._
import uk.gov.hmrc.bforms.models.{FieldValue, FormTemplate}

object RetrieveService {

  def bformsConnector = BformsConnector

  def getFields(formTemplate: FormTemplate): List[FieldValue] = {
    formTemplate.sections.flatMap(_.fields)
  }

  def formTemplateFromJson(formTemplate: JsObject): Either[String, FormTemplate] = {
    formTemplate.validate[FormTemplate] match {
      case JsSuccess(formTemplate, _) => Right(formTemplate)
      case JsError(error) => Left(error.toString)
    }
  }

  def getFormTemplate(formTypeId: FormTypeId, version: String)(implicit hc : HeaderCarrier): Future[Either[String, FormTemplate]] = {
    val templateF = bformsConnector.retrieveFormTemplate(formTypeId, version)

    for {
      template <- templateF
    } yield {
      template match {
        case Some(jsonTemplate) => formTemplateFromJson(jsonTemplate)
        case None => Left(s"No template for formTypeId $formTypeId version $version")
      }
    }
  }

}
