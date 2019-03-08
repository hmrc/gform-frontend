/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.MonadError
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.{ Data, Recalculation }
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future
import scala.util.{ Failure, Success }
case class EmailParameter(emailTemplateVariable: String, value: TextExpression)
object EmailParameter {
  implicit val format: OFormat[EmailParameter] = {
    val mongoFormat = Json.format[EmailParameter]

    val uploadTemplateReads: Reads[EmailParameter] =
      for {
        emailTemplateVariable <- (JsPath \ "emailTemplateVariable").read[String]
        value                 <- (JsPath \ "value").read[TextExpression]
      } yield EmailParameter(emailTemplateVariable, value)

    val reads: Reads[EmailParameter] = uploadTemplateReads | mongoFormat

    OFormat(reads, mongoFormat)
  }
}

case class EmailParameters(emailParameters: Map[String, String]) {
  def toFormFields: Seq[FormField] =
    emailParameters.map(parameter => FormField(FormComponentId(parameter._1), parameter._2)).toSeq
}

object EmailParameters {

  implicit val format: OFormat[EmailParameters] = Json.format

}
