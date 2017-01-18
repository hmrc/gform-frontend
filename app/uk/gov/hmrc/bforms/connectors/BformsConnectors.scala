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

package uk.gov.hmrc.bforms.connectors

import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.mvc.Action
import uk.gov.hmrc.bforms.WSHttp
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpPost, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

case class VerificationResult(error: Option[String])

object VerificationResult {
  implicit val formats = Json.format[VerificationResult]
}

trait BformsConnector {

  def httpPost: HttpPost

  def bformsUrl: String

  def saveForm(formDetails : JsValue)(implicit hc : HeaderCarrier, ec : ExecutionContext) : Future[VerificationResult] = {
    httpPost.POST[JsValue, VerificationResult](bformsUrl + "/saveForm", formDetails)
  }

  def retrieveForm(registrationNumber: String)(implicit hc: HeaderCarrier, ec : ExecutionContext) : Future[JsObject] = {
    httpPost.POSTString[JsObject](bformsUrl + s"/retrieveForm/$registrationNumber", registrationNumber)
  }
}

object BformsConnector extends BformsConnector with ServicesConfig {

  lazy val httpPost = WSHttp

  def bformsUrl: String = s"http://localhost:9090/bforms"
}
