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
import uk.gov.hmrc.bforms.models.VerificationResult
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpGet, HttpPost, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

trait BformsConnector {

  def httpGet : HttpGet

  def httpPost: HttpPost

  def bformsUrl: String

  def saveForm(formDetails : JsValue, registrationNumber: String)(implicit hc : HeaderCarrier, ec : ExecutionContext) : Future[VerificationResult] = {
    httpPost.POST[JsValue, VerificationResult](bformsUrl + s"/saveForm/$registrationNumber", formDetails)
  }

  def retrieveForm(registrationNumber: String)(implicit hc: HeaderCarrier, ec : ExecutionContext) : Future[JsObject] = {
    httpPost.POSTString[JsObject](bformsUrl + s"/retrieveForm/$registrationNumber", registrationNumber)
  }

  def submit(registrationNumber :String)(implicit hc: HeaderCarrier, ec : ExecutionContext) : Future[HttpResponse] ={
    httpGet.GET(bformsUrl+s"/submit/$registrationNumber")
  }
}

object BformsConnector extends BformsConnector with ServicesConfig {

  lazy val httpGet = WSHttp
  lazy val httpPost = WSHttp

  def bformsUrl: String = s"${baseUrl("bforms")}/bforms"

}
