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

package uk.gov.hmrc.gform.connectors

import play.api.libs.json.{JsObject, JsValue}
import uk.gov.hmrc.gform.WSHttp
import uk.gov.hmrc.gform.models.form._
import uk.gov.hmrc.gform.models.{SaveResult, VerificationResult}
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._

import scala.concurrent.{ExecutionContext, Future}

trait GFormConnector {

  def httpGet : HttpGet

  def httpPost: HttpPost

  def httpPut: HttpPut

  def gFormUrl: String

  def retrieveFormTemplate(formTypeId: FormTypeId, version: Version)(implicit hc: HeaderCarrier, ec : ExecutionContext) : Future[Option[JsObject]] = {
    httpGet.GET[Option[JsObject]](gFormUrl + s"/formtemplates/${formTypeId.value}/${version.value}")
  }

  def saveForm(formDetails : JsValue, registrationNumber: String)(implicit hc : HeaderCarrier, ec : ExecutionContext) : Future[VerificationResult] = {
    httpPost.POST[JsValue, VerificationResult](gFormUrl + s"/saveForm/$registrationNumber", formDetails)
  }

  def retrieveForm(registrationNumber: String)(implicit hc: HeaderCarrier, ec : ExecutionContext) : Future[JsObject] = {
    httpPost.POSTString[JsObject](gFormUrl + s"/retrieveForm/$registrationNumber", registrationNumber)
  }

  def submit(registrationNumber: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    httpGet.GET[HttpResponse](gFormUrl+s"/submit/$registrationNumber")
  }

  def getById(formTypeId: FormTypeId, version: Version, formId: FormId)(implicit hc: HeaderCarrier): Future[FormData] = {
    httpGet.GET[FormData](gFormUrl + s"/forms/${formTypeId.value}/${version.value}/${formId.value}")
  }

  def save(formDetails: FormData, tolerant: Boolean)(implicit hc : HeaderCarrier) : Future[SaveResult] = {
    httpPost.POST[FormData, SaveResult](gFormUrl + s"/forms?tolerant=$tolerant", formDetails)
  }

  def update(formId: FormId, formData: FormData, tolerant: Boolean)(implicit hc : HeaderCarrier) : Future[SaveResult] = {
    httpPut.PUT[FormData, SaveResult](gFormUrl + s"/forms/${formId.value}?tolerant=$tolerant", formData)
  }

  def sendSubmission(formTypeId: FormTypeId, formId: FormId)(implicit hc : HeaderCarrier) : Future[HttpResponse] = {
    httpPost.POSTEmpty[HttpResponse](gFormUrl + s"/forms/${formTypeId.value}/submission/${formId.value}")
  }
}

object GFormConnector extends GFormConnector with ServicesConfig {

  lazy val httpGet = WSHttp
  lazy val httpPost = WSHttp
  lazy val httpPut = WSHttp

  def gFormUrl: String = s"${baseUrl("gform")}/gform"
}
