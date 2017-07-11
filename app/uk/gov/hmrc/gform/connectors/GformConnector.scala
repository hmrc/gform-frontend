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

import play.api.libs.json.{ JsObject, JsValue }
import uk.gov.hmrc.gform.WSHttp
import uk.gov.hmrc.gform.gformbackend.model.{ FormData, FormId, FormTypeId, Version }
import uk.gov.hmrc.gform.models.{ SaveResult, VerificationResult }
import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.{ SaveResult, UserId }
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._

import scala.concurrent.{ ExecutionContext, Future }

trait GformConnector {

  def httpGet: HttpGet

  def httpPost: HttpPost

  def httpPut: HttpPut

  def httpDelete: HttpDelete

  def baseUrl: String

  def formTemplate(formTypeId: FormTypeId, version: Version)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[JsObject]] = {
    httpGet.GET[Option[JsObject]](s"$baseUrl/formtemplates/${formTypeId.value}/${version.value}")
  }

  def form(formTypeId: FormTypeId, version: Version, formId: FormId)(implicit hc: HeaderCarrier): Future[Form] = {
    httpGet.GET[Form](s"$baseUrl/forms/${formTypeId.value}/${version.value}/${formId.value}")
  }

  def getByUserId(userId: UserId, formTypeId: FormTypeId, version: Version)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Index]] = {
    httpGet.GET[Option[Index]](baseUrl + s"/forms/user/$userId/regime/$formTypeId/$version")
  }

  def updateForm(formId: FormId, formData: FormData, tolerant: Boolean)(implicit hc: HeaderCarrier): Future[SaveResult] = {
    httpPut.PUT[FormData, SaveResult](s"$baseUrl/forms/${formId.value}?tolerant=$tolerant", formData)
  }

  def sendSubmission(formTypeId: FormTypeId, formId: FormId)(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    httpPost.POSTEmpty[HttpResponse](s"$baseUrl/forms/${formTypeId.value}/submission/${formId.value}")
  }

  def getByIdCache(formTypeId: FormTypeId, version: Version, userId: UserId)(implicit hc: HeaderCarrier): Future[Form] = {
    httpGet.GET[Form](baseUrl + s"/forms/$formTypeId/$version/$userId/cache")
  }
  def sendSubmission(formTypeId: FormTypeId, userId: UserId, version: Version)(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    httpPost.POSTEmpty[HttpResponse](baseUrl + s"/forms/$formTypeId/submission/$userId/$version")
  }

  def deleteForm(formId: FormId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[SaveResult] = {
    httpDelete.DELETE[SaveResult](baseUrl + s"/forms/$formId/delete")
  }
}

object GformConnector extends GformConnector with ServicesConfig {

  lazy val httpGet = WSHttp
  lazy val httpPost = WSHttp
  lazy val httpPut = WSHttp
  lazy val httpDelete = WSHttp

  def baseUrl: String = s"${baseUrl("gform")}/gform"
}
