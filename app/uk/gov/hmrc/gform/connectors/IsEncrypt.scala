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
import uk.gov.hmrc.gform.gformbackend.model.{ FormData, FormId, FormTypeId }
import uk.gov.hmrc.gform.models.{ UserId => _, _ }
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._

import scala.concurrent.{ ExecutionContext, Future }
object IsEncrypt {
  case class Encrypt(value: Boolean)
  lazy val is: Boolean = pureconfig.loadConfigOrThrow[Encrypt]("feature.encrypt").value
}
//trait GformConnector {
//
//  def httpGet: HttpGet
//
//  def httpPost: HttpPost
//
//  def httpPut: HttpPut
//
//  def httpDelete: HttpDelete
//
//  def gformUrl: String
//
//  def retrieveFormTemplate(formTypeId: FormTypeId, version: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[JsObject]] = {
//    httpGet.GET[Option[JsObject]](gformUrl + s"/formtemplates/$formTypeId/$version")
//  }
//
//  def saveForm(formDetails: JsValue, registrationNumber: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[VerificationResult] = {
//    httpPost.POST[JsValue, VerificationResult](gformUrl + s"/saveForm/$registrationNumber", formDetails)
//  }
//
//  def retrieveForm(registrationNumber: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[JsObject] = {
//    httpPost.POSTString[JsObject](gformUrl + s"/retrieveForm/$registrationNumber", registrationNumber)
//  }
//
//  def submit(registrationNumber: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
//    httpGet.GET[HttpResponse](gformUrl + s"/submit/$registrationNumber")
//  }
//
//  def getByUserId(userId: UserId, formTypeId: FormTypeId, version: String)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[FormId]] = {
//    httpGet.GET[Option[FormId]](gformUrl + s"/forms/user/$userId/regime/$formTypeId/$version")
//  }
//
//  def getById(formTypeId: FormTypeId, version: String, formId: FormId)(implicit hc: HeaderCarrier): Future[Form] = {
//    httpGet.GET[Form](gformUrl + s"/forms/$formTypeId/$version/$formId")
//  }
//
//  def getByIdCache(formTypeId: FormTypeId, version: String, userId: UserId)(implicit hc: HeaderCarrier): Future[Form] = {
//    httpGet.GET[Form](gformUrl + s"/forms/$formTypeId/$version/$userId/cache")
//  }
//
//  def save(formDetails: FormData, tolerant: Boolean)(implicit hc: HeaderCarrier): Future[SaveResult] = {
//    httpPost.POST[FormData, SaveResult](gformUrl + s"/forms?tolerant=$tolerant", formDetails)
//  }
//
//  def update(formId: FormId, formData: FormData, tolerant: Boolean)(implicit hc: HeaderCarrier): Future[SaveResult] = {
//    httpPut.PUT[FormData, SaveResult](gformUrl + s"/forms/$formId?tolerant=$tolerant", formData)
//  }
//
//  def sendSubmission(formTypeId: FormTypeId, formId: FormId)(implicit hc: HeaderCarrier): Future[HttpResponse] = {
//    httpPost.POSTEmpty[HttpResponse](gformUrl + s"/forms/$formTypeId/submission/$formId")
//  }
//
//}
//
//object GformConnector extends GformConnector with ServicesConfig {
//
//  lazy val httpGet = WSHttp
//  lazy val httpPost = WSHttp
//  lazy val httpPut = WSHttp
//  lazy val httpDelete = WSHttp
//
//
//  def gformUrl: String = s"${baseUrl("gform")}/gform"
//
//}
