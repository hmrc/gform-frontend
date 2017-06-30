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

package uk.gov.hmrc.gform.gformbackend

import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.SaveResult
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }

class GformConnector(ws: WSHttp, baseUrl: String) {

  def formTemplate(formTypeId: FormTypeId, version: Version)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[FormTemplate]] = {
    ws.GET[Option[FormTemplate]](s"$baseUrl/formtemplates/${formTypeId.value}/${version.value}")
  }

  def form(formTypeId: FormTypeId, version: Version, formId: FormId)(implicit hc: HeaderCarrier): Future[FormData] = {
    ws.GET[FormData](s"$baseUrl/forms/${formTypeId.value}/${version.value}/${formId.value}")
  }

  def saveForm(formDetails: FormData, tolerant: Boolean)(implicit hc: HeaderCarrier): Future[SaveResult] = {
    ws.POST[FormData, SaveResult](s"$baseUrl/forms?tolerant=$tolerant", formDetails)
  }

  def updateForm(formId: FormId, formData: FormData, tolerant: Boolean)(implicit hc: HeaderCarrier): Future[SaveResult] = {
    ws.PUT[FormData, SaveResult](s"$baseUrl/forms/${formId.value}?tolerant=$tolerant", formData)
  }

  def sendSubmission(formTypeId: FormTypeId, formId: FormId)(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    ws.POSTEmpty[HttpResponse](s"$baseUrl/forms/${formTypeId.value}/submission/${formId.value}")
  }
}
