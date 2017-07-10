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

import uk.gov.hmrc.gform.connectors.GformConnector
import uk.gov.hmrc.gform.gformbackend.model.{ FormData, FormId, FormTypeId, Version }
import uk.gov.hmrc.gform.models.SaveResult
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.Future

object SaveService {

  def gformConnector: GformConnector = GformConnector

  def getFormById(formTypeId: FormTypeId, version: Version, formId: FormId)(implicit hc: HeaderCarrier) = {
    gformConnector.form(formTypeId, version, formId)
  }

  def updateFormData(formId: FormId, formData: FormData, tolerant: Boolean)(implicit hc: HeaderCarrier): Future[SaveResult] = {
    gformConnector.updateForm(formId, formData, tolerant)
  }

  def sendSubmission(formTypeId: FormTypeId, formId: FormId)(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    gformConnector.sendSubmission(formTypeId, formId)
  }
}
