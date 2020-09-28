/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel

import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.gform.sharedmodel.email.EmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierTemplateId

sealed trait EmailVerifierService extends Product with Serializable

object EmailVerifierService {

  def notify(emailTemplateId: NotifierTemplateId) = Notify(emailTemplateId)
  def digitalContact(emailTemplateId: EmailTemplateId) = DigitalContact(emailTemplateId)

  case class Notify(emailTemplateId: NotifierTemplateId) extends EmailVerifierService
  case class DigitalContact(emailTemplateId: EmailTemplateId) extends EmailVerifierService

  implicit val format: OFormat[EmailVerifierService] = derived.oformat()

}
