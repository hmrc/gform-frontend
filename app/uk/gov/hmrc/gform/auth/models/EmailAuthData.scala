/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.auth.models

import julienrf.json.derived
import org.typelevel.ci.CIString
import play.api.libs.json.OFormat
import uk.gov.hmrc.gform.models.EmailId
import uk.gov.hmrc.gform.sharedmodel.form.EmailAndCode

sealed trait EmailAuthData {
  def email: CIString = this match {
    case InvalidEmail(EmailId(email), _)       => email
    case ValidEmail(EmailAndCode(email, _), _) => email
  }
}

case class InvalidEmail(emailId: EmailId, message: String) extends EmailAuthData
case class ValidEmail(emailAndCode: EmailAndCode, confirmed: Boolean = false) extends EmailAuthData

object EmailAuthData {
  implicit val format: OFormat[EmailAuthData] = derived.oformat()
}
