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

package uk.gov.hmrc.gform.gform

import play.api.libs.json.{ Json, Reads }
import play.api.mvc.{ AnyContent, Request }
import uk.gov.hmrc.gform.auth.models.{ EmailAuthDetails, ValidEmail }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

object EmailAuthUtils {

  val EMAIL_AUTH_DETAILS_SESSION_KEY = "emailAuthDetails"

  def isEmailConfirmed(formTemplateId: FormTemplateId)(implicit request: Request[AnyContent]): Option[String] = {
    val emailAuthDetails: EmailAuthDetails = fromSession(request, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails())
    emailAuthDetails.get(formTemplateId) match {
      case Some(ValidEmail(emailAndCode, confirmed)) if confirmed => Some(emailAndCode.email)
      case _                                                      => None
    }
  }

  def fromSession[T: Reads](request: Request[AnyContent], key: String, default: T): T =
    request.session
      .get(key)
      .map(json => Json.fromJson[T](Json.parse(json)).get)
      .getOrElse(default)
}
