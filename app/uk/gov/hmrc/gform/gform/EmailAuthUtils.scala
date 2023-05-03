/*
 * Copyright 2023 HM Revenue & Customs
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

import org.typelevel.ci.CIString
import play.api.mvc.RequestHeader
import uk.gov.hmrc.gform.auth.models.EmailAuthDetails
import uk.gov.hmrc.gform.controllers.GformSessionKeys.EMAIL_AUTH_DETAILS_SESSION_KEY
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateContext, FormTemplateId, JsonUtils }

object EmailAuthUtils {

  def isEmailConfirmed(
    formTemplateContext: FormTemplateContext
  )(implicit rh: RequestHeader): Option[CIString] = {
    val emailAuthDetails: EmailAuthDetails =
      jsonFromSession(rh, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails.empty)
    emailAuthDetails
      .get(formTemplateContext)
      .fold[Option[CIString]](None)(_.confirmedEmail)
  }

  def removeFormTemplateFromAuthSession(formTemplateId: FormTemplateId)(implicit rh: RequestHeader) = {
    val emailAuthDetails: EmailAuthDetails =
      jsonFromSession(rh, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails.empty)
    (EMAIL_AUTH_DETAILS_SESSION_KEY, JsonUtils.toJsonStr(emailAuthDetails - formTemplateId))
  }
}
