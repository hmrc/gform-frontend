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

import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, JsonUtils }
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.form.EmailAndCode

case class EmailAuthDetails(mappings: Map[FormTemplateId, EmailCodeConfirmation] = Map.empty) {
  def +(values: (FormTemplateId, EmailCodeConfirmation)*): EmailAuthDetails =
    EmailAuthDetails(mappings ++ values)

  def get(formTemplateId: FormTemplateId): Option[EmailCodeConfirmation] =
    mappings.get(formTemplateId)

  def checkCodeAndConfirm(formTemplateId: FormTemplateId, emailAndCode: EmailAndCode): Option[EmailAuthDetails] =
    get(formTemplateId).flatMap { emailCodeConfirmation =>
      if (emailCodeConfirmation.emailAndCode == emailAndCode) {
        Some(EmailAuthDetails(mappings + (formTemplateId -> emailCodeConfirmation.copy(confirmed = true))))
      } else {
        None
      }
    }

  def isConfirmed(formTemplateId: FormTemplateId) = get(formTemplateId).exists(_.confirmed)
}

object EmailAuthDetails {

  val formatMap: Format[Map[FormTemplateId, EmailCodeConfirmation]] =
    JsonUtils.formatMap(FormTemplateId.apply, _.value)

  implicit val format: Format[EmailAuthDetails] = Format(
    formatMap.map(EmailAuthDetails.apply),
    formatMap.contramap(_.mappings)
  )
}
