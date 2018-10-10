/*
 * Copyright 2018 HM Revenue & Customs
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

import play.api.libs.json.OFormat
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

// Not necessarily an identifier, the name is temporary, may have to be composed with AccessCodeId to make one
// TODO need a good name for this case class

case class UserFormTemplateId(value: String)

object UserFormTemplateId {
  def apply(userId: UserId, formTemplateId: FormTemplateId): UserFormTemplateId =
    UserFormTemplateId(s"${userId.value}-${formTemplateId.value}")
  implicit val format: OFormat[UserFormTemplateId] = ValueClassFormat.oformat("_id", UserFormTemplateId.apply, _.value)
}
