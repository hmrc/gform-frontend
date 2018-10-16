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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.json._
import uk.gov.hmrc.gform.auth.models.UserDetails
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

case class FormId(value: String)

object FormId {

  def apply(userDetails: UserDetails, formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): FormId = {
    val userId = userDetails.groupIdentifier
    val ac = maybeAccessCode.map("-" + _.value).getOrElse("")
    new FormId(s"$userId-${formTemplateId.value}$ac")
  }

  implicit val format: OFormat[FormId] = ValueClassFormat.oformat("_id", FormId.apply, _.value)

}
