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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.json._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, UserId, ValueClassFormat }

case class FormId(value: String) extends AnyVal

object FormId {

  def apply(cache: AuthCacheWithForm, maybeAccessCode: Option[AccessCode]): FormId =
    apply(cache.retrievals, cache.formTemplate, maybeAccessCode)

  def apply(
    retrievals: MaterialisedRetrievals,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]): FormId = apply(UserId(retrievals), formTemplateId, maybeAccessCode)

  def apply(
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    maybeAccessCode: Option[AccessCode]): FormId = apply(UserId(retrievals), formTemplate._id, maybeAccessCode)

  def apply(userId: UserId, formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): FormId =
    maybeAccessCode.fold(direct(userId, formTemplateId)) { accessCode =>
      withAccessCode(userId, formTemplateId, accessCode)
    }

  def direct(userId: UserId, formTemplateId: FormTemplateId): FormId =
    new FormId(s"${userId.value}-${formTemplateId.value}")

  def withAccessCode(userId: UserId, formTemplateId: FormTemplateId, accessCode: AccessCode): FormId =
    new FormId(s"${userId.value}-${formTemplateId.value}-${accessCode.value}")

  implicit val format: OFormat[FormId] = ValueClassFormat.oformat("_id", FormId.apply, _.value)

}
