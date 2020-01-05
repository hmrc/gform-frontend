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

import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SubmissionRef, UserId, ValueClassFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

sealed trait FormIdData {
  def toFormId: FormId = this match {
    case FormIdData.Plain(userId, formTemplateId) => FormId.direct(userId, formTemplateId)
    case FormIdData.WithAccessCode(userId, formTemplateId, accessCode) =>
      FormId.withAccessCode(userId, formTemplateId, accessCode)
  }

  def maybeAccessCode: Option[AccessCode] = this match {
    case FormIdData.Plain(_, _)                      => None
    case FormIdData.WithAccessCode(_, _, accessCode) => Some(accessCode)
  }
}

object FormIdData {

  case class Plain(userId: UserId, formTemplateId: FormTemplateId) extends FormIdData
  case class WithAccessCode(userId: UserId, formTemplateId: FormTemplateId, accessCode: AccessCode) extends FormIdData

  def fromForm(form: Form, maybeAccessCode: Option[AccessCode]): FormIdData = {
    val userId = form.userId
    val formTemplateId = form.formTemplateId
    maybeAccessCode.fold[FormIdData](FormIdData.Plain(userId, formTemplateId)) { accessCode =>
      FormIdData.WithAccessCode(userId, formTemplateId, accessCode)
    }
  }

  def apply(cache: AuthCacheWithForm, maybeAccessCode: Option[AccessCode]): FormIdData =
    apply(cache.retrievals, cache.formTemplate, maybeAccessCode)

  def apply(
    retrievals: MaterialisedRetrievals,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]): FormIdData = apply(UserId(retrievals), formTemplateId, maybeAccessCode)

  def apply(
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    maybeAccessCode: Option[AccessCode]): FormIdData = apply(UserId(retrievals), formTemplate._id, maybeAccessCode)

  def apply(userId: UserId, formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): FormIdData =
    maybeAccessCode.fold[FormIdData](FormIdData.Plain(userId, formTemplateId)) { accessCode =>
      FormIdData.WithAccessCode(userId, formTemplateId, accessCode)
    }

  implicit val format: OFormat[FormIdData] = derived.oformat
}
