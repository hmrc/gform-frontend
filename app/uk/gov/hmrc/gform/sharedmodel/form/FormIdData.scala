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

package uk.gov.hmrc.gform.sharedmodel.form

import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, UserId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

sealed trait FormIdData {

  val formTemplateId: FormTemplateId

  def fold[A](f: FormIdData.Plain => A)(g: FormIdData.WithAccessCode => A): A = this match {
    case p: FormIdData.Plain          => f(p)
    case w: FormIdData.WithAccessCode => g(w)
  }

  def toFormId: FormId = fold { p =>
    FormId.direct(p.userId, p.formTemplateId)
  } { w =>
    FormId.withAccessCode(w.userId, w.formTemplateId, w.accessCode)
  }

  def maybeAccessCode: Option[AccessCode] =
    fold(_ => Option.empty[AccessCode])(withAccessCode => Some(withAccessCode.accessCode))

  def withOriginalTemplateId(formTemplate: FormTemplate): FormIdData =
    fold[FormIdData](_.copy(formTemplateId = formTemplate.originalId)) {
      _.copy(formTemplateId = formTemplate.originalId)
    }

  def withTemplateId(formTemplateId: FormTemplateId): FormIdData =
    fold[FormIdData](_.copy(formTemplateId = formTemplateId)) {
      _.copy(formTemplateId = formTemplateId)
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
    maybeAccessCode: Option[AccessCode]
  ): FormIdData = apply(UserId(retrievals), formTemplateId, maybeAccessCode)

  def apply(
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    maybeAccessCode: Option[AccessCode]
  ): FormIdData = apply(UserId(retrievals), formTemplate._id, maybeAccessCode)

  def apply(userId: UserId, formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): FormIdData =
    maybeAccessCode.fold[FormIdData](FormIdData.Plain(userId, formTemplateId)) { accessCode =>
      FormIdData.WithAccessCode(userId, formTemplateId, accessCode)
    }

  implicit val format: OFormat[FormIdData] = derived.oformat()
}
