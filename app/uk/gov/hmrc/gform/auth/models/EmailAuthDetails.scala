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

package uk.gov.hmrc.gform.auth.models

import cats.Eq
import cats.syntax.eq._
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.form.EmailAndCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateContext, FormTemplateId, JsonUtils }

case class EmailAuthDetails(mappings: Map[FormTemplateId, EmailAuthData] = Map.empty) {

  implicit val equal: Eq[EmailAndCode] = Eq.fromUniversalEquals

  def +(values: (FormTemplateId, EmailAuthData)*): EmailAuthDetails =
    EmailAuthDetails(mappings ++ values)

  def -(key: FormTemplateId): EmailAuthDetails =
    EmailAuthDetails(mappings - key)

  def get(formTemplateWithRedirects: FormTemplateContext): Option[EmailAuthData] =
    mappings
      .get(formTemplateWithRedirects.formTemplate._id)
      .orElse(formTemplateWithRedirects.redirect.flatMap(mappings.get))

  def checkCodeAndConfirm(
    formTemplateId: FormTemplateId,
    formTemplate: FormTemplate,
    emailAndCode: EmailAndCode
  ): Option[EmailAuthDetails] =
    get(FormTemplateContext.basicContext(formTemplate, None)).flatMap(_.fold[Option[EmailAuthDetails]](_ => None) { v =>
      if (v.emailAndCode === emailAndCode)
        Some(
          EmailAuthDetails(
            mappings + (formTemplateId -> v.copy(confirmed = true))
              ++ formTemplate.legacyFormIds.fold(List.empty[(FormTemplateId, ValidEmail)])(
                _.map(_ -> v.copy(confirmed = true)).toList
              )
          )
        )
      else
        None
    })
}

object EmailAuthDetails {

  val empty = EmailAuthDetails()

  val formatMap: Format[Map[FormTemplateId, EmailAuthData]] =
    JsonUtils.formatMap(FormTemplateId.apply, _.value)

  implicit val format: Format[EmailAuthDetails] = Format(
    formatMap.map(EmailAuthDetails.apply),
    formatMap.contramap(_.mappings)
  )
}
