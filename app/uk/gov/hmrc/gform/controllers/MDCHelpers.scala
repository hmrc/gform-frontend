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

package uk.gov.hmrc.gform.controllers

import cats.Applicative
import cats.syntax.applicative._
import org.slf4j.MDC
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

object MDCHelpers {
  def addFormIdToMdc[F[_]: Applicative](formId: FormId): F[Unit] =
    MDC.put("FormId", formId.value).pure[F]

  def addFormTemplateIdToMdc[F[_]: Applicative](formTemplateId: FormTemplateId): F[Unit] =
    MDC.put("FormTemplateId", formTemplateId.value).pure[F]

  def addAccessCodeToMdc[F[_]: Applicative](accessCode: Option[AccessCode]): F[Unit] =
    accessCode
      .foreach { ac =>
        MDC.put("AccessCode", ac.value)
      }
      .pure[F]
}
