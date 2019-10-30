/*
 * Copyright 2019 HM Revenue & Customs
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

import cats.Monoid
import uk.gov.hmrc.gform.models.email.EmailFieldId
import uk.gov.hmrc.gform.sharedmodel.des.DesRegistrationResponse
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

case class ValidationResult(
  desRegistrationResponse: Option[DesRegistrationResponse],
  emailVerification: Map[EmailFieldId, EmailAndCode]
)

object ValidationResult {

  implicit val monoidIns: Monoid[ValidationResult] = new Monoid[ValidationResult] {
    def empty = ValidationResult.empty
    def combine(l: ValidationResult, r: ValidationResult): ValidationResult = (l, r) match {
      case (ValidationResult(Some(drr), m1), ValidationResult(_, m2))    => ValidationResult(Some(drr), m1 ++ m2)
      case (ValidationResult(None, m1), ValidationResult(Some(drr), m2)) => ValidationResult(Some(drr), m1 ++ m2)
      case (ValidationResult(None, m1), ValidationResult(None, m2))      => ValidationResult(None, m1 ++ m2)
    }
  }

  val empty = ValidationResult(None, Map.empty)
}
