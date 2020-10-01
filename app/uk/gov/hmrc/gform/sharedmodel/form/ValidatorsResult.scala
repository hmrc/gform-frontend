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

import cats.Monoid
import uk.gov.hmrc.gform.models.email.EmailFieldId
import uk.gov.hmrc.gform.sharedmodel.des.DesRegistrationResponse
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

case class ValidatorsResult(
  desRegistrationResponse: Option[DesRegistrationResponse],
  emailVerification: Map[EmailFieldId, EmailAndCode]
)

object ValidatorsResult {

  implicit val monoidIns: Monoid[ValidatorsResult] = new Monoid[ValidatorsResult] {
    def empty = ValidatorsResult.empty
    def combine(l: ValidatorsResult, r: ValidatorsResult): ValidatorsResult = (l, r) match {
      case (ValidatorsResult(Some(drr), m1), ValidatorsResult(_, m2))    => ValidatorsResult(Some(drr), m1 ++ m2)
      case (ValidatorsResult(None, m1), ValidatorsResult(Some(drr), m2)) => ValidatorsResult(Some(drr), m1 ++ m2)
      case (ValidatorsResult(None, m1), ValidatorsResult(None, m2))      => ValidatorsResult(None, m1 ++ m2)
    }
  }

  val empty = ValidatorsResult(None, Map.empty)
}
