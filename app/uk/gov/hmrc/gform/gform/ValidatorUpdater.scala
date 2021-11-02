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

package uk.gov.hmrc.gform.gform

import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormCtx, HmrcRosmRegistrationCheckValidator, Validator }

class ValidatorUpdater(validator: Validator, index: Int, baseIds: List[FormComponentId]) {

  private def expandSmartString(smartString: SmartString) = smartString.expand(index, baseIds)

  private def expandFormCtx(formCtx: FormCtx): FormCtx = ExprUpdater.formCtx(formCtx, index, baseIds)

  def updated: Validator = validator match {
    case v: HmrcRosmRegistrationCheckValidator =>
      v.copy(
        errorMessage = expandSmartString(v.errorMessage),
        utr = expandFormCtx(v.utr),
        postcode = expandFormCtx(v.postcode)
      )
  }
}

object ValidatorUpdater {
  def apply(validator: Validator, index: Int, baseIds: List[FormComponentId]): Validator =
    new ValidatorUpdater(validator, index, baseIds).updated
}
