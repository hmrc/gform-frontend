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

package uk.gov.hmrc.gform.models

import shapeless.tag
import shapeless.tag.@@
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

package email {
  trait EmailFieldIdTag
  trait VerificationCodeFieldIdTag
}

package object email {

  // Since AnyVal cannot wrap AnyVal we use tagged AnyVal
  type EmailFieldId = FormComponentId @@ EmailFieldIdTag
  type VerificationCodeFieldId = FormComponentId @@ VerificationCodeFieldIdTag

  def emailFieldId(atomicFcId: FormComponentId) = tag[EmailFieldIdTag][FormComponentId](atomicFcId)
  def verificationCodeFieldId(atomicFcId: FormComponentId) =
    tag[VerificationCodeFieldIdTag][FormComponentId](atomicFcId)
}
