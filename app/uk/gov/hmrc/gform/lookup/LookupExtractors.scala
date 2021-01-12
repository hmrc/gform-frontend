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

package uk.gov.hmrc.gform.lookup

import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, Lookup, Register, Text }

class LookupExtractors(lookup: Map[Register, LookupType]) {

  private object IsRadioRegister {
    def unapply(register: Register): Boolean =
      lookup.get(register) match {
        case Some(al @ RadioLookup(_)) => true
        case _                         => false
      }
  }

  object IsRadioLookup {
    def unapply(fc: FormComponent): Option[Text] =
      fc.`type` match {
        case t @ Text(Lookup(IsRadioRegister(), _), _, _, _, _, _) => Some(t)
        case _                                                     => None
      }
  }
}
