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

package uk.gov.hmrc.gform.lookup

import com.miguelfonseca.completely.AutocompleteEngine

sealed trait LookupType extends Product with Serializable

case class RadioLookup(options: LookupOptions) extends LookupType
case class AjaxLookup(
  options: LookupOptions,
  autocomplete: AutocompleteEngine[LookupRecord],
  showAll: ShowAll
) extends LookupType

sealed trait ShowAll extends Product with Serializable {
  val enabled = this == ShowAll.Enabled
}

object ShowAll {
  case object Enabled extends ShowAll
  case object Disabled extends ShowAll
}
