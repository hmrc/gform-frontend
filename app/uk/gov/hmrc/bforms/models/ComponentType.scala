/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.models

import julienrf.json.derived
import play.api.libs.json.OFormat

/**
  * Created by dimitra on 21/03/17.
  */
sealed trait ComponentType

case object Text extends ComponentType

case object Date extends ComponentType {
  val fields = (id: FieldId) => List("day", "month", "year").map(id.withSuffix)
}

case object Address extends ComponentType {
  val fields = (id: FieldId) => List("street1", "street2", "street3", "town", "county", "postcode").map(id.withSuffix)
}

object ComponentType {
  implicit val format: OFormat[ComponentType] = derived.oformat
}