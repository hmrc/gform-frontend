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

package uk.gov.hmrc.bforms.models.userdetails

import julienrf.json.derived
import play.api.libs.json._
import play.api.libs.functional.syntax._

sealed trait AffinityGroup

case object Individual extends AffinityGroup
case object Organisation extends AffinityGroup
case object Agent extends AffinityGroup

object AffinityGroup {
  implicit val reads = Reads[AffinityGroup] {
    case JsString("Individual") => JsSuccess(Individual)
    case JsString("Organisation") => JsSuccess(Organisation)
    case JsString("Agent") => JsSuccess(Agent)
    case unknown => JsError(s"No AffinityGroup found in json: $unknown")
  }
}
