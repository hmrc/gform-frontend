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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import cats.Eq
import play.api.libs.json.Format
import uk.gov.hmrc.gform.sharedmodel.formtemplate.ADTFormat

sealed trait SdesDestination extends Product with Serializable {
  def downloadPath: String = this match {
    case SdesDestination.HmrcIlluminate                              => "hmrc-illuminate"
    case SdesDestination.DataStore | SdesDestination.DataStoreLegacy => "data-store"
    case SdesDestination.Dms                                         => "dms"
    case SdesDestination.InfoArchive                                 => "info-archive"
  }

  def description: String = this match {
    case SdesDestination.HmrcIlluminate  => "HMRC Illuminate"
    case SdesDestination.DataStore       => "Data Store"
    case SdesDestination.DataStoreLegacy => "Data Store (Legacy)"
    case SdesDestination.Dms             => "DMS"
    case SdesDestination.InfoArchive     => "InfoArchive"
  }
}

object SdesDestination {
  case object Dms extends SdesDestination

  case object HmrcIlluminate extends SdesDestination

  case object DataStoreLegacy extends SdesDestination // Alias for HmrcIlluminate (deprecated)

  case object DataStore extends SdesDestination

  case object InfoArchive extends SdesDestination

  implicit val equal: Eq[SdesDestination] = Eq.fromUniversalEquals

  implicit val format: Format[SdesDestination] =
    ADTFormat.formatEnumeration(
      "Dms"             -> Dms,
      "HmrcIlluminate"  -> HmrcIlluminate,
      "DataStoreLegacy" -> DataStoreLegacy,
      "DataStore"       -> DataStore,
      "InfoArchive"     -> InfoArchive
    )
}
