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

package uk.gov.hmrc.gform.lookup

case class LookupOptions(options: Map[LookupLabel, LookupInfo]) extends AnyVal {

  def get(lookupLabel: LookupLabel): Option[LookupInfo] = options.get(lookupLabel)

  def contains(lookupLabel: LookupLabel): Boolean = options.contains(lookupLabel)

  def keys: Iterable[LookupLabel] = options.keys

  def sortLookupByIdx: List[(LookupLabel, LookupInfo)] =
    options.toList.sortBy {
      case (_, DefaultLookupInfo(_, idx))           => idx
      case (_, CountryLookupInfo(_, idx, _, _, _))  => idx
      case (_, CurrencyLookupInfo(_, idx, _, _, _)) => idx
      case (_, PortLookupInfo(_, idx, _, _, _, _))  => idx
    }

  def sortLookupByPriorityAndLabel: List[(LookupLabel, LookupInfo)] =
    options.toList.sortBy {
      case (label, DefaultLookupInfo(_, _))                  => (LookupPriority(1), label)
      case (label, CountryLookupInfo(_, _, _, priority, _))  => (priority, label)
      case (label, CurrencyLookupInfo(_, _, _, priority, _)) => (priority, label)
      case (label, PortLookupInfo(_, _, _, priority, _, _))  => (priority, label)
    }
}
