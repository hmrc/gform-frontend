/*
 * Copyright 2022 HM Revenue & Customs
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

import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ CsvColumnName, SimplifiedSelectionCriteria }

case class LookupOptions(options: Map[LookupLabel, LookupInfo]) extends AnyVal {

  def get(lookupLabel: LookupLabel): Option[LookupInfo] = options.get(lookupLabel)

  def contains(lookupLabel: LookupLabel): Boolean = options.contains(lookupLabel)

  def keys: Iterable[LookupLabel] = options.keys

  def sortLookupByIdx: List[LookupLabel] =
    options.toList
      .sortBy { case (_, lookupInfo) =>
        lookupInfo.index
      }
      .map(_._1)

  def sortLookupByPriorityAndLabel: List[LookupLabel] =
    options.toList
      .sortBy {
        case (label, DefaultLookupInfo(_, _))                          => (LookupPriority(1), label)
        case (label, CountryLookupInfo(_, _, _, priority, _, _, _, _)) => (priority, label)
        case (label, CurrencyLookupInfo(_, _, _, priority, _))         => (priority, label)
        case (label, PortLookupInfo(_, _, _, priority, _, _, _, _))    => (priority, label)
      }
      .map(_._1)
}

object LookupOptions {

  def getLookupValue(lookupInfo: LookupInfo, columnName: String): Option[String] =
    (lookupInfo, columnName) match {
      // format: off
      case (CountryLookupInfo(_, _, _, _, region, _, _, _), CsvColumnName.region)         => Some(region.region)
      case (CountryLookupInfo(id, _, _, _, _, _, _, _), CsvColumnName.countryCode)        => Some(id.id)
      case (CountryLookupInfo(_, _, _, _, _, inEU, _, _), CsvColumnName.inEU)             => Some(inEU.inEU)
      case (CountryLookupInfo(_, _, _, _, _, _, inEEA, _), CsvColumnName.inEEA)           => Some(inEEA.inEEA)
      case (CountryLookupInfo(_, _, _, _, _, _, _, inEFTA), CsvColumnName.inEFTA)         => Some(inEFTA.inEFTA)
      case (CurrencyLookupInfo(id, _, _, _, _), CsvColumnName.currencyCode)               => Some(id.id)
      case (CurrencyLookupInfo(_, _, _, _, countryCode), CsvColumnName.countryCode)       => Some(countryCode.countryCode)
      case (PortLookupInfo(id, _, _, _, _, _, _, _), CsvColumnName.portId)                => Some(id.id)
      case (PortLookupInfo(_, _, _, _, region, _, _, _), CsvColumnName.region)            => Some(region.region)
      case (PortLookupInfo(_, _, _, _, _, portType, _, _), CsvColumnName.portType)        => Some(portType.portType)
      case (PortLookupInfo(_, _, _, _, _, _, countryCode, _), CsvColumnName.countryCode)  => Some(countryCode.countryCode)
      case (PortLookupInfo(_, _, _, _, _, _, _, portCode), CsvColumnName.portCode)        => Some(portCode.portCode)
      case _                                                                              => None
      // format: on
    }

  def filterBySelectionCriteria(
    selectionCriteria: List[SimplifiedSelectionCriteria],
    lookupOptions: Map[LookupLabel, LookupInfo]
  ): Map[LookupLabel, LookupInfo] =
    if (lookupOptions.isEmpty) lookupOptions
    else
      selectionCriteria match {
        case Nil => lookupOptions
        case head :: tail =>
          val (column, values) = (head.column.column.toLowerCase, head.value)
          filterBySelectionCriteria(
            tail,
            lookupOptions.filter(r => values.contains(getLookupValue(r._2, column).getOrElse("")))
          )
      }

}
