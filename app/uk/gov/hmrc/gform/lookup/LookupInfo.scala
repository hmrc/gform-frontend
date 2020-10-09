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

sealed trait LookupInfo {
  def id: LookupId
  def index: Int
}

sealed trait AutoCompleteInfo {
  val keywords: LookupKeyWords = LookupKeyWords.EMPTY
  val priority: LookupPriority = LookupPriority(1)
}
final case class LookupInfoDefault(
  id: LookupId,
  englishLabel: LookupLabel,
  welshLabel: LookupLabel)
    extends AutoCompleteInfo

final case class LookupInfoCountry(
  id: LookupId,
  englishLabel: LookupLabel,
  welshLabel: LookupLabel,
  override val keywords: LookupKeyWords,
  override val priority: LookupPriority,
  region: LookupRegion)
    extends AutoCompleteInfo

final case class DefaultLookupInfo(
  id: LookupId,
  index: Int
) extends LookupInfo

final case class CountryLookupInfo(
  id: LookupId,
  index: Int,
  keyWords: LookupKeyWords,
  priority: LookupPriority,
  region: LookupRegion
) extends LookupInfo

final case class CurrencyLookupInfo(
  id: LookupId,
  index: Int,
  keyWords: LookupKeyWords,
  priority: LookupPriority,
  currencyCountryCode: LookupCurrencyCountryCode
) extends LookupInfo

final case class PortLookupInfo(
  id: LookupId,
  index: Int,
  keyWords: LookupKeyWords,
  priority: LookupPriority,
  region: LookupRegion,
  portType: LookupPortType
) extends LookupInfo
