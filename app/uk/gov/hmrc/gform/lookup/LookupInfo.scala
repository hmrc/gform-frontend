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

package uk.gov.hmrc.gform.lookup

sealed trait LookupInfo {
  def id: LookupId
  def index: Int
}

final case class DefaultLookupInfo(
  id: LookupId,
  index: Int
) extends LookupInfo

final case class CountryLookupInfo(
  id: LookupId,
  index: Int,
  keywords: LookupKeywords,
  priority: LookupPriority,
  priorityUk: LookupPriority,
  region: LookupRegion,
  inGibraltarEuEeaEfta: LookupInGibraltarEuEeaEfta,
  columns: Map[String, String]
) extends LookupInfo

final case class NationalityLookupInfo(
  id: LookupId,
  index: Int,
  keywords: LookupKeywords,
  priority: LookupPriority,
  priorityUk: LookupPriority,
  region: LookupRegion,
  countryCode: LookupCountryCode,
  columns: Map[String, String]
) extends LookupInfo

final case class CurrencyLookupInfo(
  id: LookupId,
  index: Int,
  keywords: LookupKeywords,
  priority: LookupPriority,
  countryCode: LookupCountryCode
) extends LookupInfo

final case class PortLookupInfo(
  id: LookupId,
  index: Int,
  keywords: LookupKeywords,
  priority: LookupPriority,
  region: LookupRegion,
  portType: LookupPortType,
  countryCode: LookupCountryCode,
  portCode: LookupPortCode
) extends LookupInfo

final case class SicCodeLookupInfo(
  id: LookupId,
  index: Int,
  section: LookupSicCodeSection,
  columns: Map[String, String]
) extends LookupInfo

final case class AgentComplaintCategoriesLookupInfo(
  id: LookupId,
  index: Int,
  keywords: LookupKeywords,
  columns: Map[String, String]
) extends LookupInfo

final case class FiveColumnLookupInfo(
  id: LookupId,
  index: Int,
  keywords: LookupKeywords,
  priority: LookupPriority,
  columns: Map[String, String]
) extends LookupInfo
