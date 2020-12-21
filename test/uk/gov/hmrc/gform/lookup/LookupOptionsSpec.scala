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

import org.scalatest.{ FlatSpecLike, Matchers }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ CsvColumnName, SimplifiedSelectionCriteria }

class LookupOptionsSpec extends FlatSpecLike with Matchers {

  val emptyLookupOptions = Map.empty[LookupLabel, LookupInfo]

  val lookupOptions = Map(LookupLabel("aaaa") -> DefaultLookupInfo(LookupId("1"), 0))
  val countryLookupOptions = Map(
    LookupLabel("Great Britain") -> CountryLookupInfo(
      LookupId("GB"),
      0,
      LookupKeywords(Some("Britain")),
      LookupPriority(1),
      LookupRegion("1")),
    LookupLabel("France") -> CountryLookupInfo(
      LookupId("FR"),
      1,
      LookupKeywords(Some("France")),
      LookupPriority(1),
      LookupRegion("2"))
  )

  val portLookupOptions = Map(
    LookupLabel("London Heathrow Airport") -> PortLookupInfo(
      LookupId("1"),
      0,
      LookupKeywords(Some("London Heathrow")),
      LookupPriority(1),
      LookupRegion("1"),
      LookupPortType("T2"),
      LookupCountryCode("GB"),
      LookupPortCode("LHR")
    ),
    LookupLabel("London Gatwick Airport") -> PortLookupInfo(
      LookupId("2"),
      1,
      LookupKeywords(Some("London Gatwick")),
      LookupPriority(2),
      LookupRegion("1"),
      LookupPortType("T2"),
      LookupCountryCode("GB"),
      LookupPortCode("LGW")
    ),
    LookupLabel("Charles de Gaulle Airport") -> PortLookupInfo(
      LookupId("3"),
      2,
      LookupKeywords(Some("Paris")),
      LookupPriority(1),
      LookupRegion("2"),
      LookupPortType("T2"),
      LookupCountryCode("FR"),
      LookupPortCode("CDG"))
  )

  "filterBySelectionCriteria" should "return options unchanged when selectionCriteria is empty" in {
    val result = LookupOptions.filterBySelectionCriteria(List.empty, lookupOptions)
    result shouldBe lookupOptions
  }

  it should "return options unchanged when its empty" in {
    val result = LookupOptions.filterBySelectionCriteria(List.empty, emptyLookupOptions)
    result shouldBe emptyLookupOptions
  }

  it should "return empty options when no selection field exists in lookupOptions" in {
    val result = LookupOptions.filterBySelectionCriteria(
      List(SimplifiedSelectionCriteria(CsvColumnName("other"), List("other-value"))),
      countryLookupOptions)
    result shouldBe Map.empty
  }

  it should "return empty options when no selection field does not match any value in lookupOptions" in {
    val result = LookupOptions.filterBySelectionCriteria(
      List(SimplifiedSelectionCriteria(CsvColumnName("region"), List("region-value"))),
      countryLookupOptions)
    result shouldBe Map.empty
  }

  it should "return matching options, when selection field matches corresponding field in given option" in {
    val result = LookupOptions.filterBySelectionCriteria(
      List(SimplifiedSelectionCriteria(CsvColumnName("region"), List("1"))),
      countryLookupOptions)
    result shouldBe countryLookupOptions.toList.take(1).toMap
  }

  it should "return matching options, when selection fields(multiple) matches corresponding field in given option" in {
    val result = LookupOptions.filterBySelectionCriteria(
      List(
        SimplifiedSelectionCriteria(CsvColumnName("region"), List("1")),
        SimplifiedSelectionCriteria(CsvColumnName("countrycode"), List("GB")),
        SimplifiedSelectionCriteria(CsvColumnName("porttype"), List("T2"))
      ),
      portLookupOptions
    )
    result shouldBe portLookupOptions.toList.take(2).toMap
  }

}
