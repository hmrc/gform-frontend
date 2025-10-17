/*
 * Copyright 2025 HM Revenue & Customs
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

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class ChoiceRuntimeIndexServiceSpec extends AnyFlatSpecLike with Matchers {

  val service = new ChoiceRuntimeIndexService()
  val indexKey = "test-index-key-123"

  val sampleChoiceOptions = List(
    ChoiceOption("limitedcompany", "Limited Company", Some("ltd")),
    ChoiceOption("partnership", "Partnership", Some("part")),
    ChoiceOption("soletrader", "Sole Trader", Some("sole")),
    ChoiceOption("charity", "Charitable Organisation", Some("charity")),
    ChoiceOption("corporation", "Corporation", None), // No keyword
    ChoiceOption("publiclimitedcompany", "Public Limited Company", Some("plc"))
  )

  "ChoiceRuntimeIndexService" should "create index successfully" in {
    service.createIndexForChoiceOptions(indexKey, sampleChoiceOptions)
  }

  it should "return empty results for non-existent index key" in {
    val results = service.search("non-existent", "test")
    results shouldBe List.empty
  }

  it should "return empty results for empty search query" in {
    service.createIndexForChoiceOptions(indexKey, sampleChoiceOptions)
    val results = service.search(indexKey, "")
    results shouldBe List.empty
  }

  it should "return empty results for whitespace-only search query" in {
    service.createIndexForChoiceOptions(indexKey, sampleChoiceOptions)
    val results = service.search(indexKey, "   ")
    results shouldBe List.empty
  }

  "Exact keyword search" should "find results by exact keyword match" in {
    service.createIndexForChoiceOptions(indexKey, sampleChoiceOptions)

    val results = service.search(indexKey, "ltd")
    results should have size 1
    results.head.value shouldBe "limitedcompany"
    results.head.label shouldBe "Limited Company"
  }

  it should "find results by exact keyword match case insensitive" in {
    service.createIndexForChoiceOptions(indexKey, sampleChoiceOptions)

    val results = service.search(indexKey, "LTD")
    results should have size 1
    results.head.value shouldBe "limitedcompany"
    results.head.label shouldBe "Limited Company"
  }

  it should "find multiple results with same keyword" in {
    val optionsWithDuplicateKeyword = List(
      ChoiceOption("option1", "Option One", Some("test")),
      ChoiceOption("option2", "Option Two", Some("test"))
    )
    val testIndexKey = "duplicate-test-key"
    service.createIndexForChoiceOptions(testIndexKey, optionsWithDuplicateKeyword)

    val results = service.search(testIndexKey, "test")
    results should have size 2
    results.map(_.value) should contain theSameElementsAs List("option1", "option2")
  }

  "Label prefix search" should "find results by label prefix" in {
    service.createIndexForChoiceOptions(indexKey, sampleChoiceOptions)

    val results = service.search(indexKey, "lim")
    results should have size 2
    results.map(_.value) should contain theSameElementsAs List("limitedcompany", "publiclimitedcompany")
  }

  it should "find results by label prefix case insensitive" in {
    service.createIndexForChoiceOptions(indexKey, sampleChoiceOptions)

    val results = service.search(indexKey, "LIM")
    results should have size 2
    results.map(_.value) should contain theSameElementsAs List("limitedcompany", "publiclimitedcompany")
  }

  it should "find multiple results with same prefix" in {
    service.createIndexForChoiceOptions(indexKey, sampleChoiceOptions)

    val results = service.search(indexKey, "p")
    results should have size 2
    results.map(_.value) should contain theSameElementsAs List("partnership", "publiclimitedcompany")
  }

  "Keyword prefix search" should "find results by keyword prefix in searchTerms" in {
    service.createIndexForChoiceOptions(indexKey, sampleChoiceOptions)

    val results = service.search(indexKey, "par")
    results should have size 1
    results.head.value shouldBe "partnership"
    results.head.label shouldBe "Partnership"
  }

  it should "find results by partial keyword" in {
    service.createIndexForChoiceOptions(indexKey, sampleChoiceOptions)

    val results = service.search(indexKey, "pl")
    results should have size 1
    results.head.value shouldBe "publiclimitedcompany"
    results.head.label shouldBe "Public Limited Company"
  }

  "Multi-word search" should "find results when searching with multiple terms" in {
    service.createIndexForChoiceOptions(indexKey, sampleChoiceOptions)

    val results = service.search(indexKey, "public limited")
    results should have size 1
    results.map(_.value) should contain theSameElementsAs List("publiclimitedcompany")
  }

  "Search priority" should "prioritize exact keyword match over prefix match" in {
    val optionsWithOverlap = List(
      ChoiceOption("limited", "Limited", Some("ltd")),
      ChoiceOption("limitedcompany", "Limited Company", Some("limitedco"))
    )
    val testIndexKey = "priority-test-key"
    service.createIndexForChoiceOptions(testIndexKey, optionsWithOverlap)

    val results = service.search(testIndexKey, "ltd")
    results should have size 1
    results.head.value shouldBe "limited"
  }

  "Edge cases" should "handle options with no keywords" in {
    service.createIndexForChoiceOptions(indexKey, sampleChoiceOptions)

    val results = service.search(indexKey, "corp")
    results should have size 1
    results.head.value shouldBe "corporation"
    results.head.label shouldBe "Corporation"
  }

  it should "handle empty keyword" in {
    val optionsWithEmptyKeyword = List(
      ChoiceOption("test", "Test Option", Some("")),
      ChoiceOption("valid", "Valid Option", Some("valid"))
    )
    val testIndexKey = "empty-keyword-test-key"
    service.createIndexForChoiceOptions(testIndexKey, optionsWithEmptyKeyword)

    val results = service.search(testIndexKey, "valid")
    results should have size 1
    results.head.value shouldBe "valid"
  }

  it should "handle whitespace in keywords" in {
    val optionsWithWhitespaceKeyword = List(
      ChoiceOption("test", "Test Option", Some("  test  ")),
      ChoiceOption("valid", "Valid Option", Some("valid"))
    )
    val testIndexKey = "whitespace-keyword-test-key"
    service.createIndexForChoiceOptions(testIndexKey, optionsWithWhitespaceKeyword)

    val results = service.search(testIndexKey, "test")
    results should have size 1
    results.head.value shouldBe "test"
  }

  it should "limit results to maxResults parameter" in {
    val manyOptions = (1 to 20).map { i =>
      ChoiceOption(s"option$i", s"Option $i", Some("test"))
    }.toList
    val testIndexKey = "limit-test-key"
    service.createIndexForChoiceOptions(testIndexKey, manyOptions)

    val results = service.search(testIndexKey, "test")
    results should have size 20
  }

  "Real world example" should "work with the given example data" in {
    val realWorldOptions = List(
      ChoiceOption("limitedcompany", "Limited Company", Some("ltd")),
      ChoiceOption("partnership", "Partnership", None),
      ChoiceOption("soletrader", "Sole Trader", None)
    )
    val testIndexKey = "real-world-test-key"
    service.createIndexForChoiceOptions(testIndexKey, realWorldOptions)

    //mimic concurrent users
    val testIndexKey2 = "real-world-test-key-2"
    service.createIndexForChoiceOptions(testIndexKey2, realWorldOptions)

    // Test exact keyword search
    val ltdResults = service.search(testIndexKey, "ltd")
    ltdResults should have size 1
    ltdResults.head.value shouldBe "limitedcompany"
    ltdResults.head.label shouldBe "Limited Company"

    // Test label prefix search
    val limResults = service.search(testIndexKey, "lim")
    limResults should have size 1
    limResults.head.value shouldBe "limitedcompany"
    limResults.head.label shouldBe "Limited Company"

    // Both should return the same result
    ltdResults shouldBe limResults
  }
}
