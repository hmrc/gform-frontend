/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.services

import play.api.libs.json.Json
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.keystore.{ RepeatingComponentService, SessionCacheConnector }
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.form.RepeatingGroup
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.cache.client.CacheMap

import scala.collection.immutable.List
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

class RepeatingComponentServiceSpec extends Spec with ExampleData {

  implicit lazy val hc = HeaderCarrier()

  "getAllSections" should "return only sections in template when no repeating sections are defined" in {

    val formTemplate = super.formTemplate.copy(sections = List(`section - group`))

    val testSessionCacheConnector = new SessionCacheConnector(null, null, null, null) {
      override def fetch()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CacheMap]] = Future.successful(None)
    }

    val testService = new RepeatingComponentService(testSessionCacheConnector, null)

    testService.getAllSections(formTemplate, Map.empty, testService.getCache).futureValue shouldBe List(`section - group`)
  }

  it should "return no dynamically created sections when field in repeatsMax expression in repeating group and no form data" in {

    val formTemplate = super.formTemplate.copy(sections = List(`section - group`, `repeating section`))

    val testSessionCacheConnector = new SessionCacheConnector(null, null, null, null) {
      override def fetch()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CacheMap]] = Future.successful(None)
    }

    val testService = new RepeatingComponentService(testSessionCacheConnector, null)

    val expectedList = List(`section - group`)

    testService.getAllSections(formTemplate, Map.empty, testService.getCache).futureValue shouldBe expectedList
  }

  it should "return dynamically created sections (title and shortName text built dynamically) when field to track in repeating group, and non-empty form data" in {

    val thisGroup = `group - type`.copy(
      repeatsMax = Some(4),
      repeatsMin = Some(1),
      repeatLabel = Some("RepGrpLabel"),
      repeatAddAnotherText = Some("AddButtonLabel")
    )

    val thisGroupFieldValue = `fieldValue - group`.copy(`type` = thisGroup)

    val thisSection1 = `section - group`.copy(fields = List(thisGroupFieldValue))

    val thisSection2 = `repeating section`.copy(
      title = """${n_repeatingSectionDriver}, $n""",
      shortName = Some("""$n, ${n_repeatingSectionDriver}""")
    )
    val formTemplate = super.formTemplate.copy(sections = List(thisSection1, thisSection2))

    val textFieldR = `fieldValue - surname`.copy(id = FormComponentId(s"1_${`fieldId - surname`.value}"))
    val sectionR = thisSection2.copy(fields = List(textFieldR), title = "ONE, 1", shortName = Some("1, ONE"))

    val textFieldR2 = `fieldValue - surname`.copy(id = FormComponentId(s"2_${`fieldId - surname`.value}"))
    val sectionR2 = thisSection2.copy(fields = List(textFieldR2), title = "TWO, 2", shortName = Some("2, TWO"))

    val jsValue = Json.toJson(RepeatingGroup(List(List(`fieldValue - firstName`), List(`fieldValue - firstName`.copy(id = FormComponentId(s"1_${`fieldId - firstName`}")))), true))
    val mockCache = CacheMap("YEAH_MAN", Map("GroupFieldValueId" -> jsValue))
    val testSessionCacheConnector = new SessionCacheConnector(null, null, null, null) {
      override def fetch()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CacheMap]] = Future.successful(Some(mockCache))
    }

    val testService = new RepeatingComponentService(testSessionCacheConnector, null)

    val expectedList = List(thisSection1, sectionR, sectionR2)

    val formData = Map(FormComponentId("repeatingSectionDriver") -> Seq("ONE"), FormComponentId("1_repeatingSectionDriver") -> Seq("TWO"))

    testService.getAllSections(formTemplate, formData, testService.getCache).futureValue shouldBe expectedList
  }

  it should "return a dynamically created section when field to track in a NON-repeating group" in {
    val thisSection2 = `repeating section`.copy(
      title = "Repeating section title $n",
      shortName = Some("shortName $n")
    )

    val formTemplate = super.formTemplate.copy(sections = List(`section - group`, thisSection2))

    val jsValue = Json.toJson(List(List(`fieldValue - firstName`)))
    val mockCache = CacheMap("YEAH_MAN", Map("GroupFieldValueId" -> jsValue))
    val testSessionCacheConnector = new SessionCacheConnector(null, null, null, null) {
      override def fetch()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CacheMap]] = Future.successful(Some(mockCache))
    }

    val testService = new RepeatingComponentService(testSessionCacheConnector, null)

    val textFieldDosR = `fieldValue - surname`.copy(id = FormComponentId(s"1_${`fieldId - surname`.value}"))
    val sectionR = thisSection2.copy(fields = List(textFieldDosR), title = "Repeating section title 1", shortName = Some("shortName 1"))
    val expectedList = List(`section - group`, sectionR)

    val formData = Map(`fieldId - firstName` -> Seq("1"))

    testService.getAllSections(formTemplate, formData, testService.getCache).futureValue shouldBe expectedList
  }

  it should "return dynamically created sections (title and shortName text built dynamically) when field to track in a NON-repeating group, with form data" in {
    val thisSection2 = `repeating section`.copy(
      title = "Repeating section title $n",
      shortName = Some("shortName $n")
    )
    val formTemplate = super.formTemplate.copy(sections = List(`section - group`, thisSection2))

    val testSessionCacheConnector = new SessionCacheConnector(null, null, null, null) {
      override def fetch()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CacheMap]] = Future.successful(None)
    }

    val testService = new RepeatingComponentService(testSessionCacheConnector, null)

    val textFieldDos1 = `fieldValue - surname`.copy(id = FormComponentId(s"1_${`fieldId - surname`.value}"))
    val textFieldDos2 = `fieldValue - surname`.copy(id = FormComponentId(s"2_${`fieldId - surname`.value}"))
    val sectionR1 = thisSection2.copy(fields = List(textFieldDos1), title = "Repeating section title 1", shortName = Some("shortName 1"))
    val sectionR2 = thisSection2.copy(fields = List(textFieldDos2), title = "Repeating section title 2", shortName = Some("shortName 2"))
    val expectedList = List(`section - group`, sectionR1, sectionR2)

    val formData = Map(`fieldId - firstName` -> Seq("2"))

    testService.getAllSections(formTemplate, formData, testService.getCache).futureValue shouldBe expectedList
  }
}
