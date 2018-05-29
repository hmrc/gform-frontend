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

package uk.gov.hmrc.gform.controllers

import org.scalatest.mockito.MockitoSugar.mock
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ IncludeIf, IsFalse }

class SaveSpec extends Spec {

  behavior of "navigate - Save"

  it should "all sections are included" in new Fixture {
    override def data = super.data + (`fieldId - save` -> `formField - Save`)
    new Navigator(sectionNumber0, allSections, rawDataFromBrowser, retrievals).navigate shouldBe SaveAndExit
    new Navigator(sectionNumber1, allSections, rawDataFromBrowser, retrievals).navigate shouldBe SaveAndExit
    new Navigator(sectionNumber2, allSections, rawDataFromBrowser, retrievals).navigate shouldBe SaveAndExit
  }

  it should "mid section is excluded" in new FixtureExcludedMidSection {
    override def data = super.data + (`fieldId - save` -> `formField - Save`)
    new Navigator(sectionNumber0, allSections, rawDataFromBrowser, retrievals).navigate shouldBe SaveAndExit
    new Navigator(sectionNumber1, allSections, rawDataFromBrowser, retrievals).navigate shouldBe SaveAndExit
    new Navigator(sectionNumber2, allSections, rawDataFromBrowser, retrievals).navigate shouldBe SaveAndExit
  }

  behavior of "navigate - Continue"

  it should "all sections are included" in new Fixture {
    override def data = super.data + (`fieldId - save` -> `formField - Continue`)
    new Navigator(sectionNumber0, allSections, rawDataFromBrowser, retrievals).navigate shouldBe SaveAndContinue(
      sectionNumber1)
    new Navigator(sectionNumber1, allSections, rawDataFromBrowser, retrievals).navigate shouldBe SaveAndContinue(
      sectionNumber2)
    new Navigator(sectionNumber2, allSections, rawDataFromBrowser, retrievals).navigate shouldBe SaveAndSummary

  }

  it should "mid section is excluded" in new FixtureExcludedMidSection {
    override def data = super.data + (`fieldId - save` -> `formField - Continue`)

    new Navigator(sectionNumber0, allSections, rawDataFromBrowser, retrievals).navigate shouldBe SaveAndContinue(
      sectionNumber2)
    new Navigator(sectionNumber1, allSections, rawDataFromBrowser, retrievals).navigate shouldBe SaveAndContinue(
      sectionNumber2)
    new Navigator(sectionNumber2, allSections, rawDataFromBrowser, retrievals).navigate shouldBe SaveAndSummary
  }

  behavior of "navigate - Back"

  it should "all sections are included" in new Fixture {
    override def data = super.data + (`fieldId - save` -> `formField - Back`)
    new Navigator(sectionNumber0, allSections, rawDataFromBrowser, retrievals).navigate shouldBe Back(sectionNumber0)
    new Navigator(sectionNumber1, allSections, rawDataFromBrowser, retrievals).navigate shouldBe Back(sectionNumber0)
    new Navigator(sectionNumber2, allSections, rawDataFromBrowser, retrievals).navigate shouldBe Back(sectionNumber1)

  }

  it should "mid section is excluded" in new FixtureExcludedMidSection {
    override def data = super.data + (`fieldId - save` -> `formField - Back`)
    new Navigator(sectionNumber0, allSections, rawDataFromBrowser, retrievals).navigate shouldBe Back(sectionNumber0)
    new Navigator(sectionNumber1, allSections, rawDataFromBrowser, retrievals).navigate shouldBe Back(sectionNumber0)
    new Navigator(sectionNumber2, allSections, rawDataFromBrowser, retrievals).navigate shouldBe Back(sectionNumber0)
  }

  behavior of "new Navigator"

  it should "throw exception if section numbers are out of bounds" in new Fixture {
    an[IllegalArgumentException] shouldBe thrownBy(
      new Navigator(`sectionNumber-1`, allSections, rawDataFromBrowser, retrievals))
    an[IllegalArgumentException] shouldBe thrownBy(
      new Navigator(sectionNumber3, allSections, rawDataFromBrowser, retrievals))
  }

  trait Fixture extends ExampleData {
    override def allSections = List(
      `section - about you`,
      `section - businessDetails`,
      `section - group`
    )

    val retrievals = mock[MaterialisedRetrievals]

  }

  trait FixtureExcludedMidSection extends Fixture {
    override def `section - businessDetails` =
      super.`section - businessDetails`.copy(includeIf = Some(IncludeIf(IsFalse)))
  }

}
