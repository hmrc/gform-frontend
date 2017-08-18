package uk.gov.hmrc.gform.controllers

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.controllers.Navigator
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{IncludeIf, IsFalse}

class SaveSpec extends Spec {

  behavior of "navigate - Save"

  it should "all sections are included" in new Fixture {
    override def data = super.data + (`fieldId - save` -> `formField - Save`)
    new Navigator(sectionNumber0, allSections, rawDataFromBrowser).navigate shouldBe SaveAndExit
    new Navigator(sectionNumber1, allSections, rawDataFromBrowser).navigate shouldBe SaveAndExit
    new Navigator(sectionNumber2, allSections, rawDataFromBrowser).navigate shouldBe SaveAndExit
  }

  it should "mid section is excluded" in new FixtureExcludedMidSection {
    override def data = super.data + (`fieldId - save` -> `formField - Save`)
    new Navigator(sectionNumber0, allSections, rawDataFromBrowser).navigate shouldBe SaveAndExit
    new Navigator(sectionNumber1, allSections, rawDataFromBrowser).navigate shouldBe SaveAndExit
    new Navigator(sectionNumber2, allSections, rawDataFromBrowser).navigate shouldBe SaveAndExit
  }

  behavior of "navigate - Continue"

  it should "all sections are included" in new Fixture {
    override def data = super.data + (`fieldId - save` -> `formField - Continue`)
    new Navigator(sectionNumber0, allSections, rawDataFromBrowser).navigate shouldBe SaveAndContinue(sectionNumber1)
    new Navigator(sectionNumber1, allSections, rawDataFromBrowser).navigate shouldBe SaveAndContinue(sectionNumber2)
    new Navigator(sectionNumber2, allSections, rawDataFromBrowser).navigate shouldBe SaveAndSummary

  }

  it should "mid section is excluded" in new FixtureExcludedMidSection {
    override def data = super.data + (`fieldId - save` -> `formField - Continue`)

    new Navigator(sectionNumber0, allSections, rawDataFromBrowser).navigate shouldBe SaveAndContinue(sectionNumber2)
    new Navigator(sectionNumber1, allSections, rawDataFromBrowser).navigate shouldBe SaveAndContinue(sectionNumber2)
    new Navigator(sectionNumber2, allSections, rawDataFromBrowser).navigate shouldBe SaveAndSummary
  }

  behavior of "navigate - Back"

  it should "all sections are included" in new Fixture {
    override def data = super.data + (`fieldId - save` -> `formField - Back`)
    new Navigator(sectionNumber0, allSections, rawDataFromBrowser).navigate shouldBe Back(sectionNumber0)
    new Navigator(sectionNumber1, allSections, rawDataFromBrowser).navigate shouldBe Back(sectionNumber0)
    new Navigator(sectionNumber2, allSections, rawDataFromBrowser).navigate shouldBe Back(sectionNumber1)

  }

  it should "mid section is excluded" in new FixtureExcludedMidSection {
    override def data = super.data + (`fieldId - save` -> `formField - Back`)
    new Navigator(sectionNumber0, allSections, rawDataFromBrowser).navigate shouldBe Back(sectionNumber0)
    new Navigator(sectionNumber1, allSections, rawDataFromBrowser).navigate shouldBe Back(sectionNumber0)
    new Navigator(sectionNumber2, allSections, rawDataFromBrowser).navigate shouldBe Back(sectionNumber0)
  }

  behavior of "new Navigator"

  it should "throw exception if section numbers are out of bounds" in {
    an[IllegalArgumentException] shouldBe thrownBy (new Navigator(`sectionNumber-1`, allSections, rawDataFromBrowser))
    an[IllegalArgumentException] shouldBe thrownBy (new Navigator(sectionNumber3, allSections, rawDataFromBrowser))
  }


  trait Fixture extends ExampleData {
    override def allSections = List(
      `section - about you`,
      `section - businessDetails`,
      `section - group`
    )
  }

  trait FixtureExcludedMidSection extends Fixture {
    override def `section - businessDetails` = super.`section - businessDetails`.copy(includeIf = Some(IncludeIf(IsFalse)))
  }

}
