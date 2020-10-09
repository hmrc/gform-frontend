package uk.gov.hmrc.gform

import org.scalatest.{Matchers, WordSpec}
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.LangADT.languages
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Register

import scala.collection.JavaConverters._

class LookupLoaderSpec extends WordSpec with Matchers {

  val loader = new LookupLoader()

  "registry" should {
    "contain CashType" in {
      val cashTypes: LookupType = loader.registerLookup(Register.CashType)
      cashTypes match {
        case RadioLookup(lookupOptions) =>
          languages.foreach { l =>
            lookupOptions.m.contains(l) shouldBe true
            lookupOptions.m(l).options.size shouldBe 10
            lookupOptions.m(l).options.head shouldBe (LookupLabel("Other") -> LookupInfoDefault(
              LookupId("7"),
              LookupLabel("Other"),
              LookupLabel("Other")))
          }

        case _ => fail()
      }
    }

    "contain Countries" in {

      val countriesType: LookupType = loader.registerLookup(Register.Country)

      countriesType match {
        case AjaxLookup(options, autocomplete, showAll) =>
          options.m(LangADT.En).options.size shouldBe 254
          autocomplete(LangADT.En)
            .search("United Kingdom")
            .asScala shouldBe List(
            LookupRecord("United Kingdom", LookupPriority(30), LookupKeyWords("England Great Britain")))
          showAll shouldBe ShowAll.Disabled
        case _ => fail()
      }
    }
  }
}
