package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators

import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationPrint
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.PrintSection.{Page, Pdf}

trait DestinationPrintGen {
  def formIdListGen: Gen[List[FormComponentId]] = PrimitiveGen.zeroOrMoreGen(FormComponentGen.formComponentIdGen)

  def destinationPrintPageGen: Gen[Page] =
    for {
      titleSmartString        <- SmartStringGen.smartStringGen
      instructionsSmartString <- SmartStringGen.smartStringGen
    } yield Page(titleSmartString, instructionsSmartString)

  def destinationPrintPdfGen: Gen[Pdf] =
    for {
      headerSmartString <- SmartStringGen.smartStringGen
      footerSmartString <- SmartStringGen.smartStringGen
      fieldIds          <- formIdListGen
    } yield Pdf(headerSmartString, footerSmartString, fieldIds)

  def destinationPrintGen: Gen[DestinationPrint] =
    for {
      page <- destinationPrintPageGen
      pdf  <- Gen.option(destinationPrintPdfGen)
    } yield DestinationPrint(page, pdf)

}

object DestinationPrintGen extends DestinationPrintGen
