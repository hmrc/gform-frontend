package uk.gov.hmrc.gform.obligation
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{FormTemplate, HmrcTaxPeriod}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

case class HmrcTaxPeriodIdentifier(idType: String, idNumber: String, regimeType: String)

class ObligationService(gformConnector: GformConnector) {



  def lookupObligations(formTemplate: FormTemplate, retrievals: MaterialisedRetrievals)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext) = {
    val x = formTemplate.sections.flatMap(i => i.fields.flatMap(j => j.`type` match {
      case HmrcTaxPeriod(a,b,c) => Some(HmrcTaxPeriodIdentifier(a,b,c))
      case _ => None
    }))

    //val xx = x.map(i => (i, gformConnector.getTaxPeriods(i))).map(i => i._2.map(f => (i._1, f)))

   // val xxx = xx.map(i => i._2.map(f => (i._1, f)))

    val xxxx = Future.sequence(x.map(i => (i, gformConnector.getTaxPeriods(i)))
      .map(i => i._2.map(f => (i._1, f))))
      .map(x => x.toMap)

    xxxx

  }

}
