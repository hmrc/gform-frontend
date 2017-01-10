package uk.gov.hmrc.bforms.service

import uk.gov.hmrc.bforms.models.{GovernmentGatewayId, LandfillTaxDetails}
import uk.gov.hmrc.bforms.repositories.LandFillTaxRepository

import scala.concurrent.Future

/**
  * Created by daniel-connelly on 10/01/17.
  */
trait TaxFormRetrieve[A] {
  def apply(a: A): Future[Either[String,Unit]]
}

object TaxFormRetrieve {

  private def retrieveTaxForm[A](f: A => Future[Either[String, Unit]]) : TaxFormRetrieve[A] = {
    new TaxFormRetrieve[A] {
      def apply(params: A) : Future[Either[String, Unit]] = f(params)
    }
  }

  implicit def nameLater(implicit repository: LandFillTaxRepository) = {
    retrieveTaxForm((r : String) =>  repository.get(r))
  }
}

object RetrieveService {

  def retrieve[A]()(implicit taxFormRetrieve:TaxFormRetrieve[A]) = {

  }
}
