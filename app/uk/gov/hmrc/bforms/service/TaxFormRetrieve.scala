/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.service

import uk.gov.hmrc.bforms.models.LandFillTaxDetailsPersistence
import uk.gov.hmrc.bforms.repositories.LandFillTaxRepository

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by daniel-connelly on 10/01/17.
  */
trait TaxFormRetrieve[A, B] {
  def apply(a: A) : Future[List[B]]
}

object TaxFormRetrieve {

  private def retrieveTaxForm[A, B](f: A => Future[List[B]]) : TaxFormRetrieve[A, B] = {
    new TaxFormRetrieve[A, B] {
      def apply(params: A) : Future[List[B]]= f(params)
    }
  }

  implicit def somethingElse(implicit repository: LandFillTaxRepository) : TaxFormRetrieve[String, LandFillTaxDetailsPersistence]  = {
    retrieveTaxForm((r : String) =>  repository.get(r))
  }
}

object RetrieveService {

  def retrieve[A, B](registrationNumber:A)(implicit taxFormRetrieve:TaxFormRetrieve[A, B]):Future[Either[String, List[LandFillTaxDetailsPersistence]]] = {
    taxFormRetrieve(registrationNumber).flatMap{
      case user:List[LandFillTaxDetailsPersistence] => Future.successful(Right(user))
      case _ => Future.successful(Left("Bob"))
    }
  }
}
