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

import play.api.Logger
import uk.gov.hmrc.bforms.models.LandFillTaxDetailsPersistence
import uk.gov.hmrc.bforms.repositories.LandFillTaxRepository

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by daniel-connelly on 10/01/17.
  */
trait TaxFormRetrieve[A, B, C] {
  def apply(a: A) : Future[List[Either[B, C]]]
}

object TaxFormRetrieve {

  private def retrieveTaxForm[A, B, C](f: A => Future[List[Either[B, C]]]) : TaxFormRetrieve[A, B, C] = {
    new TaxFormRetrieve[A, B, C] {
      def apply(params: A) : Future[List[Either[B, C]]] = f(params)
    }
  }

  implicit def somethingElse(implicit repository: LandFillTaxRepository) : TaxFormRetrieve[String, LandFillTaxDetailsPersistence, Map[String, String]]  = {
    retrieveTaxForm((f : String) =>  repository.get(f))
  }
}

object RetrieveService {

  def retrieve[A, B, C](registrationNumber:A)(implicit taxFormRetrieve:TaxFormRetrieve[A, LandFillTaxDetailsPersistence, Map[String, String]]) : Future[Either[Unit, Either[LandFillTaxDetailsPersistence, Map[String, String]]]] = {
    taxFormRetrieve(registrationNumber).flatMap {
      case obj: List[Either[LandFillTaxDetailsPersistence, Map[String, String]]] if(obj.isEmpty) => {
        println("emptyList")
        Future.successful(Left(()))
      }
      case obj: List[Either[LandFillTaxDetailsPersistence, Map[String, String]]] => obj(0).fold(
        left => {
          println("left")
          Future.successful(Right(Left(left)))
        },
        right => {
          println("right")
          Future.successful(Right(Right(right)))
        }
      )
      }
  }
}