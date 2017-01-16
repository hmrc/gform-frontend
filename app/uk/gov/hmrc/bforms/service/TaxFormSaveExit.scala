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

import uk.gov.hmrc.bforms.repositories.LandFillTaxRepository
import uk.gov.hmrc.bforms.models.LandfillTaxDetails

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by daniel-connelly on 06/01/17.
  */
trait TaxFormSaveExit[A] {
  def apply(a: A): Future[Either[String,Unit]]
}

object TaxFormSaveExit {

  private def getTaxFormSaveExit[A](f: A => Future[Either[String, Unit]]) : TaxFormSaveExit[A] = {
    new TaxFormSaveExit[A] {
      def apply(params: A) : Future[Either[String, Unit]] = f(params)
    }
  }

  implicit def nameLater(implicit repository: LandFillTaxRepository): TaxFormSaveExit[Either[LandfillTaxDetails, Map[String, String]]] = {
    getTaxFormSaveExit((r : Either[LandfillTaxDetails, Map[String, String]]) =>  repository.store(r))
  }
}

object SaveExit {

  def SaveForm[A, B](formDetails:Either[A, B])(implicit taxFormSaveExit:TaxFormSaveExit[Either[A, B]]):Future[Boolean] = {
    taxFormSaveExit(formDetails).map {
      case _ => true
      case x => false
    }
  }
}
