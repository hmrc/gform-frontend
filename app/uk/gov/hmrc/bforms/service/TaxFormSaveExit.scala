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

import uk.gov.hmrc.bforms.repositories.LandFillTaxDetailRepository
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

  implicit def fasfsaf(implicit repository: LandFillTaxDetailRepository): TaxFormSaveExit[LandfillTaxDetails] = {
    getTaxFormSaveExit((r : LandfillTaxDetails) =>  Future.successful(Right(())))
  }
}

object SaveExit {

  def SaveForm[A](formDetails:String)(implicit taxFormSaveExit:TaxFormSaveExit[A]):Future[Unit] = {
//    taxFormSaveExit(formDetails).map {
//      case Right(_) => ()
//      case Left(x) => x
//    }
    Future.successful(())
  }
}
