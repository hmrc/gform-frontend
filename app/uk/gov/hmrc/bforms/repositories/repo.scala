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

package uk.gov.hmrc.bforms.repositories

import javax.inject.Inject

import com.google.inject.Singleton
import com.sun.xml.internal.bind.v2.TODO
import play.api.libs.json.{Format, Json}
import reactivemongo.api.DB
import uk.gov.hmrc.bforms.models.{FirstName, LandFillTaxDetailsPersistence, LandfillTaxDetails, LastName}
import uk.gov.hmrc.mongo.ReactiveRepository

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by daniel-connelly on 22/12/16.
  */
//trait LandFillTaxDetailRepository {
//
//
//  def storeForm(form : LandfillTaxDetails) : Future[Either[String, Unit]]
//
////  def deleteSubmittedForm(governmentGateWay : String)
//}
//
//class LandFillTaxDetailsRepository(implicit mongo : () => DB) extends ReactiveRepository[LandFillTaxDetailsPersistence, BSONObjectID]("formData", mongo, LandFillTaxDetailsPersistence.oFormat) with LandFillTaxDetailRepository  {
//
//  def storeForm(form : LandfillTaxDetails): Future[Either[String, Unit]] = {
//
//   val store = LandFillTaxDetailsPersistence("Something" , FirstName(form.firstName), LastName(form.lastName))
//   insert(store) map {
//     case r if r.ok =>
//       logger.info(s"form with details of '${form.firstName}' & '${form.lastName}' was successfully stored")
//       Right(())
//     case r =>
//       logger.error(s"form with details of '${form.firstName}' & '${form.lastName}' was not successfully stored")
//       Left(r.message)
//   }
//  }
//
//  //Confirm what BSON object is.
////  def deleteSubmittedForm(governmentGateway: String) = { //Change to GG
////    removeById(new BSONObjectID(governmentGateway))
////  }
//}

@Singleton
class LandFillTaxRepository @Inject()(db:DB) extends ReactiveRepository[LandFillTaxDetailsPersistence, String]("formData", () => db, LandFillTaxDetailsPersistence.mongoFormat, implicitly[Format[String]]) with LandFillTaxRepo {

  def store(form: LandfillTaxDetails) = { val store = LandFillTaxDetailsPersistence("Something" , FirstName(form.firstName), LastName(form.lastName))
       insert(store) map {
         case r if r.ok =>
           logger.info(s"form with details of '${form.firstName}' & '${form.lastName}' was successfully stored")
           Right(())
         case r =>
           logger.error(s"form with details of '${form.firstName}' & '${form.lastName}' was not successfully stored")
           Left(r.message)
       }
  }

  def get() = { TODO }
}




object LandFillTaxDetailsPersistence {
  val mongoFormat = Json.format[LandFillTaxDetailsPersistence]
}

trait LandFillTaxRepo {

  def store(form : LandfillTaxDetails) : Future[Either[String, Unit]]

  def get() : Future[Either[String, Unit]]
}

