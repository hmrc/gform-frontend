/*
 * Copyright 2016 HM Revenue & Customs
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

import reactivemongo.api.DB
import reactivemongo.bson.BSONObjectID
import uk.gov.hmrc.bforms.models.{LandFillTaxDetailsPersistence, LandfillTaxDetailsForm}
import uk.gov.hmrc.mongo.ReactiveRepository
import uk.gov.hmrc.play.frontend.auth.GovernmentGateway

import scala.concurrent.Future

/**
  * Created by daniel-connelly on 22/12/16.
  */
trait LandFillTaxDetails {

  def storeForm(form : LandfillTaxDetailsForm) : Future[Either[String, Unit]]

  def deleteSubmittedForm(governmentGateWay : String)

}

class LandFillTaxDetailsRepository(implicit mongo : () => DB) extends ReactiveRepository[LandfillTaxDetailsForm, BSONObjectID]("formData", mongo, LandfillTaxDetailsForm.oFormat)  {

  def storeForm(form : LandFillTaxDetailsForm): Future[Either[String, Unit]] = {

   val store = LandFillTaxDetailsPersistence(form.firstName, form.secondName, form.blue)
   insert(store) map {
     case r if r.ok =>
       logger.info(s"form with details of '${form.firstName}' & '${form.secondName}' was successfully stored")
       Right(())
     case r =>
       logger.error(s"form with details of '${form.firstName}' & '${form.secondName}' was not successfully stored")
       Left(r.message)
   }
  }

  def deleteSubmittedForm(governmentGateway: String) = { //Change to GG
    removeById(new BSONObjectID(governmentGateway))
  }
}

object LandFillTaxDetailsObject extends LandFillTaxDetailsRepository