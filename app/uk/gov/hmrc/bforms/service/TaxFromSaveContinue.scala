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

package uk.gov.hmrc.bforms.service

import uk.gov.hmrc.bforms.models.LandfillTaxDetails

import scala.concurrent.Future

/**
  * Created by daniel-connelly on 22/12/16.
  */
object TaxFormSaveContinue extends TaxFormSaveContinue

case class SaveContinueResult(errorMessage : Option[String], saveContinueAcknowledgement : Option[String])

trait TaxFormSaveContinue {

  def saveContinueForm(formData: LandfillTaxDetails) : Future[SaveContinueResult] = Future.successful(SaveContinueResult(None, Some("KAKAKAKAXXX")))
}
