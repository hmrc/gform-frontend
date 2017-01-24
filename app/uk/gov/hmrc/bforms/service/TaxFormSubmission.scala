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

import uk.gov.hmrc.bforms.connectors.BformsConnector
import uk.gov.hmrc.play.http.HeaderCarrier
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object TaxFormSubmission extends TaxFormSubmission

case class SubmissionResult(errorMessage: Option[String], submissionAcknowledgement: Option[String])

trait TaxFormSubmission {

  val bformsconnector = BformsConnector

  def submit(registrationNumber: String)(implicit hc: HeaderCarrier): Future[SubmissionResult] =
    bformsconnector.submit(registrationNumber).map { x => SubmissionResult(None, Some(x.body)) }

}
