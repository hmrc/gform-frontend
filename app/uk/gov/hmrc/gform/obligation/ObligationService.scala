/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.obligation
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, HmrcTaxPeriod }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

case class HmrcTaxPeriodIdentifier(idType: String, idNumber: String, regimeType: String)

class ObligationService(gformConnector: GformConnector) {

  def lookupObligations(formTemplate: FormTemplate, retrievals: MaterialisedRetrievals)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext) = {
    val hmrcTaxPeriodIdentifiers = formTemplate.sections.flatMap(i =>
      i.fields.flatMap(j =>
        j.`type` match {
          case HmrcTaxPeriod(a, b, c) => Some(HmrcTaxPeriodIdentifier(a, b, c))
          case _                      => None
      }))
    Future
      .sequence(
        hmrcTaxPeriodIdentifiers
          .map(i => (i, gformConnector.getTaxPeriods(i).map(j => j.obligations)))
          .map(i => i._2.map(f => (i._1, f))))
      .map(x => x.toMap)
  }
}
