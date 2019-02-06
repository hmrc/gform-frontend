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
import java.text.SimpleDateFormat

import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.ExecutionContext

class ObligationService(gformConnector: GformConnector) {

  val stringToDate = new SimpleDateFormat("yyyy-MM-dd")

  def lookupObligationsMultiple(formTemplate: FormTemplate)(implicit hc: HeaderCarrier, ec: ExecutionContext) = {
    val hmrcTaxPeriodIdentifiers = formTemplate.expandFormTemplateFull.allFCs.collect {
      case IsHmrcTaxPeriod(el) => el
    }
    val futureListOfTaxPeriodDes =
      gformConnector
        .getAllTaxPeriods(hmrcTaxPeriodIdentifiers)
        .map(i => i.flatMap(j => j.obligation.obligations.flatMap(h => makeMap(j.id, h.obligationDetails))))
    futureListOfTaxPeriodDes.map(i => i.toMap)
  }

  def makeMap(id: HmrcTaxPeriod, obligation: List[ObligationDetail]) =
    Map(
      id ->
        TaxPeriods(obligation.map(l =>
          TaxPeriod(l.inboundCorrespondenceFromDate, l.inboundCorrespondenceToDate, l.periodKey))))
}
