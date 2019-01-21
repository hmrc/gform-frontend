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
import uk.gov.hmrc.gform.sharedmodel.{TaxPeriod, TaxPeriods}
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class ObligationService(gformConnector: GformConnector) {

  val stringToDate = new SimpleDateFormat("yyyy-MM-dd")

  def lookupObligations(formTemplate: FormTemplate)(implicit hc: HeaderCarrier, ec: ExecutionContext) = {
    val hmrcTaxPeriodIdentifiers = formTemplate.expandFormTemplateFull.allFCs.collect {
      case IsHmrcTaxPeriod(el) => el
    }
    Future
      .traverse(hmrcTaxPeriodIdentifiers)(i => gformConnector.getTaxPeriods(i).map(j => (i, j.obligations)))
      .map(x => x.toMap)
  }

  def lookupObligationsMultiple(formTemplate: FormTemplate)(implicit hc: HeaderCarrier, ec: ExecutionContext) = {
    val hmrcTaxPeriodIdentifiers = formTemplate.expandFormTemplateFull.allFCs.collect {
      case IsHmrcTaxPeriod(el) => el
    }
    val abc = gformConnector.getAllTaxPeriods(hmrcTaxPeriodIdentifiers).map(h=> h.flatMap(j => j.obligations.flatMap(k => Map(new HmrcTaxPeriod(new IdType(k.identification.referenceType), new IdNumber(k.identification.referenceNumber), new RegimeType(k.identification.incomeSourceType)) ->
      new TaxPeriods(k.obligationDetails.map(l => new TaxPeriod(stringToDate.parse(l.inboundCorrespondenceFromDate), stringToDate.parse(l.inboundCorrespondenceToDate), l.periodKey)))))))
    abc.map(x => x.toMap)
  }
}
