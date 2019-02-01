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

package uk.gov.hmrc.gform.sharedmodel

import java.util.Date

import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.HmrcTaxPeriod

case class TaxPeriods(taxPeriods: List[TaxPeriod])

object TaxPeriods {
  implicit val format: OFormat[TaxPeriods] = Json.format[TaxPeriods]
}

case class TaxPeriod(inboundCorrespondenceFromDate: Date, inboundCorrespondenceToDate: Date, periodKey: String)

object TaxPeriod {
  implicit val format: OFormat[TaxPeriod] = Json.format[TaxPeriod]
}

case class Obligations(obligations: TaxPeriods)

object Obligations {
  implicit val format: OFormat[Obligations] = Json.format[Obligations]
}

case class Identification(incomeSourceType: String, referenceNumber: String, referenceType: String)

object Identification {
  implicit val format: OFormat[Identification] = Json.format[Identification]
}

case class ObligationDetail(
  status: String,
  inboundCorrespondenceFromDate: String,
  inboundCorrespondenceToDate: String,
  inboundCorrespondenceDueDate: String,
  periodKey: String)

object ObligationDetail {
  implicit val format: OFormat[ObligationDetail] = Json.format[ObligationDetail]
}

case class TaxPeriodDes(identification: Identification, obligationDetails: List[ObligationDetail])

object TaxPeriodDes {
  implicit val format: OFormat[TaxPeriodDes] = Json.format[TaxPeriodDes]
}

case class ObligationDetails(obligationDetails: List[ObligationDetail])

object ObligationDetails {
  implicit val format: OFormat[ObligationDetails] = Json.format[ObligationDetails]
}

case class Obligation(obligations: List[ObligationDetails])

object Obligation {
  implicit val format: OFormat[Obligation] = Json.format[Obligation]
}

case class TaxResponse(id: HmrcTaxPeriod, obligation: Obligation)

object TaxResponse {
  implicit val format: OFormat[TaxResponse] = Json.format[TaxResponse]
}
