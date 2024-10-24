/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.lookup

import julienrf.json.derived
import play.api.libs.json.{JsValue, Json, OFormat}
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve

import java.time.LocalDate
import java.time.format.DateTimeFormatter

class CsvTaxRateAdapter extends CsvDataRetrieveAdapter[TaxRate] {
  private val hmrcTaxRateDateFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
  private val hmrcTaxRateRequestFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("d MMMM yyyy")

  override val data: List[TaxRate] = readCsvWithColumns("HMRCTaxRates.csv")
    .map { row =>
      TaxRate(
        row("TaxRegime"),
        row("Code"),
        row("Rate"),
        LocalDate.parse(row("StartDate"), hmrcTaxRateDateFormat),
        LocalDate.parse(row("EndDate"), hmrcTaxRateDateFormat)
      )
    }

  override def search(request: DataRetrieve.Request): Option[JsValue] = {
    val taxRateRequest: TaxRateRequest = request.json.as[TaxRateRequest]
    val regimeRates: Option[List[TaxRate]] = data.groupBy(_.regime).get(taxRateRequest.regime)
    val requestedDate: LocalDate = LocalDate.parse(taxRateRequest.date, hmrcTaxRateRequestFormat)

    val searchResult: Option[List[TaxRate]] = regimeRates.map(_.filter { rate =>
      rate.code == taxRateRequest.code && (
        rate.startDate.isEqual(requestedDate) || rate.startDate.isBefore(requestedDate)
      ) && (
        rate.endDate.isEqual(requestedDate) || rate.endDate.isAfter(requestedDate)
      )
    })

    // There must be only one...
    val response: Option[TaxRate] = searchResult match {
      case Some(list) if list.size == 1 => Some(list.head)
      case _                            => None
    }

    response.map(rate => Json.toJson(rate))
  }
}

case class TaxRate(regime: String, code: String, rate: String, startDate: LocalDate, endDate: LocalDate)
object TaxRate {
  implicit val format: OFormat[TaxRate] = derived.oformat()
}

case class TaxRateRequest(regime: String, code: String, date: String)
object TaxRateRequest {
  implicit val format: OFormat[TaxRateRequest] = derived.oformat()
}
