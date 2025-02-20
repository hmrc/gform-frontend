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

package uk.gov.hmrc.gform.gform.csv

import cats.implicits.catsSyntaxEq
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve

import java.time.LocalDate
import java.time.format.DateTimeFormatter

class CsvTaxRateAdapter extends CsvDataRetrieveAdapter[TaxRate] {

  private val hmrcTaxRateDateFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")

  override val data: List[TaxRate] = CsvUtils
    .readCsvWithColumns("HMRCTaxRates.csv")
    .map { row =>
      TaxRate(
        row("TaxRegime"),
        row("Code"),
        BigDecimal(row("Rate")).setScale(2),
        LocalDate.parse(row("StartDate"), hmrcTaxRateDateFormat),
        LocalDate.parse(row("EndDate"), hmrcTaxRateDateFormat)
      )
    }

  override def search(request: DataRetrieve.Request): Option[JsValue] =
    request.json.validate[TaxRateRequest] match {
      case JsSuccess(params, _) =>
        val regimeRates: Option[List[TaxRate]] = data.groupBy(_.regime).get(params.regime)
        val response: Option[TaxRate] = regimeRates.flatMap(_.find { rate =>
          rate.code === params.code && (
            rate.startDate.isEqual(params.date) || rate.startDate.isBefore(params.date)
          ) && (
            rate.endDate.isEqual(params.date) || rate.endDate.isAfter(params.date)
          )
        })
        response.map(Json.toJson(_))
      case JsError(e) =>
        throw new Exception(
          s"An error occurred attempting to unmarshal request: ${Json.stringify(request.json)}. Error: ${Json
            .stringify(JsError.toJson(e))}"
        )
    }
}

case class TaxRate(regime: String, code: String, rate: BigDecimal, startDate: LocalDate, endDate: LocalDate)
object TaxRate {
  implicit val format: OFormat[TaxRate] = derived.oformat()
}

case class TaxRateRequest(regime: String, code: String, date: LocalDate)
object TaxRateRequest {
  implicit val format: OFormat[TaxRateRequest] = Json.format[TaxRateRequest]
}
