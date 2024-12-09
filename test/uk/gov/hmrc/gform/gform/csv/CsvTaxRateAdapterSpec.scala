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

import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.gform.csv.TaxRateRequest._
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve

import java.time.LocalDate

class CsvTaxRateAdapterSpec extends Spec {

  val csvTaxRateAdapter: CsvTaxRateAdapter = new CsvTaxRateAdapter()
  val request: TaxRateRequest = TaxRateRequest("APD", "BANDB-RDCD", LocalDate.parse("2023-10-25"))
  val response: TaxRate = TaxRate(
    "APD",
    "BANDB-RDCD",
    BigDecimal("87").setScale(2),
    LocalDate.parse("2023-04-01"),
    LocalDate.parse("2024-03-31")
  )

  "CsvTaxRateAdapter" should "have loaded all rates in the hmrcTaxRate file" in {
    csvTaxRateAdapter.data.nonEmpty
  }

  it should "find an exact rate for" in {
    val dataRetrieveRequest: DataRetrieve.Request = DataRetrieve.Request(Json.toJson(request), List.empty)
    val jsResponse: JsValue = Json.toJson(response)
    csvTaxRateAdapter.search(dataRetrieveRequest) shouldBe Some(jsResponse)
  }

  it should "not find a rate for" in {
    val request: TaxRateRequest = TaxRateRequest("APD", "NOT-EXISTS", LocalDate.parse("2023-10-25"))
    val dataRetrieveRequest: DataRetrieve.Request = DataRetrieve.Request(Json.toJson(request), List.empty)
    csvTaxRateAdapter.search(dataRetrieveRequest) shouldBe None
  }

}
