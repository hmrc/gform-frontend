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

package uk.gov.hmrc.gform.models.helpers
import uk.gov.hmrc.gform.validation.{ ComponentField, FormFieldValidationResult }
import uk.gov.hmrc.gform.views.summary.TextFormatter
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions.{ getMonthValue, renderMonth }

object TaxPeriodHelper {

  def formatTaxPeriodOutput(valResult: Option[FormFieldValidationResult]) = {
    val mapOfResults = valResult.get match { case ComponentField(a, b) => b }
    mapOfResults.keySet.toString.split('(')(2).split(',')(0)
  }

  def formatTaxPeriodOutput2(valResult: Option[FormFieldValidationResult]) = {
    val mapOfResults = valResult.get match { case ComponentField(a, b) => b }
    TextFormatter.formatText(Some(mapOfResults.values.toList(0))).drop(1)
  }

  def formatDate(date: String) = {
    val splitDate = date.split("-")
    splitDate(2) + " " + getMonthValue(Some(splitDate(1))).getOrElse("") + " " + splitDate(0)

  }

}
