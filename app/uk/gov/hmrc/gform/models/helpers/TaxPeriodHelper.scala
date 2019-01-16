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
import java.text.SimpleDateFormat
import java.util.Date

import uk.gov.hmrc.gform.validation.{ ComponentField, FormFieldValidationResult }
import uk.gov.hmrc.gform.views.summary.TextFormatter

object TaxPeriodHelper {

  def formatTaxPeriodOutput(valResult: Option[FormFieldValidationResult]) = {
    val a = valResult match { case Some(x)      => x }
    val b = a match { case ComponentField(a, b) => b }
    TextFormatter.formatText(b.values.headOption).drop(1)
  }

  def formatDate(date: Date) =
    new SimpleDateFormat("dd MMMMM yyyy").format(date)

}
