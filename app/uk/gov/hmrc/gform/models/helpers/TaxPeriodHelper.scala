/*
 * Copyright 2020 HM Revenue & Customs
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
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.Envelope

import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.validation.{ ComponentField, FormFieldValidationResult }
import uk.gov.hmrc.gform.views.summary.TextFormatter

object TaxPeriodHelper {

  def formatTaxPeriodOutput(
    valResult: FormFieldValidationResult,
    envelope: Envelope
  )(implicit l: LangADT, messages: Messages, evaluator: SmartStringEvaluator) =
    valResult match {
      case ComponentField(a, b) => b.values.headOption.fold("")(ffvr => TextFormatter.formatText(ffvr, envelope))
      case _                    => ""
    }

  private val dtfUser = DateTimeFormatter.ofPattern("dd MMMM yyyy")

  def formatDate(date: LocalDate) = dtfUser.format(date)

}
