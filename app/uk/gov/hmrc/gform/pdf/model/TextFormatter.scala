/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.pdf.model

import play.api.i18n.Messages
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString }
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, HtmlFieldId }
import uk.gov.hmrc.gform.views.summary.TextFormatter.componentTextForSummary

object TextFormatter {
  def formatText(
    validationResult: FormFieldValidationResult,
    envelopeWithMapping: EnvelopeWithMapping,
    prefix: Option[SmartString] = None,
    suffix: Option[SmartString] = None
  )(implicit
    l: LangADT,
    messages: Messages,
    evaluator: SmartStringEvaluator
  ): List[Html] = {
    val currentValue = validationResult.getCurrentValue.getOrElse("")

    def getValue(formComponent: FormComponent): List[String] = formComponent match {
      case IsText(text) =>
        List(componentTextForSummary(currentValue, text.constraint, prefix, suffix)).filter(_.nonEmpty)
      case IsFileUpload(_)  => List(envelopeWithMapping.userFileName(formComponent))
      case IsChoice(choice) => choice.renderToString(formComponent, validationResult)
      case IsUkSortCode(sortCode) =>
        List(
          sortCode
            .fields(formComponent.modelComponentId.indexedComponentId)
            .map(modelComponentId => validationResult.getCurrentValue(HtmlFieldId.pure(modelComponentId)))
            .toList
            .mkString("-")
        )
      case IsAddress(_) =>
        Address
          .renderToString(formComponent, validationResult)
      case IsDate(date) =>
        def monthToString(atom: Atom, s: String): String = atom match {
          case Date.month =>
            val monthValue = DateHelperFunctions.getMonthValue(s)
            messages(s"date.$monthValue")
          case _ => s
        }
        List(
          date
            .fields(formComponent.modelComponentId.indexedComponentId)
            .map(modelComponentId =>
              monthToString(modelComponentId.atom, validationResult.getCurrentValue(HtmlFieldId.pure(modelComponentId)))
            )
            .toList
            .mkString(" ")
        )
      case _ => List(currentValue)
    }

    getValue(validationResult.formComponent).map(HtmlFormat.escape(_))
  }
}
