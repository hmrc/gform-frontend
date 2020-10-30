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

package uk.gov.hmrc.gform.views.summary

import cats.syntax.option._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.commons.BigDecimalUtil.toBigDecimalSafe
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions
import uk.gov.hmrc.gform.sharedmodel.LocalisedString
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, HtmlFieldId }
import uk.gov.hmrc.gform.commons.NumberFormatUtil._
import uk.gov.hmrc.gform.sharedmodel.LangADT

object TextFormatter {

  def componentTextReadonly(currentValue: String, textConstraint: TextConstraint)(implicit l: LangADT): String =
    textConstraint match {
      case PositiveNumber(_, fracDigits, rm, unit) => formatNumber(currentValue, fracDigits, rm, unit)
      case Number(_, fracDigits, rm, unit)         => formatNumber(currentValue, fracDigits, rm, unit)
      case s: Sterling                             => formatSterling(currentValue)
      case _                                       => currentValue
    }

  def componentTextEditable(currentValue: String, textConstraint: TextConstraint): String =
    textConstraint match {
      case PositiveNumber(_, _, _, unit) => stripTrailingZeros(currentValue)
      case Number(_, _, _, unit)         => stripTrailingZeros(currentValue)
      case s: Sterling                   => stripTrailingZeros(currentValue)
      case _                             => currentValue
    }

  private def stripTrailingZeros(currentValue: String): String =
    if (currentValue.contains(".")) {
      currentValue.reverse.dropWhile(_ == '0').dropWhile(_ == '.').reverse
    } else currentValue

  private def formatNumber(
    currentValue: String,
    maxFractionalDigits: Int,
    rm: RoundingMode,
    unit: Option[LocalisedString]
  )(
    implicit l: LangADT
  ): String = {
    val un = unit.fold("")(" " + _.value)
    val maybeBigDecimal = toBigDecimalSafe(currentValue)
    stripTrailingZeros(maybeBigDecimal.fold(currentValue)(roundAndFormat(_, maxFractionalDigits, rm))) + un
  }

  private def formatSterling(currentValue: String): String =
    toBigDecimalSafe(currentValue).fold(currentValue)(currencyFormat.format)

  def formatText(
    validationResult: FormFieldValidationResult,
    envelope: Envelope
  )(
    implicit l: LangADT,
    messages: Messages,
    evaluator: SmartStringEvaluator
  ): String = {
    val currentValue = validationResult.getCurrentValue.getOrElse("")

    def getValue(formComponent: FormComponent): String = formComponent match {
      case IsText(text)     => componentTextReadonly(currentValue, text.constraint)
      case IsFileUpload()   => envelope.userFileName(formComponent)
      case IsChoice(choice) => choice.renderToString(formComponent, validationResult).mkString("<br>")
      case IsUkSortCode(sortCode) =>
        sortCode
          .fields(formComponent.modelComponentId.indexedComponentId)
          .map(modelComponentId => validationResult.getCurrentValue(HtmlFieldId.pure(modelComponentId)))
          .toList
          .mkString("-")
      case IsAddress(address) =>
        Address
          .renderToString(formComponent, validationResult)
          .mkString("<br>")
      case IsDate(date) =>
        def monthToString(atom: Atom, s: String): String = atom match {
          case Date.month =>
            val monthValue = DateHelperFunctions.getMonthValue(s)
            messages(s"date.$monthValue")
          case _ => s
        }
        date
          .fields(formComponent.modelComponentId.indexedComponentId)
          .map(modelComponentId =>
            monthToString(modelComponentId.atom, validationResult.getCurrentValue(HtmlFieldId.pure(modelComponentId))))
          .toList
          .mkString(" ")
      case _ => currentValue
    }

    getValue(validationResult.formComponent)
  }

  def appendUnit(constraint: TextConstraint)(implicit l: LangADT): Option[String] = constraint match {
    case PositiveNumber(_, _, _, Some(unit)) => unit.value.some
    case Number(_, _, _, Some(unit))         => unit.value.some
    case _                                   => none
  }

  def isNumber(formComponent: FormComponent) = formComponent.`type` match {
    case Text(Number(_, _, _, _), _, _, _) | Text(PositiveNumber(_, _, _, _), _, _, _) => true
    case _                                                                             => false
  }
}
