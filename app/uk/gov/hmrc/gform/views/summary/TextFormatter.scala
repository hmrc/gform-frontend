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

package uk.gov.hmrc.gform.views.summary

import cats.syntax.option._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.commons.BigDecimalUtil._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, HtmlFieldId }
import uk.gov.hmrc.gform.commons.NumberFormatUtil._

object TextFormatter {

  def componentTextReadonly(
    currentValue: String,
    textConstraint: TextConstraint,
    prefix: Option[SmartString] = None,
    suffix: Option[SmartString] = None)(
    implicit l: LangADT
  ): String =
    (textConstraint, prefix, suffix) match {
      // format: off
      case (PositiveNumber(_, fracDigits, rm, _), Some(p), Some(s))  =>   prependPrefix(p) + formatNumber(currentValue, fracDigits, rm, Some(s.localised))
      case (PositiveNumber(_, fracDigits, rm, unit), Some(p), None)  =>   prependPrefix(p) + formatNumber(currentValue, fracDigits, rm, unit)
      case (PositiveNumber(_, fracDigits, rm, _), None, Some(s))     =>   formatNumber(currentValue, fracDigits, rm, Some(s.localised))
      case (PositiveNumber(_, fracDigits, rm, unit), None, None)     =>   formatNumber(currentValue, fracDigits, rm, unit)
      case (Number(_, fracDigits, rm, _), Some(p), Some(s))          =>   prependPrefix(p) + formatNumber(currentValue, fracDigits, rm, Some(s.localised))
      case (Number(_, fracDigits, rm, unit), Some(p), None)          =>   prependPrefix(p) + formatNumber(currentValue, fracDigits, rm, unit)
      case (Number(_, fracDigits, rm, _), None, Some(s))             =>   formatNumber(currentValue, fracDigits, rm, Some(s.localised))
      case (Number(_, fracDigits, rm, unit), None, None)             =>   formatNumber(currentValue, fracDigits, rm, unit)
      case (_: Sterling, Some(p), Some(s))                           =>   prependPrefix(p) + currentValue + appendSuffix(s)
      case (_: Sterling, Some(p), None)                              =>   prependPrefix(p) + currentValue
      case (_: Sterling, None, Some(s))                              =>   formatSterling(currentValue) + appendSuffix(s)
      case (_: Sterling, None, None)                                 =>   formatSterling(currentValue)
      case (_, Some(p), Some(s))                                     =>   prependPrefix(p) + currentValue + appendSuffix(s)
      case (_, Some(p), None)                                        =>   prependPrefix(p) + currentValue
      case (_, None, Some(s))                                        =>   currentValue + appendSuffix(s)
      case _                                                         =>   currentValue
      // format: on
    }

  def componentTextEditable(currentValue: String, textConstraint: TextConstraint): String =
    textConstraint match {
      case PositiveNumber(_, _, _, _) => stripTrailingZeros(currentValue)
      case Number(_, _, _, _)         => stripTrailingZeros(currentValue)
      case _: Sterling                => stripTrailingZeros(currentValue)
      case _                          => currentValue
    }

  private def stripTrailingZeros(currentValue: String): String =
    if (currentValue.contains(".")) {
      currentValue.reverse.dropWhile(_ == '0').dropWhile(_ == '.').reverse
    } else currentValue

  private def prependPrefix(
    prefix: SmartString
  )(
    implicit l: LangADT
  ): String =
    prefix.localised.value + " "

  private def appendSuffix(
    suffix: SmartString
  )(
    implicit l: LangADT
  ): String =
    " " + suffix.localised.value

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
    envelope: Envelope,
    prefix: Option[SmartString] = None,
    suffix: Option[SmartString] = None
  )(
    implicit l: LangADT,
    messages: Messages,
    evaluator: SmartStringEvaluator
  ): String = {
    val currentValue = validationResult.getCurrentValue.getOrElse("")

    def getValue(formComponent: FormComponent): String = formComponent match {
      case IsText(text)     => componentTextReadonly(currentValue, text.constraint, prefix, suffix)
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
    case Text(Number(_, _, _, _), _, _, _, _, _) | Text(PositiveNumber(_, _, _, _), _, _, _, _, _) => true
    case _                                                                                         => false
  }
}
