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

  def componentTextReadonly(currentValue: String, textConstraint: TextConstraint)(implicit l: LangADT): String =
    textConstraint match {
      case PositiveNumber(_, fracDigits, rm, unit) => formatNumber(currentValue, fracDigits, rm, unit)
      case Number(_, fracDigits, rm, unit)         => formatNumber(currentValue, fracDigits, rm, unit)
      case s: Sterling                             => formatSterling(currentValue)
      case _                                       => currentValue
    }

  def componentTextEditable(currentValue: String, textConstraint: TextConstraint): String =
    textConstraint match {
      case PositiveNumber(_, _, _, _) => stripTrailingZeros(currentValue)
      case Number(_, _, _, _)         => stripTrailingZeros(currentValue)
      case _: Sterling                => stripTrailingZeros(currentValue)
      case _                          => currentValue
    }

  def componentTextForSummary(
    currentValue: String,
    textConstraint: TextConstraint,
    prefix: Option[SmartString],
    suffix: Option[SmartString])(
    implicit l: LangADT
  ): String =
    (textConstraint, prefix, suffix) match {
      // format: off
      case (IsPositiveNumberOrNumber(PositiveNumberOrNumber(_, maxFractionalDigits, roundingMode, _)), Some(p), Some(s))  =>   prependPrefix(p) + formatNumber(currentValue, maxFractionalDigits, roundingMode, Some(s.localised))
      case (IsPositiveNumberOrNumber(PositiveNumberOrNumber(_, maxFractionalDigits, roundingMode, unit)), Some(p), None)  =>   prependPrefix(p) + formatNumber(currentValue, maxFractionalDigits, roundingMode, unit)
      case (IsPositiveNumberOrNumber(PositiveNumberOrNumber(_, maxFractionalDigits, roundingMode, _)), None, Some(s))     =>   formatNumber(currentValue, maxFractionalDigits, roundingMode, Some(s.localised))
      case (IsPositiveNumberOrNumber(PositiveNumberOrNumber(_, maxFractionalDigits, roundingMode, unit)), None, None)     =>   formatNumber(currentValue, maxFractionalDigits, roundingMode, unit)
      case (_: Sterling, _, _)                                                                                            =>   formatSterling(currentValue)
      case (_, Some(p), Some(s))                                                                                          =>   prependPrefix(p) + currentValue + appendSuffix(s)
      case (_, Some(p), None)                                                                                             =>   prependPrefix(p) + currentValue
      case (_, None, Some(s))                                                                                             =>   currentValue + appendSuffix(s)
      case _                                                                                                              =>   currentValue
      // format: on
    }

  private def stripTrailingZeros(currentValue: String): String =
    if (currentValue.contains(".")) {
      currentValue.reverse.dropWhile(_ == '0').dropWhile(_ == '.').reverse
    } else currentValue

  private def stripPoundSignAndCommas(currentValue: String): String =
    "[£,]".r.replaceAllIn(currentValue, "")

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
    val sanitisedValue = stripPoundSignAndCommas(currentValue)
    val un = unit.fold("")(" " + _.value)
    val maybeBigDecimal = toBigDecimalSafe(sanitisedValue)
    stripTrailingZeros(maybeBigDecimal.fold(sanitisedValue)(roundAndFormat(_, maxFractionalDigits, rm))) + un
  }

  private def formatSterling(currentValue: String): String = {
    val sanitisedValue = stripPoundSignAndCommas(currentValue)
    toBigDecimalSafe(sanitisedValue).fold(sanitisedValue)(currencyFormat.format)
  }

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
      case IsText(text)     => componentTextForSummary(currentValue, text.constraint, prefix, suffix)
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
