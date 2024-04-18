/*
 * Copyright 2023 HM Revenue & Customs
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

import java.text.NumberFormat
import cats.syntax.option._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.commons.BigDecimalUtil._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, HtmlFieldId }
import uk.gov.hmrc.gform.commons.NumberFormatUtil._
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }

object TextFormatter {

  def componentTextReadonly(currentValue: String, textConstraint: TextConstraint)(implicit l: LangADT): String =
    textConstraint match {
      // format: off
      case IsPositiveNumberOrNumber(maxFractionalDigits, roundingMode, unit) => formatNumber(currentValue, maxFractionalDigits, roundingMode, unit)
      case _: Sterling                                                       => formatSterling(currentValue)
      case _: WholeSterling                                                  => stripDecimal(formatSterling(currentValue))
      case UkSortCodeFormat                                                  => formatUkSortCode(currentValue)
      case _                                                                 => currentValue
      // format: on
    }

  def componentTextForRendering(
    currentValue: String,
    textConstraint: TextConstraint,
    presentationHint: Option[List[PresentationHint]],
    editable: Boolean
  )(implicit l: LangADT): String =
    (textConstraint, presentationHint, editable) match {
      // format: off
      case (IsPositiveNumberOrNumber(_, _, _), _, true)                               => stripTrailingZeros(currentValue)
      case (IsPositiveNumberOrNumber(maxFractionalDigits, roundingMode, _), _, false) => formatNumber(currentValue, maxFractionalDigits, roundingMode, None)
      case (_: Sterling, Some(ph), _) if ph.contains(TotalValue)                      => formatSterling(stripTrailingZeros(currentValue))
      case (_: Sterling, _, true)                                                     => stripTrailingZeros(currentValue)
      case (_: Sterling, _, _)                                                        => formatSterling(stripTrailingZeros(currentValue), defaultFormat)
      case (_: WholeSterling, _, true)                                                => stripTrailingZeros(currentValue)
      case (_: WholeSterling, _, _)                                                   => stripDecimal(formatSterling(stripTrailingZeros(currentValue), defaultFormat))
      case (UkSortCodeFormat,_ ,_)                                                    => formatUkSortCode(currentValue)
      case _                                                                          => currentValue
      // format: on
    }

  def componentTextForSummary[D <: DataOrigin](
    currentValue: String,
    textConstraint: TextConstraint,
    prefix: Option[SmartString],
    suffix: Option[SmartString],
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    l: LangADT,
    sse: SmartStringEvaluator
  ): String =
    (textConstraint, prefix, suffix) match {
      // format: off
      case (IsPositiveNumberOrNumber(maxFractionalDigits, roundingMode, unit), p, s) => prependPrefix(p) + formatNumber(currentValue, maxFractionalDigits, roundingMode, s.map(_.localised(formModelVisibilityOptics.booleanExprResolver.resolve(_))).orElse(unit))
      case (_: Sterling, _, _)                                                       => formatSterling(currentValue)
      case (_: WholeSterling, _, _)                                                  => stripDecimal(formatSterling(currentValue))
      case (UkSortCodeFormat, _, _)                                                  => formatUkSortCode(currentValue)
      case (_, p, s)                                                                 => prependPrefix(p) + currentValue + appendSuffix(s)
      case _                                                                         => currentValue
      // format: on
    }

  private def stripDecimal(currentValue: String): String =
    if (currentValue.contains(".")) {
      currentValue.substring(0, currentValue.indexOf('.'))
    } else {
      currentValue
    }

  private def stripTrailingZeros(currentValue: String): String =
    if (currentValue.contains(".")) {
      currentValue.reverse.dropWhile(_ == '0').dropWhile(_ == '.').reverse
    } else currentValue

  private def prependPrefix(
    prefix: Option[SmartString]
  )(implicit
    sse: SmartStringEvaluator
  ): String =
    prefix.fold("")(_.value() + " ")

  private def appendSuffix(
    suffix: Option[SmartString]
  )(implicit
    sse: SmartStringEvaluator
  ): String =
    suffix.fold("")(" " + _.value())

  private def formatNumber(
    currentValue: String,
    maxFractionalDigits: Int,
    rm: RoundingMode,
    unit: Option[LocalisedString]
  )(implicit
    l: LangADT
  ): String = {
    val un = unit.fold("")(" " + _.value)
    val maybeBigDecimal = toBigDecimalSafe(currentValue)
    stripTrailingZeros(maybeBigDecimal.fold(currentValue)(roundAndFormat(_, maxFractionalDigits, rm))) + un
  }

  def formatNumberWithPrecise(
    currentValue: String,
    maxFractionalDigits: Int,
    rm: RoundingMode
  ): String = {
    val maybeBigDecimal = toBigDecimalSafe(currentValue)
    maybeBigDecimal.fold(currentValue)(formatWithPrecise(_, maxFractionalDigits, rm))
  }

  def formatSterling(currentValue: String, format: NumberFormat = currencyFormat): String =
    toBigDecimalSafe(currentValue).fold(currentValue)(format.format)

  private def formatUkSortCode(currentValue: String): String = currentValue.grouped(2).mkString("-")

  def formatText[D <: DataOrigin](
    validationResult: FormFieldValidationResult,
    envelope: EnvelopeWithMapping,
    prefix: Option[SmartString] = None,
    suffix: Option[SmartString] = None,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    l: LangADT,
    messages: Messages,
    evaluator: SmartStringEvaluator
  ): List[String] = {
    val currentValue = validationResult.getCurrentValue.getOrElse("")

    def getValue(formComponent: FormComponent): List[String] = formComponent match {
      case IsText(text) =>
        componentTextForSummary(currentValue, text.constraint, prefix, suffix, formModelVisibilityOptics) :: Nil
      case IsFileUpload(_)  => envelope.userFileName(formComponent) :: Nil
      case IsChoice(choice) => choice.renderToString(formComponent, validationResult, formModelVisibilityOptics)
      case IsAddress(address) =>
        Address.renderToString(formComponent, validationResult)
      case IsOverseasAddress(address) =>
        OverseasAddress.renderToString(formComponent, validationResult)
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
            monthToString(modelComponentId.atom, validationResult.getCurrentValue(HtmlFieldId.pure(modelComponentId)))
          )
          .toList
          .mkString(" ") :: Nil
      case _ => currentValue :: Nil
    }

    getValue(validationResult.formComponent)
  }

  def appendUnit(constraint: TextConstraint): Option[LocalisedString] = constraint match {
    case PositiveNumber(_, _, _, Some(unit)) => unit.some
    case Number(_, _, _, Some(unit))         => unit.some
    case _                                   => none
  }

  def isNumber(formComponent: FormComponent) = formComponent.`type` match {
    case Text(Number(_, _, _, _), _, _, _, _, _) | Text(PositiveNumber(_, _, _, _), _, _, _, _, _) => true
    case _                                                                                         => false
  }
}
