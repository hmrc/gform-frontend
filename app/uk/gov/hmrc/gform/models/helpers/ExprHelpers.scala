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

import uk.gov.hmrc.gform.sharedmodel.formtemplate._

case class MaxDigitsAndRoundingMode(maxDigits: Int, roundingMode: RoundingMode)

object HasDigits {
  def unapply(expr: ComponentType): Option[MaxDigitsAndRoundingMode] =
    expr match {
      case Text(Number(_, digits, rm, _), _, _, _)          => Some(MaxDigitsAndRoundingMode(digits, rm))
      case Text(PositiveNumber(_, digits, rm, _), _, _, _)  => Some(MaxDigitsAndRoundingMode(digits, rm))
      case TextArea(Number(_, digits, rm, _), _, _)         => Some(MaxDigitsAndRoundingMode(digits, rm))
      case TextArea(PositiveNumber(_, digits, rm, _), _, _) => Some(MaxDigitsAndRoundingMode(digits, rm))
      case _                                                => None
    }
}

object HasSterling {
  def unapply(expr: ComponentType): Option[MaxDigitsAndRoundingMode] =
    expr match {
      case Text(s: Sterling, _, _, _)  => Some(MaxDigitsAndRoundingMode(2, s.roundingMode))
      case TextArea(s: Sterling, _, _) => Some(MaxDigitsAndRoundingMode(2, s.roundingMode))
      case _                           => None
    }
}

object FormComponentHelper {
  def extractMaxFractionalDigits(fc: FormComponent): MaxDigitsAndRoundingMode = fc.`type` match {
    case HasDigits(digits)   => digits
    case HasSterling(digits) => digits
    case _                   => MaxDigitsAndRoundingMode(TextConstraint.defaultFractionalDigits, RoundingMode.defaultRoundingMode)
  }
}
