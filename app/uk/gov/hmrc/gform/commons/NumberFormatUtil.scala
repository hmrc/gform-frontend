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

package uk.gov.hmrc.gform.commons

import java.text.NumberFormat
import java.util.Locale
import java.math.RoundingMode

import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ RoundingMode => GformRoundingMode }

object NumberFormatUtil {
  val defaultFormat = NumberFormat.getInstance(Locale.UK)
  def defaultFormat(i: Int, roundingMode: GformRoundingMode) =
    RoundingModeUtil.RoundingFormat(i, roundingMode)
  val currencyFormat = NumberFormat.getCurrencyInstance(Locale.UK)
}

object RoundingModeUtil {
  def RoundingFormat(digits: Int, roundingMode: GformRoundingMode) = {
    val formatter = NumberFormat.getInstance(Locale.UK)
    formatter.setMaximumFractionDigits(digits)
    roundingMode match {
      case GformRoundingMode.Up       => formatter.setRoundingMode(RoundingMode.UP)
      case GformRoundingMode.Down     => formatter.setRoundingMode(RoundingMode.DOWN)
      case GformRoundingMode.Ceiling  => formatter.setRoundingMode(RoundingMode.CEILING)
      case GformRoundingMode.Floor    => formatter.setRoundingMode(RoundingMode.FLOOR)
      case GformRoundingMode.HalfDown => formatter.setRoundingMode(RoundingMode.HALF_DOWN)
      case GformRoundingMode.HalfUp   => formatter.setRoundingMode(RoundingMode.HALF_UP)
      case GformRoundingMode.HalfEven => formatter.setRoundingMode(RoundingMode.HALF_EVEN)
    }
    formatter
  }
}
