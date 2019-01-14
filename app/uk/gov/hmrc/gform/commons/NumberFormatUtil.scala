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

object NumberFormatUtil {
  val defaultFormat = NumberFormat.getInstance(Locale.UK)
  def defaultFormat(i: Int, roundingMode: RoundingMode) =
    RoundingModeUtil.RoundingFormat(i, roundingMode)
  val currencyFormat = NumberFormat.getCurrencyInstance(Locale.UK)
}

object RoundingModeUtil {
  def RoundingFormat(digits: Int, roundingMode: RoundingMode) = {
    val formatter = NumberFormat.getInstance(Locale.UK)
    formatter.setMaximumFractionDigits(digits)
    roundingMode match {
      case RoundingMode.UP        => formatter.setRoundingMode(RoundingMode.UP)
      case RoundingMode.DOWN      => formatter.setRoundingMode(RoundingMode.DOWN)
      case RoundingMode.CEILING   => formatter.setRoundingMode(RoundingMode.CEILING)
      case RoundingMode.FLOOR     => formatter.setRoundingMode(RoundingMode.FLOOR)
      case RoundingMode.HALF_DOWN => formatter.setRoundingMode(RoundingMode.HALF_DOWN)
      case RoundingMode.HALF_UP   => formatter.setRoundingMode(RoundingMode.HALF_UP)
      case _                      => formatter.setRoundingMode(RoundingMode.HALF_EVEN)
    }
    formatter
  }
}
