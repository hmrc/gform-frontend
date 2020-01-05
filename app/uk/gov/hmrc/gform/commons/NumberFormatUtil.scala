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

package uk.gov.hmrc.gform.commons

import java.text.NumberFormat
import java.util.Locale
import java.math.RoundingMode

import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ RoundingMode => GformRoundingMode }

object NumberFormatUtil {
  val defaultFormat = NumberFormat.getInstance(Locale.UK)
  def defaultFormat(i: Int) = {
    val formatter = NumberFormat.getInstance(Locale.UK)
    formatter.setMaximumFractionDigits(i)
    formatter.setRoundingMode(RoundingMode.FLOOR)
    formatter
  }
  def roundAndFormat(bd: BigDecimal, scale: Int, roundingMode: GformRoundingMode) =
    NumberFormatUtil.defaultFormat(scale).format(NumberSetScale.setScale(bd, scale, roundingMode))
  val currencyFormat = NumberFormat.getCurrencyInstance(Locale.UK)
}

object RoundingModeBigDecimalUtil {
  def RoundingFormat(roundingMode: GformRoundingMode) =
    roundingMode match {
      case GformRoundingMode.Up       => BigDecimal.RoundingMode.UP
      case GformRoundingMode.Down     => BigDecimal.RoundingMode.DOWN
      case GformRoundingMode.Ceiling  => BigDecimal.RoundingMode.CEILING
      case GformRoundingMode.Floor    => BigDecimal.RoundingMode.FLOOR
      case GformRoundingMode.HalfDown => BigDecimal.RoundingMode.HALF_DOWN
      case GformRoundingMode.HalfUp   => BigDecimal.RoundingMode.HALF_UP
      case GformRoundingMode.HalfEven => BigDecimal.RoundingMode.HALF_EVEN
    }
}

object NumberSetScale {
  def setScale(bigDecimal: BigDecimal, decimalPlaces: Int, roundingMode: GformRoundingMode) =
    bigDecimal.setScale(decimalPlaces, RoundingModeBigDecimalUtil.RoundingFormat(roundingMode))
}
