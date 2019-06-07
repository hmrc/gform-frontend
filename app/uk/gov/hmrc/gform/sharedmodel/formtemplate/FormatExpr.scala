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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.Eq
import julienrf.json.derived
import play.api.libs.json._

sealed trait FormatExpr
final case class OrientationFormat(value: String) extends FormatExpr
final case class DateFormat(expressions: DateConstraintType) extends FormatExpr
final case class TextFormat(number: TextConstraint) extends FormatExpr

sealed trait ValueExpr

final case class TextExpression(expr: Expr) extends ValueExpr
final case class DateExpression(dateValue: DateValue) extends ValueExpr
final case class ChoiceExpression(selections: List[Int]) extends ValueExpr

sealed trait DateConstraintType
final case object AnyDate extends DateConstraintType
final case class DateConstraints(constraints: List[DateConstraint]) extends DateConstraintType

object DateConstraintType {
  implicit val format: OFormat[DateConstraintType] = derived.oformat[DateConstraintType]
}

final case class DateConstraint(
  beforeAfterPrecisely: BeforeAfterPrecisely,
  dateFormat: DateConstraintInfo,
  offset: OffsetDate)

object DateConstraint {
  implicit val format: OFormat[DateConstraint] = derived.oformat[DateConstraint]
}

sealed trait BeforeAfterPrecisely {
  def mkString: String = this match {
    case Before    => "before"
    case After     => "after"
    case Precisely => "exactDate"
  }
}
case object After extends BeforeAfterPrecisely
case object Before extends BeforeAfterPrecisely
case object Precisely extends BeforeAfterPrecisely

object BeforeAfterPrecisely {

  implicit val format: OFormat[BeforeAfterPrecisely] = derived.oformat[BeforeAfterPrecisely]
}

sealed trait ExactParameter
sealed trait DateParameter

sealed trait Year extends DateParameter
case object Next extends Year with ExactParameter
case object Previous extends Year with ExactParameter
case object AnyYear extends Year
case class ExactYear(year: Int) extends Year with ExactParameter

object Year {
  implicit val catsEq: Eq[Year] = Eq.fromUniversalEquals

  implicit val format: OFormat[Year] = derived.oformat[Year]
}

sealed trait Month extends DateParameter
case object AnyMonth extends Month
case class ExactMonth(month: Int) extends Month with ExactParameter

object Month {
  implicit val format: OFormat[Month] = derived.oformat[Month]
}

sealed trait Day extends DateParameter
case object AnyDay extends Day
case class ExactDay(day: Int) extends Day with ExactParameter
case object FirstDay extends Day with ExactParameter
case object LastDay extends Day with ExactParameter

object Day {
  implicit val catsEq: Eq[Day] = Eq.fromUniversalEquals

  implicit val format: OFormat[Day] = derived.oformat[Day]
}

sealed trait DateConstraintInfo
case object Today extends DateConstraintInfo

case class ConcreteDate(year: Year, month: Month, day: Day) extends DateConstraintInfo {

  val isExact: Boolean = (year, month, day) match {
    case (_: ExactParameter, _: ExactParameter, _: ExactParameter) => true
    case _                                                         => false
  }

  def getNumericParameters: List[ExactParameter] = (day :: month :: year :: Nil).collect {
    case parameter: ExactYear  => parameter
    case parameter: ExactMonth => parameter
    case parameter: ExactDay   => parameter
  }

}

object ConcreteDate {
  def apply(year: Int, month: Int, day: Int): ConcreteDate =
    ConcreteDate(ExactYear(year), ExactMonth(month), ExactDay(day))
}

case class DateField(value: FormComponentId) extends DateConstraintInfo

object DateConstraintInfo {
  implicit val format: OFormat[DateConstraintInfo] = derived.oformat[DateConstraintInfo]
}

case class OffsetDate(value: Int) extends AnyVal

object OffsetDate {
  implicit val formatExpr: OFormat[OffsetDate] = Json.format[OffsetDate]
}

sealed trait RoundingMode

object RoundingMode {
  case object Up extends RoundingMode
  case object Down extends RoundingMode
  case object Ceiling extends RoundingMode
  case object Floor extends RoundingMode
  case object HalfUp extends RoundingMode
  case object HalfDown extends RoundingMode
  case object HalfEven extends RoundingMode

  val defaultRoundingMode: RoundingMode = Down

  implicit val format: Format[RoundingMode] = ADTFormat.formatEnumeration(
    "Up"       -> Up,
    "Down"     -> Down,
    "Ceiling"  -> Ceiling,
    "Floor"    -> Floor,
    "HalfUp"   -> HalfUp,
    "HalfDown" -> HalfDown,
    "HalfEven" -> HalfEven
  )
}

sealed trait TextConstraint

final case object AnyText extends TextConstraint

final case class Number(
  maxWholeDigits: Int = TextConstraint.defaultWholeDigits,
  maxFractionalDigits: Int = TextConstraint.defaultFactionalDigits,
  roundingMode: RoundingMode = RoundingMode.defaultRoundingMode,
  unit: Option[String] = None)
    extends TextConstraint

final case class PositiveNumber(
  maxWholeDigits: Int = TextConstraint.defaultWholeDigits,
  maxFractionalDigits: Int = TextConstraint.defaultFactionalDigits,
  roundingMode: RoundingMode = RoundingMode.defaultRoundingMode,
  unit: Option[String] = None)
    extends TextConstraint

case object BasicText extends TextConstraint
case class ShortText(min: Int, max: Int) extends TextConstraint
object ShortText { val default = ShortText(0, 1000) }
case class Lookup(register: Register) extends TextConstraint
case class TextWithRestrictions(min: Int, max: Int) extends TextConstraint
final case class Sterling(roundingMode: RoundingMode) extends TextConstraint
final case object UkBankAccountNumber extends TextConstraint
final case object UkSortCodeFormat extends TextConstraint

case object TelephoneNumber extends TextConstraint {
  val minimumLength = 7
  val maximumLength = 25
  val phoneNumberValidation = """^[\+A-Z0-9 )/(*#-]+$""".r
}

case object Email extends TextConstraint
case object UTR extends TextConstraint
case object NINO extends TextConstraint
case object UkVrn extends TextConstraint
case object CountryCode extends TextConstraint
case object NonUkCountryCode extends TextConstraint
case object CompanyRegistrationNumber extends TextConstraint
case object EORI extends TextConstraint

object Sterling {
  val defaultRounding = Sterling(RoundingMode.defaultRoundingMode)
}

object TextConstraint {
  val defaultWholeDigits = 11
  val defaultFactionalDigits = 2

  implicit val format: OFormat[TextConstraint] = derived.oformat[TextConstraint]

  def filterNumberValue(s: String): String = s.filterNot(c => (c == '£'))
}

sealed trait Register {
  def asString: String = this match {
    case Register.CashType                 => "cashType"
    case Register.Country                  => "country"
    case Register.Currency                 => "currency"
    case Register.Intent                   => "intent"
    case Register.Intercept                => "intercept"
    case Register.Origin                   => "origin"
    case Register.Port                     => "port"
    case Register.TransportMode            => "transportMode"
    case Register.OriginWho                => "originWho"
    case Register.OriginMainPart           => "originMainPart"
    case Register.OriginSavingsEarnings    => "originSavingsEarnings"
    case Register.OriginSellingSomething   => "originSellingSomething"
    case Register.IntentBuyingWhat         => "intentBuyingWhat"
    case Register.IntentBigPurchase        => "intentBigPurchase"
    case Register.IntentBusiness           => "intentBusiness"
    case Register.IntentLivingCostsAndFees => "intentLivingCostsAndFees"
    case Register.IntentOther              => "intentOther"
  }
}

object Register {
  case object CashType extends Register
  case object Country extends Register
  case object Currency extends Register
  case object Intent extends Register
  case object Intercept extends Register
  case object Origin extends Register
  case object Port extends Register
  case object TransportMode extends Register
  case object OriginWho extends Register
  case object OriginMainPart extends Register
  case object OriginSavingsEarnings extends Register
  case object OriginSellingSomething extends Register
  case object IntentBuyingWhat extends Register
  case object IntentBigPurchase extends Register
  case object IntentBusiness extends Register
  case object IntentOther extends Register
  case object IntentLivingCostsAndFees extends Register

  implicit val format: OFormat[Register] = derived.oformat

  def fromString(str: String): Option[Register] = str match {
    case "cashType"                 => Some(Register.CashType)
    case "country"                  => Some(Register.Country)
    case "currency"                 => Some(Register.Currency)
    case "intent"                   => Some(Register.Intent)
    case "intercept"                => Some(Register.Intercept)
    case "origin"                   => Some(Register.Origin)
    case "port"                     => Some(Register.Port)
    case "transportMode"            => Some(Register.TransportMode)
    case "originWho"                => Some(Register.OriginWho)
    case "originSellingSomething"   => Some(Register.OriginSellingSomething)
    case "originSavingsEarnings"    => Some(Register.OriginSavingsEarnings)
    case "originMainPart"           => Some(Register.OriginMainPart)
    case "intentBuyingWhat"         => Some(Register.IntentBuyingWhat)
    case "intentBusiness"           => Some(Register.IntentBusiness)
    case "intentLivingCostsAndFees" => Some(Register.IntentLivingCostsAndFees)
    case "intentOther"              => Some(Register.IntentOther)
    case "intentBigPurchase"        => Some(Register.IntentBigPurchase)
    case _                          => None
  }
}

object TextExpression {

  //TODO: this is not the same as in origin
  implicit val format: OFormat[TextExpression] = Json.format[TextExpression]

  implicit val equal: Eq[TextExpression] = Eq.fromUniversalEquals
}
