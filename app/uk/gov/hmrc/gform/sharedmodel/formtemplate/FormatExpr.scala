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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.Eq
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.{ EmailVerifierService, LocalisedString }

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
  offset: OffsetDate
)

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
  implicit val format: OFormat[DateConstraintInfo] = derived.oformat
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
  case object HalfEven extends RoundingMode
  case object HalfUp extends RoundingMode
  case object HalfDown extends RoundingMode

  val defaultRoundingMode: RoundingMode = Down

  implicit val format: Format[RoundingMode] = ADTFormat.formatEnumeration(
    "Up"       -> Up,
    "Down"     -> Down,
    "Ceiling"  -> Ceiling,
    "Floor"    -> Floor,
    "HalfDown" -> HalfDown,
    "HalfEven" -> HalfEven,
    "HalfUp"   -> HalfUp
  )
}

sealed trait TextConstraint {
  val defaultSize: Int
}

final case class Number(
  maxWholeDigits: Int = TextConstraint.defaultWholeDigits,
  maxFractionalDigits: Int = TextConstraint.defaultFactionalDigits,
  roundingMode: RoundingMode = RoundingMode.defaultRoundingMode,
  unit: Option[LocalisedString] = None)
    extends TextConstraint {
  val size: Int =
    if (maxFractionalDigits > 0)
      maxWholeDigits + maxFractionalDigits + 1
    else
      maxWholeDigits

  override val defaultSize =
    if (size <= TextConstraint.defaultSizeNumber)
      size
    else
      TextConstraint.defaultSizeNumber
}

final case class PositiveNumber(
  maxWholeDigits: Int = TextConstraint.defaultWholeDigits,
  maxFractionalDigits: Int = TextConstraint.defaultFactionalDigits,
  roundingMode: RoundingMode = RoundingMode.defaultRoundingMode,
  unit: Option[LocalisedString] = None)
    extends TextConstraint {
  val size: Int =
    if (maxFractionalDigits > 0)
      maxWholeDigits + maxFractionalDigits + 1
    else
      maxWholeDigits

  override val defaultSize =
    if (size <= TextConstraint.defaultSizePositiveNumber)
      size
    else
      TextConstraint.defaultSizePositiveNumber
}

case object BasicText extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeBasicText
}

case class ShortText(min: Int, max: Int) extends TextConstraint {
  override val defaultSize = max
}

object ShortText { val default = ShortText(0, 1000) }

case class Lookup(register: Register) extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeLookup
}

case class TextWithRestrictions(min: Int, max: Int) extends TextConstraint {
  override val defaultSize = max
}

case class Sterling(roundingMode: RoundingMode, positiveOnly: Boolean) extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeSterling
}

case object UkBankAccountNumber extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeUkBankAccountNumber
}

case object UkSortCodeFormat extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeUkSortCodeFormat
}

case object SubmissionRefFormat extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeSubmissionRefFormat
}

case object TelephoneNumber extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeTelephoneNumber
  val minimumLength = 7
  val maximumLength = 25
  val phoneNumberValidation = """^[\+A-Z0-9 )/(*#-]+$""".r
}

case object Email extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeEmail
}

case class EmailVerifiedBy(formComponentId: FormComponentId, emailVerifierService: EmailVerifierService)
    extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeEmailVerifiedBy
}

case object UTR extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeUTR
}

case object NINO extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeNINO
}

case object UkVrn extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeUkVrn
}

case object CountryCode extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeCountryCode
}

case object NonUkCountryCode extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeNonUkCountryCode
}

case object CompanyRegistrationNumber extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeCompanyRegistrationNumber
}

case object EORI extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeEORI
}

case object UkEORI extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeUkEORI
}

case object ChildBenefitNumber extends TextConstraint {
  override val defaultSize = TextConstraint.defaultSizeChildBenefitNumber
}

object TextConstraint {
  val defaultWholeDigits = 11
  val defaultFactionalDigits = 2
  val defaultDisplayWidthSize = 20
  val defaultDisplayWidthXsSize = 4
  val defaultDisplayWidthSSize = 5
  val defaultDisplayWidthMSize = 10
  val defaultDisplayWidthLSize = 20
  val defaultDisplayWidthXlSize = 30
  val defaultDisplayWidthXxlSize = 40
  val defaultSizeNumber = 10
  val defaultSizePositiveNumber = 10
  val defaultSizeBasicText = 20
  val defaultSizeShortText = 20
  val defaultSizeLookup = 20
  val defaultSizeTextWithRestrictions = 20
  val defaultSizeSterling = 10
  val defaultSizeUkBankAccountNumber = 10
  val defaultSizeUkSortCodeFormat = 2
  val defaultSizeSubmissionRefFormat = 20
  val defaultSizeTelephoneNumber = 20
  val defaultSizeEmail = 30
  val defaultSizeEmailVerifiedBy = 10
  val defaultSizeUTR = 10
  val defaultSizeNINO = 20
  val defaultSizeUkVrn = 10
  val defaultSizeCountryCode = 5
  val defaultSizeNonUkCountryCode = 5
  val defaultSizeCompanyRegistrationNumber = 20
  val defaultSizeEORI = 20
  val defaultSizeUkEORI = 20
  val defaultSizeChildBenefitNumber = 20

  implicit val format: OFormat[TextConstraint] = derived.oformat[TextConstraint]

  def filterNumberValue(s: String): String = s.filterNot(c => (c == '£'))

  def getSizeForDisplayWidth(displayWidth: DisplayWidth.DisplayWidth): Int = displayWidth match {
    case DisplayWidth.XS      => TextConstraint.defaultDisplayWidthXsSize
    case DisplayWidth.S       => TextConstraint.defaultDisplayWidthSSize
    case DisplayWidth.M       => TextConstraint.defaultDisplayWidthMSize
    case DisplayWidth.L       => TextConstraint.defaultDisplayWidthLSize
    case DisplayWidth.XL      => TextConstraint.defaultDisplayWidthXlSize
    case DisplayWidth.XXL     => TextConstraint.defaultDisplayWidthXxlSize
    case DisplayWidth.DEFAULT => TextConstraint.defaultDisplayWidthSize
  }

  def getSize(constraint: TextConstraint, displayWidth: DisplayWidth.DisplayWidth): Int =
    (constraint, displayWidth) match {
      case (Number(_, _, _, _) | PositiveNumber(_, _, _, _) | ShortText(_, _) | TextWithRestrictions(_, _), _) =>
        constraint.defaultSize

      case (_, displayWidth) if displayWidth != DisplayWidth.DEFAULT =>
        getSizeForDisplayWidth(displayWidth)

      case (_, DisplayWidth.DEFAULT) =>
        constraint.defaultSize
    }
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
