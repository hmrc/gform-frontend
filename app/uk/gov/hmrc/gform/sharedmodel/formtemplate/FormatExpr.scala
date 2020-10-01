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
import cats.instances.int._
import cats.syntax.eq._
import java.time.LocalDate
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
    case Precisely => "precisely"
  }

  val intPredicate: (Int, Int) => Boolean = this match {
    case Before    => _ < _
    case After     => _ > _
    case Precisely => _ === _
  }

  val datePredicate: (LocalDate, LocalDate) => Boolean = this match {
    case Before    => _ isBefore _
    case After     => _ isAfter _
    case Precisely => _ isEqual _
  }
}
case object After extends BeforeAfterPrecisely
case object Before extends BeforeAfterPrecisely
case object Precisely extends BeforeAfterPrecisely

object BeforeAfterPrecisely {
  implicit val catsEq: Eq[BeforeAfterPrecisely] = Eq.fromUniversalEquals
  implicit val format: OFormat[BeforeAfterPrecisely] = derived.oformat[BeforeAfterPrecisely]
}

sealed trait Year extends Product with Serializable

object Year {

  case object Next extends Year
  case object Previous extends Year
  case object Any extends Year
  case class Exact(year: Int) extends Year

  implicit val catsEq: Eq[Year] = Eq.fromUniversalEquals
  implicit val format: OFormat[Year] = derived.oformat[Year]
}

sealed trait Month extends Product with Serializable {
  def isAny: Boolean = this === Month.Any
}

object Month {

  case object Any extends Month
  case class Exact(month: Int) extends Month

  implicit val catsEq: Eq[Month] = Eq.fromUniversalEquals
  implicit val format: OFormat[Month] = derived.oformat[Month]
}

sealed trait Day extends Product with Serializable {
  // format: off
  def isAny: Boolean   = fold0(true)(false)(false)(false)
  def isExact: Boolean = fold0(false)(true)(false)(false)
  def isFirst: Boolean = fold0(false)(false)(true)(false)
  def isLast: Boolean =  fold0(false)(false)(false)(true)
  // format: on

  private def fold0[B](f: B)(g: B)(h: B)(i: => B): B = fold(_ => f)(_ => g)(_ => h)(_ => i)

  def fold[B](f: Day.Any.type => B)(g: Day.Exact => B)(h: Day.First.type => B)(i: Day.Last.type => B): B = this match {
    case x: Day.Any.type   => f(x)
    case x: Day.Exact      => g(x)
    case x: Day.First.type => h(x)
    case x: Day.Last.type  => i(x)
  }
}

object Day {

  case object Any extends Day
  case class Exact(day: Int) extends Day
  case object First extends Day
  case object Last extends Day

  implicit val catsEq: Eq[Day] = Eq.fromUniversalEquals
  implicit val format: OFormat[Day] = derived.oformat[Day]
}

sealed trait DateConstraintInfo
case object Today extends DateConstraintInfo
case class ConcreteDate(year: Year, month: Month, day: Day) extends DateConstraintInfo
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
  val defaultCssClassName: String = this match {
    case Number(maxWholeDigits, maxFractionalDigits, _, _) =>
      deriveCssClassNameForNumber(maxWholeDigits, maxFractionalDigits)
    case PositiveNumber(maxWholeDigits, maxFractionalDigits, _, _) =>
      deriveCssClassNameForNumber(maxWholeDigits, maxFractionalDigits)
    case BasicText                    => CssClassSize._20
    case ShortText(_, max)            => deriveCssClassNameForText(max)
    case Lookup(_)                    => CssClassSize._30
    case TextWithRestrictions(_, max) => deriveCssClassNameForText(max)
    case Sterling(_, _)               => CssClassSize._10
    case UkBankAccountNumber          => CssClassSize._10
    case UkSortCodeFormat             => CssClassSize._2
    case SubmissionRefFormat          => CssClassSize._20
    case TelephoneNumber              => CssClassSize._10
    case Email                        => CssClassSize._30
    case EmailVerifiedBy(_, _)        => CssClassSize._10
    case UTR                          => CssClassSize._10
    case NINO                         => CssClassSize._10
    case UkVrn                        => CssClassSize._10
    case CountryCode                  => CssClassSize._5
    case NonUkCountryCode             => CssClassSize._5
    case CompanyRegistrationNumber    => CssClassSize._20
    case EORI                         => CssClassSize._20
    case UkEORI                       => CssClassSize._20
    case ChildBenefitNumber           => CssClassSize._10
  }

  private def deriveCssClassNameForText(n: Int): String = n match {
    case n if n <= 2            => CssClassSize._2
    case 3                      => CssClassSize._3
    case 4                      => CssClassSize._4
    case 5                      => CssClassSize._5
    case n if n > 5 && n <= 10  => CssClassSize._10
    case n if n > 10 && n <= 20 => CssClassSize._20
    case n if n > 20 && n <= 30 => CssClassSize._30
    case n if n > 30            => CssClassSize._40
  }

  private def deriveCssClassNameForNumber(maxWholeDigits: Int, maxFractionalDigits: Int): String = {
    val size =
      if (maxFractionalDigits > 0)
        maxWholeDigits + maxFractionalDigits + 1
      else
        maxWholeDigits

    size match {
      case n if n <= 2            => CssClassSize._2
      case 3                      => CssClassSize._3
      case 4                      => CssClassSize._4
      case 5                      => CssClassSize._5
      case n if n > 5 && n <= 15  => CssClassSize._10
      case n if n > 15 && n <= 25 => CssClassSize._20
      case n if n > 25            => CssClassSize._30
    }
  }
}

final case class Number(
  maxWholeDigits: Int = TextConstraint.defaultWholeDigits,
  maxFractionalDigits: Int = TextConstraint.defaultFactionalDigits,
  roundingMode: RoundingMode = RoundingMode.defaultRoundingMode,
  unit: Option[LocalisedString] = None)
    extends TextConstraint

final case class PositiveNumber(
  maxWholeDigits: Int = TextConstraint.defaultWholeDigits,
  maxFractionalDigits: Int = TextConstraint.defaultFactionalDigits,
  roundingMode: RoundingMode = RoundingMode.defaultRoundingMode,
  unit: Option[LocalisedString] = None)
    extends TextConstraint

case object BasicText extends TextConstraint
case class ShortText(min: Int, max: Int) extends TextConstraint
object ShortText { val default = ShortText(0, 1000) }
case class Lookup(register: Register) extends TextConstraint
case class TextWithRestrictions(min: Int, max: Int) extends TextConstraint
case class Sterling(roundingMode: RoundingMode, positiveOnly: Boolean) extends TextConstraint
case object UkBankAccountNumber extends TextConstraint
case object UkSortCodeFormat extends TextConstraint
case object SubmissionRefFormat extends TextConstraint

case object TelephoneNumber extends TextConstraint {
  val minimumLength = 7
  val maximumLength = 25
  val phoneNumberValidation = """^[\+A-Z0-9 )/(*#-]+$""".r
}

case object Email extends TextConstraint
case class EmailVerifiedBy(formComponentId: FormComponentId, emailVerifierService: EmailVerifierService)
    extends TextConstraint
case object UTR extends TextConstraint
case object NINO extends TextConstraint
case object UkVrn extends TextConstraint
case object CountryCode extends TextConstraint
case object NonUkCountryCode extends TextConstraint
case object CompanyRegistrationNumber extends TextConstraint
case object EORI extends TextConstraint
case object UkEORI extends TextConstraint
case object ChildBenefitNumber extends TextConstraint

object TextConstraint {
  val defaultWholeDigits = 11
  val defaultFactionalDigits = 2

  implicit val format: OFormat[TextConstraint] = derived.oformat[TextConstraint]

  def filterNumberValue(s: String): String = s.filterNot(c => (c == '£'))

  private def getSizeClassForDisplayWidthForText(displayWidth: DisplayWidth.DisplayWidth): String = displayWidth match {
    case DisplayWidth.XS  => CssClassSize._4
    case DisplayWidth.S   => CssClassSize._5
    case DisplayWidth.M   => CssClassSize._10
    case DisplayWidth.L   => CssClassSize._20
    case DisplayWidth.XL  => CssClassSize._30
    case DisplayWidth.XXL => CssClassSize._40
  }

  private def getSizeClassForDisplayWidthForNumber(
    constraint: TextConstraint,
    displayWidth: DisplayWidth.DisplayWidth): String =
    (constraint, displayWidth) match {
      case (Sterling(_, _), DisplayWidth.XS) => CssClassSize._3
      case (_, DisplayWidth.XS)              => CssClassSize._2
      case (_, DisplayWidth.S)               => CssClassSize._3
      case (_, DisplayWidth.M)               => CssClassSize._4
      case (_, DisplayWidth.L)               => CssClassSize._5
      case (_, DisplayWidth.XL)              => CssClassSize._10
      case (_, DisplayWidth.XXL)             => CssClassSize._20
      case _                                 => CssClassSize._10
    }

  def getSizeClass(constraint: TextConstraint, displayWidth: DisplayWidth.DisplayWidth): String =
    (constraint, displayWidth) match {
      case (_, DisplayWidth.DEFAULT) =>
        constraint.defaultCssClassName

      case (
          Number(_, _, _, _) | PositiveNumber(_, _, _, _) | Sterling(_, _) | UkBankAccountNumber | UkSortCodeFormat,
          displayWidth) =>
        getSizeClassForDisplayWidthForNumber(constraint, displayWidth)

      case (_, displayWidth) =>
        getSizeClassForDisplayWidthForText(displayWidth)
    }
}

object CssClassSize {
  val _2 = "govuk-input--width-2"
  val _3 = "govuk-input--width-3"
  val _4 = "govuk-input--width-4"
  val _5 = "govuk-input--width-5"
  val _10 = "govuk-input--width-10"
  val _20 = "govuk-input--width-20"
  val _30 = "govuk-input--width-30"
  val _40 = "govuk-input--width-40"
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
