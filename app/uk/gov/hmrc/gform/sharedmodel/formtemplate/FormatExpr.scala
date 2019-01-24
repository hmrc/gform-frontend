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

sealed trait BeforeAfterPrecisely
case object After extends BeforeAfterPrecisely
case object Before extends BeforeAfterPrecisely
case object Precisely extends BeforeAfterPrecisely

object BeforeAfterPrecisely {
  implicit val format: OFormat[BeforeAfterPrecisely] = derived.oformat[BeforeAfterPrecisely]
}

sealed trait DateConstraintInfo
case object Today extends DateConstraintInfo
case class ConcreteDate(year: Int, month: Int, day: Int) extends DateConstraintInfo
case class NextDate(month: Int, day: Int) extends DateConstraintInfo
case class PreviousDate(month: Int, day: Int) extends DateConstraintInfo
case class FirstDay(year: Int, month: Int) extends DateConstraintInfo
case class LastDay(year: Int, month: Int) extends DateConstraintInfo
case class AnyWord(value: String) extends DateConstraintInfo
case class DateField(value: FormComponentId) extends DateConstraintInfo

object DateConstraintInfo {
  implicit val format: OFormat[DateConstraintInfo] = derived.oformat[DateConstraintInfo]
}

case class OffsetDate(value: Int) extends AnyVal

object OffsetDate {
  implicit val formatExpr: OFormat[OffsetDate] = Json.format[OffsetDate]
}

sealed trait TextConstraint

final case object AnyText extends TextConstraint

final case class Number(
  maxWholeDigits: Int = TextConstraint.defaultWholeDigits,
  maxFractionalDigits: Int = TextConstraint.defaultFactionalDigits,
  unit: Option[String] = None)
    extends TextConstraint

final case class PositiveNumber(
  maxWholeDigits: Int = TextConstraint.defaultWholeDigits,
  maxFractionalDigits: Int = TextConstraint.defaultFactionalDigits,
  unit: Option[String] = None)
    extends TextConstraint

case object BasicText extends TextConstraint
case object ShortText extends TextConstraint
case class TextWithRestrictions(min: Int, max: Int) extends TextConstraint
final case object Sterling extends TextConstraint
final case object UkBankAccountNumber extends TextConstraint
final case object UkSortCodeFormat extends TextConstraint
case object TelephoneNumber extends TextConstraint
case object Email extends TextConstraint
case object UTR extends TextConstraint
case object NINO extends TextConstraint
case object UkVrn extends TextConstraint
case object CountryCode extends TextConstraint
case object NonUkCountryCode extends TextConstraint
case object CompanyRegistrationNumber extends TextConstraint
case object EORI extends TextConstraint

object TextConstraint {
  val defaultWholeDigits = 11
  val defaultFactionalDigits = 2

  implicit val format: OFormat[TextConstraint] = derived.oformat[TextConstraint]

  def filterNumberValue(s: String): String = s.filterNot(c => (c == '£'))
}

object TextExpression {

  //TODO: this is not the same as in origin
  implicit val format: OFormat[TextExpression] = Json.format[TextExpression]
}
