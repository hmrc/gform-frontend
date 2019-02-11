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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ After, AnyDate, AnyText, AnyWord, BasicText, Before, BeforeAfterPrecisely, CompanyRegistrationNumber, ConcreteDate, CountryCode, DateConstraint, DateConstraintInfo, DateConstraintType, DateConstraints, DateField, EORI, Email, FirstDay, LastDay, NINO, NextDate, NonUkCountryCode, Number, OffsetDate, PositiveNumber, Precisely, PreviousDate, RoundingMode, ShortText, Sterling, TelephoneNumber, TextConstraint, TextExpression, TextWithRestrictions, Today, UTR, UkBankAccountNumber, UkSortCodeFormat, UkVrn }

trait FormatExprGen {
  def numberGen: Gen[Number] =
    for {
      maxWholeDigits      <- Gen.posNum[Int]
      maxFractionalDigits <- Gen.posNum[Int]
      round               <- FormatExprGen.roundingModeGen
      units               <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
    } yield Number(maxWholeDigits, maxFractionalDigits, round, units)

  def positiveNumberGen: Gen[PositiveNumber] =
    for {
      maxWholeDigits      <- Gen.posNum[Int]
      maxFractionalDigits <- Gen.posNum[Int]
      round               <- FormatExprGen.roundingModeGen
      units               <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
    } yield PositiveNumber(maxWholeDigits, maxFractionalDigits, round, units)

  def textWithRestrictions: Gen[TextWithRestrictions] =
    for {
      min <- Gen.posNum[Int]
      max <- Gen.posNum[Int]
    } yield TextWithRestrictions(min, max)

  def textConstraintGen: Gen[TextConstraint] = Gen.oneOf(
    Gen.const(AnyText),
    numberGen,
    positiveNumberGen,
    Gen.const(BasicText),
    Gen.const(ShortText),
    textWithRestrictions,
    Gen.const(Sterling.defaultRounding),
    Gen.const(UkBankAccountNumber),
    Gen.const(UkSortCodeFormat),
    Gen.const(UTR),
    Gen.const(NINO),
    Gen.const(TelephoneNumber),
    Gen.const(Email),
    Gen.const(UkVrn),
    Gen.const(CountryCode),
    Gen.const(NonUkCountryCode),
    Gen.const(CompanyRegistrationNumber),
    Gen.const(EORI)
  )

  def textExpressionGen: Gen[TextExpression] = ExprGen.exprGen().map(TextExpression(_))

  def beforeAfterPreciselyGen: Gen[BeforeAfterPrecisely] = Gen.oneOf(Before, After, Precisely)

  def concreteDateGen: Gen[ConcreteDate] =
    for {
      year  <- Gen.posNum[Int]
      month <- Gen.posNum[Int]
      day   <- Gen.posNum[Int]
    } yield ConcreteDate(year, month, day)

  def nextDateGen: Gen[NextDate] =
    for {
      month <- Gen.posNum[Int]
      day   <- Gen.posNum[Int]
    } yield NextDate(month, day)

  def previousDateGen: Gen[PreviousDate] =
    for {
      month <- Gen.posNum[Int]
      day   <- Gen.posNum[Int]
    } yield PreviousDate(month, day)

  def anyWordGen: Gen[AnyWord] = Gen.alphaNumStr.map(AnyWord)

  def dateFieldGen: Gen[DateField] = FormComponentGen.formComponentIdGen.map(DateField)

  def offsetDateGen: Gen[OffsetDate] = Gen.posNum[Int].map(OffsetDate(_))

  def dateConstraintInfoGen: Gen[DateConstraintInfo] = Gen.oneOf(
    Gen.const(Today),
    concreteDateGen,
    nextDateGen,
    previousDateGen,
    anyWordGen,
    dateFieldGen
  )

  def dateConstraintGen: Gen[DateConstraint] =
    for {
      beforeAfterPrecisely <- beforeAfterPreciselyGen
      format               <- dateConstraintInfoGen
      offset               <- offsetDateGen
    } yield DateConstraint(beforeAfterPrecisely, format, offset)

  def dateConstraintTypeGen: Gen[DateConstraintType] = Gen.oneOf(
    Gen.const(AnyDate),
    Gen.listOfN(2, dateConstraintGen).map(DateConstraints)
  )

  def roundingModeGen: Gen[RoundingMode] = Gen.oneOf(
    RoundingMode.HalfEven,
    RoundingMode.Ceiling,
    RoundingMode.Floor,
    RoundingMode.HalfUp,
    RoundingMode.HalfDown,
    RoundingMode.Down,
    RoundingMode.Up
  )
}

object FormatExprGen extends FormatExprGen
