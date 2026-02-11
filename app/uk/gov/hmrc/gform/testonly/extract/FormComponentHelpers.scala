/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.testonly.extract

import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Address, ChildBenefitNumber, CompanyRegistrationNumber, CountryCode, CtUTR, Date, EORI, Email, FormComponent, IsAddress, IsDate, IsOverseasAddress, IsTaxPeriodDate, IsText, IsTextArea, NINO, NonUkCountryCode, Number, OverseasAddress, PayeReference, PositiveNumber, ReferenceNumber, SaUTR, ShortText, Sterling, TelephoneNumber, Text, TextArea, TextWithRestrictions, TimeFormat, UkBankAccountNumber, UkEORI, UkSortCodeFormat, UkVrn, Value, WholeSterling, YearFormat }

object FormComponentHelpers {
  def showFormatExample(formComponent: FormComponent): String =
    formComponent match {
      case IsText(Text(TelephoneNumber, _, _, _, _, _, _))            => "07654 123456 or 0161 765 4321"
      case IsText(Text(Sterling(_, true), _, _, _, _, _, _))          => "543.21"
      case IsText(Text(Sterling(_, _), _, _, _, _, _, _))             => "543.21"
      case IsText(Text(WholeSterling(true, _), _, _, _, _, _, _))     => "321"
      case IsText(Text(WholeSterling(_, _), _, _, _, _, _, _))        => "321"
      case IsText(Text(PositiveNumber(_, 0, _, _), _, _, _, _, _, _)) => "123"
      case IsText(Text(PositiveNumber(_, _, _, _), _, _, _, _, _, _)) => "123"
      case IsText(Text(Number(_, _, _, _), _, _, _, _, _, _))         => "123"
      case IsText(Text(UkSortCodeFormat, _, _, _, _, _, _))           => "12-34-56 or 12 34 56"
      case IsText(Text(UkBankAccountNumber, _, _, _, _, _, _))        => "12345678"
      case IsText(Text(CtUTR, _, _, _, _, _, _))                      => "1234567890 or 1234567890123"
      case IsText(Text(SaUTR, _, _, _, _, _, _))                      => "1234567890 or 1234567890123"
      case IsText(Text(NINO, _, _, _, _, _, _))                       => "QQ 12 34 56 A"
      case IsText(Text(PayeReference, _, _, _, _, _, _))              => "123/AB456"
      case IsText(Text(ChildBenefitNumber, _, _, _, _, _, _))         => "CHB12345678AB"
      case IsText(Text(EORI, _, _, _, _, _, _))                       => "FR123456789000"
      case IsText(Text(UkEORI, _, _, _, _, _, _))                     => "GB123456789000"
      case IsText(Text(UkVrn, _, _, _, _, _, _))                      => "123456789 or GB123456789"
      case IsText(Text(CompanyRegistrationNumber, _, _, _, _, _, _))  => "01234567 or SC123456 or NI123456"
      case IsText(Text(Email, _, _, _, _, _, _))                      => "john.smith@email.com"
      case IsText(Text(CountryCode, _, _, _, _, _, _))                => "GB"
      case IsText(Text(NonUkCountryCode, _, _, _, _, _, _))           => "DE or FR"
      case IsText(Text(YearFormat, _, _, _, _, _, _))                 => "2025"
      case IsText(Text(TimeFormat, _, _, _, _, _, _))                 => "14:30"
      case IsDate(_)                                                  => "DD/MM/YYYY or 25/10/2024"
      case IsTaxPeriodDate()                                          => "MM/YYYY or 10/2024"
      case _                                                          => ""
    }

  def getMin(formComponent: FormComponent): Option[String] =
    formComponent match {
      case IsText(Text(ShortText(min, _), _, _, _, _, _, _))            => Some(min.toString)
      case IsText(Text(TextWithRestrictions(min, _), _, _, _, _, _, _)) => Some(min.toString)
      case IsText(Text(ReferenceNumber(min, _), _, _, _, _, _, _))      => Some(min.toString)
      case _                                                            => None
    }

  def getMax(formComponent: FormComponent): Option[String] =
    formComponent match {
      case IsText(Text(ShortText(_, max), _, _, _, _, _, _))            => Some(max.toString)
      case IsText(Text(TextWithRestrictions(_, max), _, _, _, _, _, _)) => Some(max.toString)
      case IsText(Text(ReferenceNumber(_, max), _, _, _, _, _, _))      => Some(max.toString)
      case _                                                            => None
    }

  def getValue(formComponent: FormComponent): Option[String] =
    formComponent match {
      case IsText(Text(_, Value, _, _, _, _, _))                         => None
      case IsText(Text(_, expr, _, _, _, _, _))                          => Some(expr.prettyPrint)
      case IsTextArea(TextArea(_, Value, _, _, _, _))                    => None
      case IsTextArea(TextArea(_, expr, _, _, _, _))                     => Some(expr.prettyPrint)
      case IsDate(Date(_, _, Some(dateExpr)))                            => Some(dateExpr.toString)
      case IsAddress(Address(_, _, _, Some(expr)))                       => Some(expr.prettyPrint)
      case IsOverseasAddress(OverseasAddress(_, _, _, Some(expr), _, _)) => Some(expr.prettyPrint)
      case _                                                             => None
    }

}
