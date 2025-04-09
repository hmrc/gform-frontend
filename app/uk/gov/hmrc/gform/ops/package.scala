/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ ChildBenefitNumber, CompanyRegistrationNumber, CtUTR, EORI, FormComponent, NINO, Number, PayeReference, PositiveNumber, ReferenceNumber, SaUTR, ShortText, Sterling, Text, TextArea, TextWithRestrictions, UkBankAccountNumber, UkEORI, UkSortCodeFormat, UkVrn, WholeSterling }

package object ops {

  implicit class FormComponentOps(formComponent: FormComponent) {
    def isSterling = formComponent.`type` match {
      case Text(_: Sterling, _, _, _, _, _, _)       => true
      case TextArea(_: Sterling, _, _, _, _, _)      => true
      case Text(_: WholeSterling, _, _, _, _, _, _)  => true
      case TextArea(_: WholeSterling, _, _, _, _, _) => true
      case _                                         => false
    }
    def isNumber = formComponent.`type` match {
      case Text(Number(_, _, _, _), _, _, _, _, _, _)  => true
      case TextArea(Number(_, _, _, _), _, _, _, _, _) => true
      case _                                           => false
    }
    def isPositiveNumber = formComponent.`type` match {
      case Text(PositiveNumber(_, _, _, _), _, _, _, _, _, _)  => true
      case TextArea(PositiveNumber(_, _, _, _), _, _, _, _, _) => true
      case _                                                   => false
    }

    def isReferenceNumber = formComponent.`type` match {
      case Text(_: ReferenceNumber, _, _, _, _, _, _)  => true
      case TextArea(_: ReferenceNumber, _, _, _, _, _) => true
      case _                                           => false
    }

    def isPayeReference = formComponent.`type` match {
      case Text(PayeReference, _, _, _, _, _, _)  => true
      case TextArea(PayeReference, _, _, _, _, _) => true
      case _                                      => false
    }

    def isUkSortCode = formComponent.`type` match {
      case Text(UkSortCodeFormat, _, _, _, _, _, _) => true
      case _                                        => false
    }

    def isUkBankAccountNumber = formComponent.`type` match {
      case Text(UkBankAccountNumber, _, _, _, _, _, _) => true
      case _                                           => false
    }

    def isNumeric = formComponent.isNumber || formComponent.isPositiveNumber || formComponent.isSterling

    def isUkVrn = formComponent.`type` match {
      case Text(UkVrn, _, _, _, _, _, _) => true
      case _                             => false
    }

    def isEORI = formComponent.`type` match {
      case Text(EORI, _, _, _, _, _, _) => true
      case _                            => false
    }

    def isUkEORI = formComponent.`type` match {
      case Text(UkEORI, _, _, _, _, _, _) => true
      case _                              => false
    }

    def isNino = formComponent.`type` match {
      case Text(NINO, _, _, _, _, _, _) => true
      case _                            => false
    }

    def isCtUtr = formComponent.`type` match {
      case Text(CtUTR, _, _, _, _, _, _) => true
      case _                             => false
    }

    def isSaUtr = formComponent.`type` match {
      case Text(SaUTR, _, _, _, _, _, _) => true
      case _                             => false
    }

    def isChildBenefitNumber = formComponent.`type` match {
      case Text(ChildBenefitNumber, _, _, _, _, _, _) => true
      case _                                          => false
    }

    def isCompanyRegistrationNumber = formComponent.`type` match {
      case Text(CompanyRegistrationNumber, _, _, _, _, _, _) => true
      case _                                                 => false
    }

    def isUTR = formComponent.`type` match {
      case Text(SaUTR, _, _, _, _, _, _) => true
      case Text(CtUTR, _, _, _, _, _, _) => true
      case _                             => false
    }

    def isText = formComponent.`type` match {
      case Text(TextWithRestrictions(_, _), _, _, _, _, _, _) => true
      case Text(ShortText(_, _), _, _, _, _, _, _)            => true
      case _                                                  => false
    }
  }
}
