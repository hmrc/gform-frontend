/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.validation

import javax.inject.Inject

import uk.gov.hmrc.gform.models.DeclarationFieldValidationResult
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FieldId
import uk.gov.hmrc.emailaddress.EmailAddress

class DeclarationFieldValidationService @Inject() () {

  def validateDeclarationFields(data: Map[FieldId, Seq[String]]) = {
    val result = data.map {
      case a @ (FieldId("declaration-firstname"), _) => validateNonEmpty(a, "Please provide the signer's first name")
      case a @ (FieldId("declaration-lastname"), _) => validateNonEmpty(a, "Please provide the signer's last name")
      case a @ (FieldId("declaration-status"), _) => validateNonEmpty(a, "Please provide the signer's status")
      case (FieldId("declaration-email1"), value) =>
        "declaration-email1" -> DeclarationFieldValidationResult(true, value.mkString, Seq.empty)
      case b @ (FieldId("declaration-email2"), _) => validateNonEmptyEmail(data(FieldId("declaration-email1")), b)
      case _ => "others" -> DeclarationFieldValidationResult(true, "", Seq.empty)
    }

    (!result.values.exists(!_.isValid) && !result.isEmpty, result)
  }

  private def validateNonEmptyEmail(emailValue1: Seq[String], email2: (FieldId, Seq[String])) = {
    val (emailFieldId2, emailValue2) = email2

    val error = if (emailValue1.mkString.isEmpty && emailValue2.mkString.isEmpty) {
      Seq.empty
    } else if (!emailValue1.equals(emailValue2)) {
      Seq("Email addresses don't match")
    } else if (EmailAddress.isValid(emailValue2.mkString)) {
      Seq.empty
    } else {
      Seq("Please check the email address' format")
    }

    if (error.isEmpty) {
      emailFieldId2.value -> DeclarationFieldValidationResult(true, emailValue2.mkString, Seq.empty)
    } else {
      emailFieldId2.value -> DeclarationFieldValidationResult(false, emailValue2.mkString, error)
    }
  }

  private def validateNonEmpty(tuple: (FieldId, Seq[String]), errorMsg: String) = {
    val (fieldId, value) = tuple
    val valueAsString = value.mkString

    if (valueAsString.isEmpty) {
      fieldId.value -> DeclarationFieldValidationResult(false, valueAsString, Seq(errorMsg))
    } else {
      fieldId.value -> DeclarationFieldValidationResult(true, valueAsString, Seq.empty)
    }
  }
}
