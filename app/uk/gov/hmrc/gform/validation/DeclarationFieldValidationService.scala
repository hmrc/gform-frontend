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

import cats.data.Validated
import cats.instances.all._
import cats.kernel.Monoid
import cats.syntax.validated._
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class DeclarationFieldValidationService @Inject() () {

  def validateDeclarationFields(data: Map[FieldId, Seq[String]]): Validated[Map[String, Set[String]], Unit] = {
    Monoid[Validated[Map[String, Set[String]], Unit]].combineAll(Seq(
      validateFirstname(data),
      validateLastname(data),
      validateStatus(data),
      validateEmail(data)
    ))
  }

  private def validateFirstname(data: Map[FieldId, Seq[String]]) = {
    validateNonEmpty("declaration-firstname", "Please provide the signer's first name", data)
  }

  private def validateLastname(data: Map[FieldId, Seq[String]]) = {
    validateNonEmpty("declaration-lastname", "Please provide the signer's last name", data)
  }

  private def validateStatus(data: Map[FieldId, Seq[String]]) = {
    validateNonEmpty("declaration-status", "Please provide the signer's status", data)
  }

  private def validateEmail(data: Map[FieldId, Seq[String]]) = {
    val email1 = data.get(FieldId("declaration-email1"))
    val email2 = data.get(FieldId("declaration-email2"))

    (email1, email2) match {
      case (Some(val1), Some(val2)) if val1.mkString.isEmpty && val2.mkString.isEmpty => ().valid
      case (Some(val1), Some(val2)) if !val1.equals(val2) =>
        Map("declaration-email2" -> Set("Email addresses don't match")).invalid
      case (Some(_), None) | (None, Some(_)) =>
        Map("declaration-email2" -> Set("Email addresses don't match")).invalid
      case (Some(val1), Some(val2)) if !EmailAddress.isValid(val2.mkString) =>
        Map("declaration-email2" -> Set("Please check the email address' format")).invalid
      case _ => ().valid
    }
  }

  private def validateNonEmpty(fieldName: String, errorMsg: String, data: Map[FieldId, Seq[String]]): Validated[Map[String, Set[String]], Unit] = {
    data.get(FieldId(fieldName)) match {
      case Some(value) if !value.mkString.isEmpty => ().valid
      case _ => Map(fieldName -> Set(errorMsg)).invalid

    }
  }
}
