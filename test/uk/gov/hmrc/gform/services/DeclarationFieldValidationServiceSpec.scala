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

package uk.gov.hmrc.gform.services

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FieldId
import uk.gov.hmrc.gform.validation.DeclarationFieldValidationService

class DeclarationFieldValidationServiceSpec extends Spec {

  val testService = new DeclarationFieldValidationService()

  "validateDeclarationFields()" should "fail validation if firstname, lastname and status are not provided" in {
    val (isValid, result) = testService.validateDeclarationFields(Map.empty)

    isValid shouldBe false
  }

  it should "fail validation if firstname, lastname and status are provided and empty" in {
    val mapField = Map(
      FieldId("declaration-firstname") -> Seq.empty,
      FieldId("declaration-lastname") -> Seq.empty,
      FieldId("declaration-status") -> Seq.empty
    )

    val (isValid, result) = testService.validateDeclarationFields(mapField)

    isValid shouldBe false
    result("declaration-firstname").isValid shouldBe false
    result("declaration-lastname").isValid shouldBe false
    result("declaration-status").isValid shouldBe false
  }

  it should "pass validation if firstname, lastname and status are provided" in {
    val mapField = Map(
      FieldId("declaration-firstname") -> Seq("NAME"),
      FieldId("declaration-lastname") -> Seq("SURNAME"),
      FieldId("declaration-status") -> Seq("CEO")
    )

    val (isValid, result) = testService.validateDeclarationFields(mapField)

    isValid shouldBe true
    result("declaration-firstname").isValid shouldBe true
    result("declaration-lastname").isValid shouldBe true
    result("declaration-status").isValid shouldBe true
  }

  it should "fail email validation is email and confirmation email fields are not the same" in {
    val mapField = Map(
      FieldId("declaration-firstname") -> Seq("NAME"),
      FieldId("declaration-lastname") -> Seq("SURNAME"),
      FieldId("declaration-status") -> Seq("CEO"),
      FieldId("declaration-email1") -> Seq("NOT_AN_EMAIL"),
      FieldId("declaration-email2") -> Seq("")
    )

    val (isValid, result) = testService.validateDeclarationFields(mapField)

    isValid shouldBe false
    result("declaration-email2").isValid shouldBe false
  }

  it should "pass email validation if email and confirmation email fields are empty" in {
    val mapField = Map(
      FieldId("declaration-firstname") -> Seq("NAME"),
      FieldId("declaration-lastname") -> Seq("SURNAME"),
      FieldId("declaration-status") -> Seq("CEO"),
      FieldId("declaration-email1") -> Seq(""),
      FieldId("declaration-email2") -> Seq("")
    )

    val (isValid, result) = testService.validateDeclarationFields(mapField)

    isValid shouldBe true
    result("declaration-email2").isValid shouldBe true
  }

  it should "fail email validation if email and confirmation email fields are the same but contain an invalid email address" in {
    val mapField = Map(
      FieldId("declaration-firstname") -> Seq("NAME"),
      FieldId("declaration-lastname") -> Seq("SURNAME"),
      FieldId("declaration-status") -> Seq("CEO"),
      FieldId("declaration-email1") -> Seq("NOT_AN_EMAIL_ADDRESS"),
      FieldId("declaration-email2") -> Seq("NOT_AN_EMAIL_ADDRESS")
    )

    val (isValid, result) = testService.validateDeclarationFields(mapField)

    isValid shouldBe false
    result("declaration-email2").isValid shouldBe false
  }

  it should "pass email validation if email addresses are in correct format" in {
    val mapField = Map(
      FieldId("declaration-firstname") -> Seq("NAME"),
      FieldId("declaration-lastname") -> Seq("SURNAME"),
      FieldId("declaration-status") -> Seq("CEO"),
      FieldId("declaration-email1") -> Seq("team@poseidon.org"),
      FieldId("declaration-email2") -> Seq("team@poseidon.org")
    )

    val (isValid, result) = testService.validateDeclarationFields(mapField)

    isValid shouldBe true
    result("declaration-email2").isValid shouldBe true
  }
}
