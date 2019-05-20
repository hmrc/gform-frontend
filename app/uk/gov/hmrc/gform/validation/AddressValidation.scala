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

package uk.gov.hmrc.gform.validation
import cats.Monoid
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.validationSuccess

class AddressValidation(implicit messages: Messages, l: LangADT) {

  val cvh = new ComponentsValidatorHelper()

  def validateAddress(fieldValue: FormComponent, address: Address)(data: FormDataRecalculated): ValidatedType[Unit] = {
    val addressValueOf: String => Seq[String] = suffix => data.data.get(fieldValue.id.withSuffix(suffix)).toList.flatten

    def validateRequiredFieldSub(value: String, str: String) =
      validateRequiredField(value, str, fieldValue)(addressValueOf(value))

    def streetValidation(streetName: String) = lengthValidation(streetName, fieldValue)(addressValueOf(streetName))

    def ukStreetValidation(streetName: String) = ukLengthValidation(streetName, fieldValue)(addressValueOf(streetName))

    val combinedValidation = Monoid[ValidatedType[Unit]].combineAll(addressValueOf("uk") match {
      case "true" :: Nil =>
        List(validateRequiredFieldSub("street1", messages("ukAddress.line1.label")), ukStreetValidation("street1"))
      case _ =>
        List(
          validateRequiredFieldSub("street1", messages("internationalAddress.line1.label")),
          streetValidation("street1"))
    })

    val validatedResult: List[ValidatedType[Unit]] = addressValueOf("uk") match {
      case "true" :: Nil =>
        List(
          combinedValidation,
          ukStreetValidation("street2"),
          ukStreetValidation("street3"),
          ukStreetValidation("street4"),
          validateRequiredFieldSub("postcode", messages("ukAddress.postcode.label")),
          validateForbiddenField("country", fieldValue)(addressValueOf("country")),
          postcodeLengthValidation("postcode", fieldValue)(addressValueOf("postcode"))
        )
      case _ =>
        List(
          combinedValidation,
          streetValidation("street2"),
          streetValidation("street3"),
          streetValidation("street4"),
          validateForbiddenField("postcode", fieldValue)(addressValueOf("postcode")),
          validateRequiredFieldSub("country", messages("internationalAddress.country.label"))
        )
    }

    Monoid[ValidatedType[Unit]].combineAll(validatedResult)
  }

  private def validateRequiredField(value: String, errorPrefix: String, fieldValue: FormComponent) =
    cvh.validateRequired(fieldValue, fieldValue.id.withSuffix(value), Some(errorPrefix)) _

  private def validateForbiddenField(value: String, fieldValue: FormComponent) =
    cvh.validateForbidden(fieldValue, fieldValue.id.withSuffix(value)) _

  private def lengthValidation(value: String, fieldValue: FormComponent) =
    addressLineValidation(fieldValue, fieldValue.id.withSuffix(value)) _

  private def ukLengthValidation(value: String, fieldValue: FormComponent) =
    ukAddressLineValidation(fieldValue, fieldValue.id.withSuffix(value)) _

  private def postcodeLengthValidation(value: String, fieldValue: FormComponent) =
    postcodeValidation(fieldValue, fieldValue.id.withSuffix(value)) _

  private def addressLineValidation(fieldValue: FormComponent, fieldId: FormComponentId)(
    xs: Seq[String]): ValidatedType[Unit] = {
    def combineErrors(str: String) = Map(fieldId -> errors(fieldValue, str)).invalid
    val Fourth = "[4]$".r.unanchored
    (xs.filterNot(_.isEmpty()), fieldId.value) match {
      case (Nil, _) => validationSuccess
      case (value :: Nil, Fourth()) if value.length > ValidationValues.addressLine4 =>
        combineErrors(messages("address.line4.error.maxLength", ValidationValues.addressLine4))
      case (value :: Nil, _) if value.length > ValidationValues.addressLine =>
        combineErrors(
          messages("address.line.error.maxLength", fieldId.value.takeRight(1), ValidationValues.addressLine))
      case _ => validationSuccess
    }
  }

  private def ukAddressLineValidation(fieldValue: FormComponent, fieldId: FormComponentId)(
    xs: Seq[String]): ValidatedType[Unit] = {
    def combineErrors(str: String) = Map(fieldId -> errors(fieldValue, str)).invalid
    val First = "[1]$".r.unanchored
    val Second = "[2]$".r.unanchored
    val Third = "[3]$".r.unanchored
    val Fourth = "[4]$".r.unanchored
    (xs.filterNot(_.isEmpty()), fieldId.value) match {
      case (Nil, _) => validationSuccess
      case (value :: Nil, First()) if value.length > ValidationValues.addressLine =>
        combineErrors(messages("ukAddress.line1.error.maxLength", ValidationValues.addressLine))
      case (value :: Nil, Second()) if value.length > ValidationValues.addressLine =>
        combineErrors(messages("ukAddress.line2.error.maxLength", ValidationValues.addressLine))
      case (value :: Nil, Third()) if value.length > ValidationValues.addressLine =>
        combineErrors(messages("ukAddress.line3.error.maxLength", ValidationValues.addressLine))
      case (value :: Nil, Fourth()) if value.length > ValidationValues.addressLine4 =>
        combineErrors(messages("ukAddress.line4.error.maxLength", ValidationValues.addressLine4))
      case _ => validationSuccess
    }
  }

  private def postcodeValidation(fieldValue: FormComponent, fieldId: FormComponentId)(
    xs: Seq[String]): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty) match {
      case value :: Nil if value.length > ValidationValues.postcodeLimit =>
        Map(
          fieldId ->
            errors(fieldValue, messages("ukAddress.postcode.error.maxLength", ValidationValues.postcodeLimit))).invalid
      case _ => validationSuccess
    }
}
