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
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ComponentsValidator.{ errors, validateForbidden, validateRequired }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.views.html.localisation
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.validationSuccess
case object AddressValidation {

  def validateAddress(fieldValue: FormComponent, address: Address)(data: FormDataRecalculated): ValidatedType[Unit] = {
    val addressValueOf: String => Seq[String] = suffix => data.data.get(fieldValue.id.withSuffix(suffix)).toList.flatten

    def validateRequiredFieldSub(value: String, str: String) =
      validateRequiredField(value, localisation(str), fieldValue)(addressValueOf(value))

    def streetValidation(streetName: String) = lengthValidation(streetName, fieldValue)(addressValueOf(streetName))

    val combinedValidation =
      Monoid[ValidatedType[Unit]].combine(validateRequiredFieldSub("street1", "line 1"), streetValidation("street1"))

    val validatedResult: List[ValidatedType[Unit]] = addressValueOf("uk") match {
      case "true" :: Nil =>
        List(
          combinedValidation,
          streetValidation("street2"),
          streetValidation("street3"),
          streetValidation("street4"),
          validateRequiredFieldSub("postcode", "postcode"),
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
          validateRequiredFieldSub("country", "Country")
        )
    }

    Monoid[ValidatedType[Unit]].combineAll(validatedResult)
  }

 private def validateRequiredField(value: String, errorPrefix: String, fieldValue: FormComponent) =
    validateRequired(fieldValue, fieldValue.id.withSuffix(value), Some(errorPrefix)) _

  private def validateForbiddenField(value: String, fieldValue: FormComponent) =
    validateForbidden(fieldValue, fieldValue.id.withSuffix(value)) _

  private def lengthValidation(value: String, fieldValue: FormComponent) =
    addressLineValidation(fieldValue, fieldValue.id.withSuffix(value)) _

  private def postcodeLengthValidation(value: String, fieldValue: FormComponent) =
    postcodeValidation(fieldValue, fieldValue.id.withSuffix(value)) _

 private def addressLineValidation(fieldValue: FormComponent, fieldId: FormComponentId)(
    xs: Seq[String]): ValidatedType[Unit] = {
    val Fourth = "[4]$".r.unanchored
    (xs.filterNot(_.isEmpty()), fieldId.value) match {
      case (Nil, _) => validationSuccess
      case (value :: Nil, Fourth()) if value.length > ValidationValues.addressLine4 =>
        Map(fieldId -> errors(fieldValue, s"line 4 is longer than ${ValidationValues.addressLine4} characters")).invalid
      case (value :: Nil, _) if value.length > ValidationValues.addressLine =>
        Map(
          fieldId -> errors(
            fieldValue,
            s"line ${fieldId.value.takeRight(1)} is longer than ${ValidationValues.addressLine} characters")).invalid
      case _ => validationSuccess
    }
  }

  private def postcodeValidation(fieldValue: FormComponent, fieldId: FormComponentId)(xs: Seq[String]): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty) match {
      case value :: Nil if value.length > ValidationValues.postcodeLimit =>
        Map(fieldId -> errors(fieldValue, s"postcode is longer than ${ValidationValues.postcodeLimit} characters")).invalid
      case _ => validationSuccess
    }
}
