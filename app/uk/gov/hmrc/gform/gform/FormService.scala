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

package uk.gov.hmrc.gform.gform
import uk.gov.hmrc.gform.models.gform.{ FormComponentValidation, FormValidationOutcome }
import uk.gov.hmrc.gform.validation.{ FieldOk, FormFieldValidationResult, ValidationUtil }
import uk.gov.hmrc.gform.ops.FormComponentOps
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, IsText, Text }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

object FormService {

  def removeCommas(formValidatedData: List[FormComponentValidation]): List[FormComponentValidation] =
    formValidatedData.map(removeCommasHelper)

  private def removeCommasHelper(formComponentValidation: FormComponentValidation) = formComponentValidation match {
    case FormComponentValidation(formComponent, FieldOk(fv, cv))
        if formComponent.isSterling || formComponent.isPositiveNumber || formComponent.isNumber =>
      FormComponentValidation(formComponent, FieldOk(fv, cv.replaceAll(",", "")))
    case _ => formComponentValidation
  }

  def splitFormComponentValidation(
    optionFcv: Option[(FormComponent, FormFieldValidationResult)]): Option[FormComponentValidation] =
    optionFcv.map {
      case (formComponent, formFieldValidationResult) =>
        FormComponentValidation(formComponent, formFieldValidationResult)
    }

  def extractedValidateFormHelper(
    validationResult: List[FormComponentValidation],
    validatedType: ValidatedType[ValidationResult]): FormValidationOutcome = {
    val isFormValid =
      ValidationUtil.isFormValid(
        validationResult
          .map(formComponentValidation =>
            formComponentValidation.formComponent -> formComponentValidation.formFieldValidationResult)
          .toMap)
    val formComponents =
      if (isFormValid) toUpperCase(removeCommas(validationResult)) else validationResult

    FormValidationOutcome(
      isFormValid,
      FormData(formComponents.flatMap {
        case FormComponentValidation(_, formFieldValidationResult) => formFieldValidationResult.toFormField
      }),
      validatedType
    )
  }

  private def toUpperCase(formValidatedData: List[FormComponentValidation]): List[FormComponentValidation] =
    formValidatedData.map(isTextUpperCase)

  def isTextUpperCase(formComponentValidation: FormComponentValidation): FormComponentValidation =
    formComponentValidation.formComponent match {
      case fc if fc.isUpperCase =>
        FormComponentValidation(fc, makeUpperCase(formComponentValidation.formFieldValidationResult))
      case _ => formComponentValidation
    }

  private def makeUpperCase(ffvr: FormFieldValidationResult) = ffvr match {
    case FieldOk(fieldValue, value) => FieldOk(fieldValue, value.toUpperCase)
    case _                          => ffvr
  }
}
