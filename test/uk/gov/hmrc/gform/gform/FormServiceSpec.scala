/*
 * Copyright 2021 HM Revenue & Customs
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

import org.scalacheck.Gen
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormComponentGen
import uk.gov.hmrc.gform.validation.{ FieldError, FieldOk, FormFieldValidationResult, ValidationResult }
import uk.gov.hmrc.gform.ops.FormComponentOps
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData

class FormServiceSpec extends Spec {

  private val genFormComponent = FormComponentGen.formComponentGen()
  private val sterling = Sterling(RoundingMode.defaultRoundingMode, false)
  private val textSterlingConstraint = Text(sterling, Expr.additionIdentity)
  private val genFormComponentSterlingConstraint: Gen[FormComponent] =
    genFormComponent.map(e => e.copy(`type` = textSterlingConstraint))

  private def headValue(formComponent: FormComponent, formFieldValidationResult: FormFieldValidationResult): String =
    new ValidationResult(
      Map(formComponent.id -> formFieldValidationResult),
      None
    ).toFormValidationOutcome(VariadicFormData.empty).formData.fields.head.value

  it should "not remove any commas from the FormFieldValidationResult when FormFieldValidationResult is not equal to FieldOk" in {
    forAll(genFormComponentSterlingConstraint) { formComponent =>
      headValue(formComponent, FieldError(formComponent, "£1,000.25", Set("someErrors"))) shouldBe "£1,000.25"
    }
  }

  it should "leave commas untouched, when FormComponent type does not equal Sterling, Number or Positive Number" in {
    forAll(genFormComponent.filterNot(x => x.isSterling || x.isNumber || x.isPositiveNumber)) { formComponent =>
      headValue(formComponent, FieldOk(formComponent, "£1,000.25")) shouldBe "£1,000.25"
    }
  }
}
