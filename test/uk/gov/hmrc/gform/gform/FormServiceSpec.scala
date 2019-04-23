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

import org.scalacheck.Gen
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.models.gform.FormComponentValidation
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormComponentGen
import uk.gov.hmrc.gform.validation.{ FieldError, FieldOk }
import uk.gov.hmrc.gform.ops.FormComponentOps

class FormServiceSpec extends Spec {

  private val genFormComponent = FormComponentGen.formComponentGen()
  private val sterling = Sterling(RoundingMode.defaultRoundingMode)
  private val textSterlingConstraint = Text(sterling, Expr.additionIdentityExpr)
  private val genFormComponentSterlingConstraint: Gen[FormComponent] =
    genFormComponent.map(e => e.copy(`type` = textSterlingConstraint))

  private val number = Number()
  private val textNumberConstraint = Text(number, Expr.additionIdentityExpr)
  private val genFormComponentNumberConstraint: Gen[FormComponent] =
    genFormComponent.map(e => e.copy(`type` = textNumberConstraint))

  private val positiveNumber = PositiveNumber()
  private val textPositiveNumberConstraint = Text(positiveNumber, Expr.additionIdentityExpr)
  private val genFormComponentPNConstraint: Gen[FormComponent] =
    genFormComponent.map(e => e.copy(`type` = textPositiveNumberConstraint))

  "removeCommas" should "remove any commas from the FormFieldValidationResult when FormComponent type is of type Sterling" +
    "of Sterling" in {
    forAll(genFormComponentSterlingConstraint) { formComponent =>
      FormService
        .removeCommas(List(FormComponentValidation(formComponent, FieldOk(formComponent, "1000.25"))))
        .head
        .formFieldValidationResult
        .getCurrentValue shouldBe Some("1000.25")
    }
  }

  it should "not remove any commas from the FormFieldValidationResult when FormFieldValidationResult is not equal to FieldOk" in {
    forAll(genFormComponentSterlingConstraint) { formComponent =>
      FormService
        .removeCommas(
          List(FormComponentValidation(formComponent, FieldError(formComponent, "1,000.25", Set("someErrors")))))
        .head
        .formFieldValidationResult
        .getCurrentValue shouldBe Some("1,000.25")
    }
  }

  it should "remove any commas from the FormFieldValidationResult when FormComponent type is Number" in {
    forAll(genFormComponentPNConstraint) { formComponent =>
      FormService
        .removeCommas(List(FormComponentValidation(formComponent, FieldOk(formComponent, "1,000,000"))))
        .head
        .formFieldValidationResult
        .getCurrentValue shouldBe Some("1000000")
    }
  }

  it should "remove any commas from the FormFieldValidationResult when FormComponent type is PositiveNumber" in {
    forAll(genFormComponentNumberConstraint) { formComponent =>
      FormService
        .removeCommas(List(FormComponentValidation(formComponent, FieldOk(formComponent, "1,000,000"))))
        .head
        .formFieldValidationResult
        .getCurrentValue shouldBe Some("1000000")
    }
  }

  it should "leave commas untouched, when FormComponent type does not equal Sterling, Number or Positive Number" in {
    forAll(genFormComponent.filterNot(x => x.isSterling || x.isNumber || x.isPositiveNumber)) { formComponent =>
      FormService
        .removeCommas(List(FormComponentValidation(formComponent, FieldOk(formComponent, "1,000.25"))))
        .head
        .formFieldValidationResult
        .getCurrentValue shouldBe Some("1,000.25")
    }
  }
}
