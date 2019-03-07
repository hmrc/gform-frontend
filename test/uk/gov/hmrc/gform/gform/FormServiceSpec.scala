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
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormComponentGen
import uk.gov.hmrc.gform.validation.{ FieldError, FieldOk }

class FormServiceSpec extends Spec {

  lazy val formService = new FormService
  lazy val genFormComponent = FormComponentGen.formComponentGen()
  lazy val genFormComponentSterlingConstraint: Gen[FormComponent] =
    genFormComponent.map(e => e.copy(`type` = textSterlingConstraint))
  lazy val textSterlingConstraint = Text(sterling, Expr.additionIdentityExpr)
  lazy val sterling = new Sterling(RoundingMode.defaultRoundingMode)

  "removeCommas" should "remove any commas from the FormFieldValidationResult when FormComponent type is an instance" +
    "of Sterling" in {
    forAll(genFormComponentSterlingConstraint) { formComponent =>
      formService
        .removeCommas(List((formComponent, FieldOk(formComponent, "1,000.25"))))
        .head
        ._2
        .getCurrentValue shouldBe Some("1000.25")
    }
  }

  it should "remove any commas from the FormFieldValidationResult when FormComponent type is an instance of Sterling" +
    " regardless of FormFieldValidationResult type" in {
    forAll(genFormComponentSterlingConstraint) { formComponent =>
      formService
        .removeCommas(List((formComponent, FieldError(formComponent, "1,000.25", Set("someErrors")))))
        .head
        ._2
        .getCurrentValue shouldBe Some("1000.25")
    }
  }

  import uk.gov.hmrc.gform.ops.FormComponentOps
  it should "when FormComponent type does not equal Sterling, commas should be untouched" in {
    forAll(genFormComponent.filterNot(_.isSterling)) { formComponent =>
      formService
        .removeCommas(List((formComponent, FieldOk(formComponent, "1,000.25"))))
        .head
        ._2
        .getCurrentValue shouldBe Some("1,000.25")
    }
  }
}
