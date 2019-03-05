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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, IsText, Sterling }
import uk.gov.hmrc.gform.validation.{ FieldOk, FormFieldValidationResult }

class FormService {

  def removeCommas(formValidatedData: List[(FormComponent, FormFieldValidationResult)])
    : List[(FormComponent, FormFieldValidationResult)] =
    formValidatedData.map {
      case (formComponent, formFiledValidationR) =>
        formComponent match {
          case text
              if IsText
                .unapply(text)
                .map(e => e.constraint.isInstanceOf[Sterling])
                .getOrElse(false) =>
            (
              formComponent,
              FieldOk(formComponent, formFiledValidationR.getCurrentValue.getOrElse("").replaceAll(",", "")))
          case _ => (formComponent, formFiledValidationR)
        }
    }
}
