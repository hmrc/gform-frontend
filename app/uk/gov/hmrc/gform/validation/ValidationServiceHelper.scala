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

import cats.implicits._
import play.api.i18n.Messages
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.getError

object ValidationServiceHelper {
  def getCompanionFieldComponent(formComponent: Date, formComponentList: List[FormComponent]): Option[FormComponent] =
    (for {
      dateConstraints <- formComponent.constraintType.cast[DateConstraints].toSeq
      dateConstraint  <- dateConstraints.constraints
      dateField       <- dateConstraint.dateFormat.cast[DateField].toSeq
      formComponent   <- formComponentList
      if formComponent.id === dateField.value
    } yield formComponent).headOption

  val validationSuccess = ().valid

  def validationFailure(fieldValue: FormComponent, messageKey: String, vars: Option[List[String]])(
    implicit l: LangADT,
    messages: Messages) =
    getError(fieldValue, messageKey, vars)
}
