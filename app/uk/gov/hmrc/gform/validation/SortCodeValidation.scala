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

package uk.gov.hmrc.gform.validation
import cats.Monoid
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.validationSuccess
import uk.gov.hmrc.gform.validation.ComponentValidator._

object SortCodeValidation {
  def validateSortCode[D <: DataOrigin](
    fieldValue: FormComponent
  )(
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(
    implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] =
    Monoid[ValidatedType[Unit]].combineAll {
      if (fieldValue.mandatory) {
        fieldValue.multiValueId.atomsModelComponentIds.map { modelComponentId =>
          val answer = formModelVisibilityOptics.data.one(modelComponentId)
          answer
            .filterNot(_.isEmpty())
            .fold[ValidatedType[Unit]](requiredError(fieldValue, modelComponentId)) { value =>
              checkLength(fieldValue, modelComponentId, value, 2)
            }
        }
      } else List(validationSuccess)
    }

  def checkLength(
    fieldValue: FormComponent,
    modelComponentId: ModelComponentId,
    value: String,
    desiredLength: Int
  )(
    implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] = {
    val WholeShape = s"[0-9]{$desiredLength}".r
    val FractionalShape = "([+-]?)(\\d*)[.](\\d+)".r
    value match {
      case FractionalShape(_, _, _) => lengthError(fieldValue, modelComponentId)
      case WholeShape()             => validationSuccess
      case _                        => requiredError(fieldValue, modelComponentId)
    }
  }

  private def requiredError(
    formComponent: FormComponent,
    modelComponentId: ModelComponentId
  )(
    implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] = error(formComponent, modelComponentId, "generic.error.sortcode")

  private def lengthError(
    formComponent: FormComponent,
    modelComponentId: ModelComponentId
  )(
    implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] = error(formComponent, modelComponentId, genericErrorWholeNumber)

  private def error(
    formComponent: FormComponent,
    modelComponentId: ModelComponentId,
    messageKey: String
  )(
    implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] =
    Map[ModelComponentId, Set[String]](modelComponentId -> errors(formComponent, messageKey, None, "")).invalid

}
