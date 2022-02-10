/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, PostcodeLookup }
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

class PostcodeLookupValidation[D <: DataOrigin](formModelVisibilityOptics: FormModelVisibilityOptics[D])(implicit
  messages: Messages,
  sse: SmartStringEvaluator
) {

  def validate(formComponent: FormComponent): ValidatedType[Unit] =
    validateRequired(formComponent)

  private def validateRequired(
    formComponent: FormComponent
  ): ValidatedType[Unit] = {

    case class ModelComponentIdValue(modelComponentId: ModelComponentId, value: Option[String])

    val atomsWithValues: List[ModelComponentIdValue] = formComponent.multiValueId
      .atomsModelComponentIdsFilterByAtom(_.atom =!= PostcodeLookup.filter)
      .map(m => ModelComponentIdValue(m, formModelVisibilityOptics.data.one(m)))

    val validatedResult = if (formComponent.mandatory) {
      atomsWithValues.map { mcv =>
        mcv.value
          .filter(_.nonEmpty)
          .fold(requiredError(formComponent, mcv.modelComponentId))(_ => validationSuccess)
      }
    } else {
      List(validationSuccess)
    }
    Monoid[ValidatedType[Unit]].combineAll(validatedResult)
  }

  private def requiredError(formComponent: FormComponent, modelComponentId: ModelComponentId): ValidatedType[Unit] =
    Map[ModelComponentId, Set[String]](
      modelComponentId -> errors(formComponent, "field.error.required", None)
    ).invalid

}
