/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluationSyntax, SmartStringEvaluator }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OptionData

object ChoiceOptions {

  def visible(
    options: List[OptionData],
    modelComponentId: ModelComponentId,
    formModelVisibilityOptics: FormModelVisibilityOptics,
    hideChoicesSelected: Boolean,
    noDuplicates: Boolean
  )(implicit messages: Messages, sse: SmartStringEvaluator): List[(OptionData, Int)] = {
    val visibleOptions = options.zipWithIndex.filter { case (option, _) =>
      option.includeIf.forall(formModelVisibilityOptics.evalIncludeIfExpr(_, None)) &&
        option.label.value().trim.nonEmpty
    }
    val optionsValueLabel = visibleOptions.map { case (option, _) =>
      option.getValue(-1, formModelVisibilityOptics) -> option.label.value()
    }
    val selectedValues = formModelVisibilityOptics.freeCalculator.variadicFormData
      .forBaseComponentId(modelComponentId.baseComponentId)
      .filterNot { case (id, _) => id == modelComponentId }
      .flatMap { case (_, value) => value.toSeq }
      .toSet

    visibleOptions.filter { case (option, _) =>
      val notAlreadySelected = !hideChoicesSelected || (option match {
        case valueBased: OptionData.ValueBased =>
          if (noDuplicates) {
            val valuesWithSameLabel = optionsValueLabel.collect {
              case (value, label) if label == valueBased.label.value() => value
            }
            !valuesWithSameLabel.exists(selectedValues.contains)
          } else {
            !selectedValues.contains(valueBased.getValue(-1, formModelVisibilityOptics))
          }
        case _: OptionData.IndexBased => true
      })
      notAlreadySelected
    }
  }
}
