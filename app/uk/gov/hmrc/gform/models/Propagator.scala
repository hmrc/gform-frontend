/*
 * Copyright 2025 HM Revenue & Customs
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

import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormCtx, IsChoice, OptionData, OptionDataValue }
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData, VariadicValue }

class Propagator private (
  baseComponentIds: Set[BaseComponentId], // If some of these fields value changes...

  // {
  //   "id": "whoFor",
  //   "type": "choice",
  //   "choices": [
  //     {
  //       "en": "${name}",
  //       "value": "${name}"
  //     },
  //     {
  //       "en": "${cake}",
  //       "value": "${cake}"
  //     }
  //   ]
  // }
  // For example above will be represented as Map(whoFor -> Set(name, cake))
  refsInChoices: Map[ModelComponentId, Set[BaseComponentId]] // ... we need to change corresponding values in choices
) {
  private def needPropagation(modelComponentId: ModelComponentId): Boolean = baseComponentIds(
    modelComponentId.baseComponentId
  )

  private def ensurePropagateIsNeeded(
    baseComponentId: BaseComponentId,
    oldValue: Option[VariadicValue],
    newValue: VariadicValue
  ): Map[ModelComponentId, (Option[VariadicValue], VariadicValue)] =
    if (oldValue.contains(newValue)) {
      Map.empty
    } else {
      refsInChoices.collect {
        case (modelComponentId, baseComponentsId) if baseComponentsId(baseComponentId) =>
          (modelComponentId -> (oldValue, newValue))
      }
    }

  def propagate(
    variadicFormData: VariadicFormData[SourceOrigin.OutOfDate],
    recData: RecData[SourceOrigin.Current]
  ): VariadicFormData[SourceOrigin.OutOfDate] = {
    val updatedChoices: Iterable[Map[ModelComponentId, VariadicValue.Many]] = variadicFormData.data.collect {
      case (modelComponentId, newValue) =>
        if (needPropagation(modelComponentId)) {
          val oldVariadicFormData = recData.variadicFormData
          val oldValue: Option[VariadicValue] = oldVariadicFormData.get(modelComponentId)

          val needToPropagate = ensurePropagateIsNeeded(modelComponentId.baseComponentId, oldValue, newValue)

          needToPropagate.flatMap { case (modelComponentIdToChange, (oldValue, newValue)) =>
            val realOldValue = oldVariadicFormData.get(modelComponentIdToChange)

            oldValue match {
              case Some(VariadicValue.One(value)) =>
                realOldValue match {
                  case Some(VariadicValue.Many(values)) =>
                    // Note that option's value needs to be prefixed so we know to which component it belongs to
                    val prefix = modelComponentId.toMongoIdentifier + "_"
                    val prefixedValue = prefix + value

                    if (values.contains(prefixedValue)) {
                      newValue match {
                        case VariadicValue.One(nv) =>
                          Some(
                            modelComponentIdToChange -> VariadicValue.Many(
                              values.map(v => if (v == prefixedValue) prefix + nv else v)
                            )
                          )
                        case VariadicValue.Many(_) => None
                      }

                    } else None
                  case _ => None
                }
              case _ => None
            }
          }
        } else {
          Map.empty
        }
    }

    variadicFormData ++ VariadicFormData(updatedChoices.toList.flatten.toMap)
  }
}

object Propagator {
  def apply(formComponents: List[FormComponent]): Propagator = {
    val propagators: Set[(BaseComponentId, ModelComponentId)] = formComponents.flatMap {
      case fc @ IsChoice(c) =>
        c.options.toList.flatMap {
          case OptionData.ValueBased(_, _, _, _, OptionDataValue.ExprBased(FormCtx(formComponentId)), _, _) =>
            Set((formComponentId.baseComponentId -> fc.id.modelComponentId))
          case _ => Set.empty
        }
      case _ => Set.empty
    }.toSet

    val baseComponentIds: Set[BaseComponentId] = propagators.map { case (k, _) => k }

    val refsInChoices: Map[ModelComponentId, Set[BaseComponentId]] = propagators
      .groupMap { case (_, value) => value } { case (key, _) => key }

    new Propagator(baseComponentIds, refsInChoices)
  }
}
