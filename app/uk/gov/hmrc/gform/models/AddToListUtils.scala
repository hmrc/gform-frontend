/*
 * Copyright 2020 HM Revenue & Customs
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

import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData }

object AddToListUtils {

  def removeRecord(
    processData: ProcessData,
    bracket: Bracket.AddToList[DataExpanded],
    idx: Int,
  ): VariadicFormData[SourceOrigin.Current] = {

    def toModelComponentIds(iterations: List[Bracket.AddToListIteration[DataExpanded]]): Set[ModelComponentId] =
      iterations.flatMap(_.toPageModel.toList).flatMap(_.allModelComponentIds).toSet

    val iteration: Bracket.AddToListIteration[DataExpanded] = bracket.iterations.toList(idx)

    val (iteratonsToKeep, iterationsToReindex) = bracket.iterations.toList.splitAt(idx + 1)

    val toBeRemovedIds: Set[ModelComponentId] = toModelComponentIds(iteration :: Nil)
    val toReindex: Set[ModelComponentId] = toModelComponentIds(iterationsToReindex)
    /* iteration is part of iterationsToKeep (due to how splitAt works),
     * and we don't want iteration's ids in toKeep Set.
     */
    val toKeep: Set[ModelComponentId] = toModelComponentIds(iteratonsToKeep) -- toBeRemovedIds

    val variadicFormData = processData.formModelOptics.pageOpticsData

    val variadicFormDataToModify = variadicFormData.subset(toReindex)
    val variadicFormDataToKeep = variadicFormData.subset(toKeep)

    val variadicFormDataToModified = variadicFormDataToModify.mapKeys(_.decrement)

    val bracketPrefixes: Set[Int] =
      (variadicFormDataToKeep.keySet ++ variadicFormDataToModified.keySet).flatMap(_.maybeIndex)

    val maxBracketPrefix = bracketPrefixes.max

    val lastAddAnotherQuestionId =
      bracket.source.id.formComponentId.modelComponentId.expandWithPrefix(maxBracketPrefix)

    variadicFormData -- toBeRemovedIds -- variadicFormDataToModify ++ variadicFormDataToModified - lastAddAnotherQuestionId

  }

}
