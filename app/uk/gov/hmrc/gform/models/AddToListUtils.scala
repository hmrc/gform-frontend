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

    val iteration: Bracket.AddToListIteration[DataExpanded] = bracket.iterations.toList(idx)

    val (_, iterationsToReindex) = bracket.iterations.toList.splitAt(idx + 1)

    val toBeRemovedIds: List[ModelComponentId] = iteration.toPageModel.toList.flatMap(_.allModelComponentIds)
    val toReindex: Set[ModelComponentId] =
      iterationsToReindex.flatMap(_.toPageModel.toList).flatMap(_.allModelComponentIds).toSet

    val variadicFormData = processData.formModelOptics.pageOpticsData

    val variadicFormDataToModify = variadicFormData.subset(toReindex)

    val variadicFormDataToModified = variadicFormDataToModify.mapKeys { variadicValueId =>
      variadicValueId.decrement
    }

    val res = variadicFormData -- toBeRemovedIds -- variadicFormDataToModify ++ variadicFormDataToModified

    val prefixes: Set[Int] = res.keySet.flatMap(_.maybeIndex)

    val maxPrefix = prefixes.max

    val lastAddAnotherQuestionId =
      bracket.source.id.formComponentId.modelComponentId.expandWithPrefix(maxPrefix)

    res - lastAddAnotherQuestionId

  }

}
