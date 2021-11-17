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

package uk.gov.hmrc.gform.models

import uk.gov.hmrc.gform.eval.FileIdsWithMapping
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.form.FileId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, IsFileUpload }
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.FormComponentIdToFileIdMapping

object AddToListUtils {

  def removeRecord(
    processData: ProcessData,
    bracket: Bracket.AddToList[DataExpanded],
    idx: Int,
    fileIdsWithMapping: FileIdsWithMapping
  ): (VariadicFormData[SourceOrigin.Current], FormComponentIdToFileIdMapping, Set[FileId]) = {

    val addToListFileUploadIds: Set[FormComponentId] = bracket.toPageModel.toList
      .flatMap(_.allFormComponents)
      .collect { case fc @ IsFileUpload(_) =>
        fc.id
      }
      .toSet

    // We need to keep mapping for fileuploads which are not in this AddToList
    val unrelatedMapping = fileIdsWithMapping.mapping.mapping.filterNot { case (k, _) => addToListFileUploadIds(k) }

    def toModelComponentIds(iterations: List[Bracket.AddToListIteration[DataExpanded]]): Set[ModelComponentId] =
      iterations.flatMap(_.toPageModel.toList).flatMap(_.allModelComponentIds).toSet

    val iteration: Bracket.AddToListIteration[DataExpanded] = bracket.iterations.toList(idx)

    val (iterationsToKeep, iterationsToReindex) = bracket.iterations.toList.splitAt(idx + 1)

    val toBeRemovedIds: Set[ModelComponentId] = toModelComponentIds(iteration :: Nil)

    val filesToBeDeletedFromFileUpload: Set[FileId] = fileIdsWithMapping.associatedFileIds(toBeRemovedIds)

    val toReindex: Set[ModelComponentId] = toModelComponentIds(iterationsToReindex)
    /* iteration is part of iterationsToKeep (due to how splitAt works),
     * and we don't want iteration's ids in toKeep Set.
     */
    val toKeep: Set[ModelComponentId] = toModelComponentIds(iterationsToKeep) -- toBeRemovedIds

    val variadicFormData = processData.formModelOptics.pageOpticsData

    val variadicFormDataToModify = variadicFormData.subset(toReindex)
    val variadicFormDataToKeep = variadicFormData.subset(toKeep)

    val onlyFileIds = variadicFormDataToModify.subset(fileIdsWithMapping.isFileField).keySet()
    val variadicFormDataToModified = variadicFormDataToModify.mapKeys(_.decrement)

    val decrementedMapping =
      fileIdsWithMapping.mapping.mapping
        .filter { case (key, _) =>
          onlyFileIds(key.modelComponentId)
        }
        .map { case (k, v) => k.modelComponentId.decrement.toFormComponentId -> v }

    val unchangedMapping = fileIdsWithMapping.mapping.mapping.filter { case (key, _) =>
      toKeep(key.modelComponentId)
    }

    val componentIdToFileIdMapping = FormComponentIdToFileIdMapping {
      unchangedMapping ++ decrementedMapping ++ unrelatedMapping
    }

    val bracketPrefixes: Set[Int] =
      (variadicFormDataToKeep.keySet ++ variadicFormDataToModified.keySet).flatMap(_.maybeIndex)

    val updatedVariadicFormData = {
      val initialVariadicFormData =
        variadicFormData -- toBeRemovedIds -- variadicFormDataToModify ++ variadicFormDataToModified
      if (bracketPrefixes.isEmpty)
        initialVariadicFormData
      else
        initialVariadicFormData - bracket.source.id.formComponentId.modelComponentId
          .expandWithPrefix(bracketPrefixes.max)
    }

    (
      updatedVariadicFormData,
      componentIdToFileIdMapping,
      filesToBeDeletedFromFileUpload
    )
  }

}
