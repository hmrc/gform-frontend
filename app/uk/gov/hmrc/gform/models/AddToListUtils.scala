/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.VariadicValue
import uk.gov.hmrc.gform.sharedmodel.form.FileId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileComponentId, FormComponentId, IsFileUpload, IsMultiFileUpload }
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
      .collect {
        case fc @ IsFileUpload(_)      => fc.id
        case fc @ IsMultiFileUpload(_) => fc.id
      }
      .toSet

    val multiFileUploads: Set[ModelComponentId] = bracket.toPageModel.toList
      .flatMap(_.allFormComponents)
      .collect { case fc @ IsMultiFileUpload(_) =>
        fc.id.modelComponentId
      }
      .toSet

    // We need to keep mapping for fileuploads which are not in this AddToList
    val unrelatedMapping: Map[FileComponentId, FileId] = fileIdsWithMapping.mapping.mapping.filterNot { case (k, _) =>
      addToListFileUploadIds(k.underlyingFormComponentId())
    }

    def toModelComponentIds(iterations: List[Bracket.AddToListIteration[DataExpanded]]): Set[ModelComponentId] =
      iterations.flatMap(_.toPageModel.toList).flatMap(_.allModelComponentIds).toSet

    val iteration: Bracket.AddToListIteration[DataExpanded] = bracket.iterations.toList(idx)

    val (iterationsToKeep, iterationsToReindex) = bracket.iterations.toList.splitAt(idx + 1)

    val toBeRemovedIds: Set[ModelComponentId] = toModelComponentIds(iteration :: Nil)

    val toBeRemovedBaseIds: Set[BaseComponentId] = toBeRemovedIds.map(_.baseComponentId)

    val dynamicChoicesOutsideATLToBeRemoved: List[ModelComponentId] = processData.formModel.allDynamicChoices.collect {
      case (fcId, baseComponentIds) if baseComponentIds.exists(toBeRemovedBaseIds) => fcId.modelComponentId
    }

    val filesToBeDeletedFromFileUpload: Set[FileId] = fileIdsWithMapping.associatedFileIds(toBeRemovedIds)

    val toReindex: Set[ModelComponentId] = toModelComponentIds(iterationsToReindex)
    /* iteration is part of iterationsToKeep (due to how splitAt works),
     * and we don't want iteration's ids in toKeep Set.
     */
    val variadicFormData = processData.formModelOptics.pageOpticsData

    val toBeRemovedMultiFileIds: Set[ModelComponentId] =
      toReindex.toList.flatMap { modelComponentId =>
        if (multiFileUploads(modelComponentId)) {
          variadicFormData.filesOfMultiFileComponent(modelComponentId).map { case (fileComponentId, _) =>
            fileComponentId.toModelComponentId()
          }
        } else {
          List.empty
        }
      }.toSet

    val toReindexMultiFile: VariadicFormData[SourceOrigin.Current] = VariadicFormData(
      toReindex.toList.flatMap { modelComponentId =>
        if (multiFileUploads(modelComponentId)) {
          val res: List[(ModelComponentId, VariadicValue.One)] =
            variadicFormData.filesOfMultiFileComponent(modelComponentId).map { case (fileComponentId, fileName) =>
              fileComponentId match {
                case FileComponentId.Multi(fcId, index) =>
                  FileComponentId
                    .Multi(fcId.modelComponentId.decrement.toFormComponentId, index)
                    .toModelComponentId() -> fileName
              }
            }
          res
        } else {
          List.empty
        }
      }.toMap
    )

    val toKeep: Set[ModelComponentId] = toModelComponentIds(iterationsToKeep) -- toBeRemovedIds

    val variadicFormDataToModify: VariadicFormData[SourceOrigin.Current] = variadicFormData.subset(toReindex)
    val variadicFormDataToKeep = variadicFormData.subset(toKeep)
    val variadicFormDataToModified = variadicFormDataToModify.mapKeys(_.decrement)

    val onlySingleFileIds = variadicFormDataToModify.subset(fileIdsWithMapping.isSingleFileField).keySet()
    val onlyMultiFileIds = variadicFormDataToModify.subset(fileIdsWithMapping.isMultiFileField).keySet()

    val decrementedMapping: Map[FileComponentId, FileId] =
      fileIdsWithMapping.mapping.mapping
        .filter { case (fileComponentId, _) =>
          fileComponentId match {
            case FileComponentId.Single(fcId)       => onlySingleFileIds(fcId.modelComponentId)
            case FileComponentId.Multi(fcId, index) => onlyMultiFileIds(fcId.modelComponentId)
          }
        }
        .map { case (fileComponentId, fileId) =>
          fileComponentId match {
            case FileComponentId.Single(fcId) =>
              FileComponentId.Single(fcId.modelComponentId.decrement.toFormComponentId) -> fileId
            case FileComponentId.Multi(fcId, index) =>
              FileComponentId.Multi(fcId.modelComponentId.decrement.toFormComponentId, index) -> fileId
          }
        }

    val unchangedMapping = fileIdsWithMapping.mapping.mapping.filter { case (key, _) =>
      toKeep(key.underlyingFormComponentId().modelComponentId)
    }

    val componentIdToFileIdMapping = FormComponentIdToFileIdMapping {
      unchangedMapping ++ decrementedMapping ++ unrelatedMapping
    }

    val bracketPrefixes: collection.Set[Int] =
      (variadicFormDataToKeep.keySet() ++ variadicFormDataToModified.keySet()).flatMap(_.maybeIndex)

    val updatedVariadicFormData = {
      val initialVariadicFormData =
        variadicFormData -- toBeRemovedIds -- toBeRemovedMultiFileIds -- variadicFormDataToModify -- dynamicChoicesOutsideATLToBeRemoved ++ variadicFormDataToModified ++ toReindexMultiFile

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
