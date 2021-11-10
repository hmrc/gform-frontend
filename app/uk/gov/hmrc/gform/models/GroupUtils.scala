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

import cats.instances.list._
import cats.syntax.eq._
import cats.syntax.foldable._
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.form.FileId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, Group, IsFileUpload, IsGroup, SectionNumber }
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.FormComponentIdToFileIdMapping

object GroupUtils {

  def removeRecord(
    processData: ProcessData,
    modelComponentId: ModelComponentId,
    sectionNumber: SectionNumber,
    fileIdsWithMapping: FormComponentIdToFileIdMapping
  ): (VariadicFormData[SourceOrigin.Current], FormComponentIdToFileIdMapping, Set[FileId]) = {

    val removingIndex = modelComponentId.maybeIndex.getOrElse(
      throw new IllegalArgumentException(s"Attempting to delete group, but no index found $modelComponentId")
    )

    val formModelRenderPageOptics = processData.formModelOptics.formModelRenderPageOptics
    val formModel = formModelRenderPageOptics.formModel

    val bracket = formModel.bracket(sectionNumber)

    val currentGroups: List[Group] = bracket.toPageModel.toList
      .flatMap(_.allFormComponents)
      .collect {
        case fc @ IsGroup(group) if fc.id.modelComponentId.baseComponentId === modelComponentId.baseComponentId =>
          group
      }

    val allGroupFileUploadIds: Set[FormComponentId] = currentGroups
      .flatMap(_.fields.collect { case fc @ IsFileUpload(_) =>
        fc.id
      })
      .toSet

    val data = processData.formModelOptics.pageOpticsData

    val maybeFormComponent = formModelRenderPageOptics.find(modelComponentId)
    val dataToRemove: VariadicFormData[SourceOrigin.Current] =
      maybeFormComponent.map(data.by).getOrElse(VariadicFormData.empty)
    val indexedComponentId = modelComponentId.indexedComponentId
    val toToReindexed: List[FormComponent] = formModelRenderPageOptics.findBigger(indexedComponentId)

    val groupFileUploadIdToRemoves: Set[FormComponentId] = allGroupFileUploadIds.filter { fcId =>
      fcId.modelComponentId.maybeIndex.fold(false) { index =>
        index === removingIndex
      }
    }.toSet

    val groupFileUploadIdToKeep: Set[FormComponentId] = allGroupFileUploadIds.filter { fcId =>
      fcId.modelComponentId.maybeIndex.fold(false) { index =>
        index < removingIndex
      }
    }.toSet

    val groupFileUploadIdToReindex = allGroupFileUploadIds.filter { fcId =>
      fcId.modelComponentId.maybeIndex.fold(false) { index =>
        index > removingIndex
      }
    }

    val variadicFormDatas: VariadicFormData[SourceOrigin.Current] = toToReindexed.foldMap(data.by)

    val decrementedVariadicFormDatas: VariadicFormData[SourceOrigin.Current] =
      variadicFormDatas.mapKeys(_.decrement)

    val formComponentIdToFileIdMapping = fileIdsWithMapping

    val filesToDelete: Set[FileId] =
      groupFileUploadIdToRemoves.flatMap(ff => formComponentIdToFileIdMapping.find(ff))

    // We need to keep mapping for fileuploads which are not in this Group
    val unrelatedMapping = formComponentIdToFileIdMapping.mapping.filterNot { case (k, _) =>
      allGroupFileUploadIds(k)
    }

    val unchangedMapping = formComponentIdToFileIdMapping.mapping.filter { case (key, _) =>
      groupFileUploadIdToKeep(key)
    }

    val decrementedMapping = formComponentIdToFileIdMapping.mapping.collect {
      case (key, v) if groupFileUploadIdToReindex(key) =>
        key.modelComponentId.decrement.toFormComponentId -> v
    }

    val componentIdToFileId = FormComponentIdToFileIdMapping {
      unchangedMapping ++ decrementedMapping ++ unrelatedMapping
    }

    val updData = data -- dataToRemove -- variadicFormDatas ++ decrementedVariadicFormDatas

    (updData, componentIdToFileId, filesToDelete)

  }

}
