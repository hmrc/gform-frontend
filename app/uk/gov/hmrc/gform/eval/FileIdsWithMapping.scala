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

package uk.gov.hmrc.gform.eval

import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormComponentIdToFileIdMapping }

class FileIdsWithMapping(
  val isFileField: Set[ModelComponentId], // Ids of all file components
  val mapping: FormComponentIdToFileIdMapping
) {
  def associatedFileIds(modelComponentIds: Set[ModelComponentId]): Set[FileId] =
    modelComponentIds.filter(isFileField).map(mapping.find).flatten
}

object FileIdsWithMapping {
  val empty = new FileIdsWithMapping(Set.empty, FormComponentIdToFileIdMapping.empty)

  def apply(fileIds: Set[ModelComponentId], mapping: FormComponentIdToFileIdMapping): FileIdsWithMapping =
    new FileIdsWithMapping(
      fileIds,
      mapping
    )

}
