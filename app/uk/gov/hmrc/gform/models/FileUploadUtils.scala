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

import cats.syntax.eq._
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, Form, FormComponentIdToFileIdMapping, FormData, InProgress, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

object FileUploadUtils {

  def updateMapping(
    formComponentId: FormComponentId,
    fileId: FileId,
    form: Form
  ): UserData = {
    val mapping = form.componentIdToFileId

    val mappingUpd = mapping + (formComponentId, fileId)

    UserData(
      form.formData, // FormData are updated by Submit button from the browser
      InProgress,
      form.visitsIndex,
      form.thirdPartyData,
      mappingUpd
    )
  }

  def prepareDeleteFile(
    formComponentId: FormComponentId,
    form: Form
  ): Option[(FileId, FormData, FormComponentIdToFileIdMapping)] = {
    val mapping = form.componentIdToFileId
    mapping.find(formComponentId).map { fileToDelete =>
      val mappingUpd = mapping - formComponentId
      val formDataUpd =
        FormData(form.formData.fields.filterNot(formField => formField.id === formComponentId.modelComponentId))

      (fileToDelete, formDataUpd, mappingUpd)
    }
  }
}
