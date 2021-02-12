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
import cats.syntax.show._
import uk.gov.hmrc.gform.fileupload.{ Envelope, EnvelopeWithMapping }
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, Form, FormData, InProgress, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

object FileUploadUtils {

  def updateMapping(
    formComponentId: FormComponentId,
    fileId: FileId,
    form: Form,
    envelope: Envelope
  ): (FileId, UserData) = {
    val mapping = form.componentIdToFileId
    val modelComponentId = formComponentId.modelComponentId
    val resolvedFileId =
      if (fileId === FileId.empty) {
        mapping.fileIdFor(formComponentId)
      } else {
        EnvelopeWithMapping(envelope, form)
          .find(modelComponentId)
          .map(_.fileId)
          .getOrElse(throw new IllegalArgumentException(s"No associated FileId found for $modelComponentId"))
      }

    val mappingUpd = mapping + (formComponentId, resolvedFileId)

    val userData = UserData(
      form.formData, // FormData are updated by Submit button from the browser
      InProgress,
      form.visitsIndex,
      form.thirdPartyData,
      mappingUpd
    )
    (resolvedFileId, userData)
  }

  def prepareDeleteFile(formComponentId: FormComponentId, form: Form): (FileId, UserData) = {
    val mapping = form.componentIdToFileId
    val fileToDelete = mapping
      .find(formComponentId)
      .getOrElse(throw new IllegalArgumentException(show"No associated FileId found for $formComponentId"))

    val mappingUpd = mapping - formComponentId
    val formDataUpd =
      FormData(form.formData.fields.filterNot(formField => formField.id === formComponentId.modelComponentId))

    val userData = UserData(
      formDataUpd,
      InProgress,
      form.visitsIndex,
      form.thirdPartyData,
      mappingUpd
    )

    (fileToDelete, userData)
  }

}
