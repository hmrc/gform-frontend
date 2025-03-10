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

import cats.syntax.eq._
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, Form, FormComponentIdToFileIdMapping, FormData, InProgress, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FileComponentId

object FileUploadUtils {

  def updateMapping(
    fileComponentId: FileComponentId,
    fileId: FileId,
    form: Form
  ): UserData = {
    val mapping = form.componentIdToFileId

    val mappingUpd = mapping + (fileComponentId, fileId)

    UserData(
      form.formData, // FormData are updated by Submit button from the browser
      InProgress,
      form.visitsIndex,
      form.thirdPartyData,
      mappingUpd,
      form.taskIdTaskStatus
    )
  }

  def prepareDeleteFile(
    fileComponentId: FileComponentId,
    form: Form
  ): Option[(FileId, FormData, FormComponentIdToFileIdMapping)] = {
    val mapping = form.componentIdToFileId
    mapping.find(fileComponentId).map { fileToDelete =>
      val mappingUpd = mapping - fileComponentId
      val formDataUpd =
        FormData(
          form.formData.fields
            .filterNot { formField =>
              val formFileComponentId = FileComponentId.fromFormField(formField)
              formFileComponentId === fileComponentId
            }
            .map { formField =>
              val formFileComponentId = FileComponentId.fromFormField(formField)
              (fileComponentId, formFileComponentId) match {
                case (FileComponentId.Multi(fcId1, index1), FileComponentId.Multi(fcId2, index2))
                    if fcId1 === fcId2 && index2 > index1 =>
                  formFileComponentId.decrement().toFormField(formField.value)
                case _ => formField
              }

            }
        )

      (fileToDelete, formDataUpd, mappingUpd)
    }
  }

  def formatSize(size: Long): String =
    if (size == 0) {
      "0 Bytes"
    } else {
      val k = 1024d
      val sizes = List("Bytes", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB");
      val i = Math.floor(Math.log(size.toDouble) / Math.log(k));
      val v = size / Math.pow(k, i)
      if (i == 0)
        s"${v.toInt} ${sizes(i.toInt)}"
      else
        f"$v%1.2f ${sizes(i.toInt)}"
    }
}
