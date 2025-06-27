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
      "0\u00A0Bytes"
    } else {
      val k = 1024d
      val sizes = List("Bytes", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")
      val i = Math.floor(Math.log(size.toDouble) / Math.log(k)).toInt
      val v = size.toDouble / Math.pow(k, i.toDouble)

      val formatted = i match {
        case 0 => s"${v.toInt}" // Bytes
        case 1 => Math.ceil(v).toInt.toString // KB
        case 2 => // MB
          val rounded = Math.ceil(v * 10) / 10
          if (rounded == rounded.toInt) rounded.toInt.toString
          else f"$rounded%.1f"
        case _ => // GB and up
          val rounded = Math.ceil(v * 100) / 100
          f"$rounded%.2f"
      }

      s"$formatted\u00A0${sizes(i)}"
    }
}
