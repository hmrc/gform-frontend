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

import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.gform.sharedmodel.form.{ Accepted, EnvelopeId, FileId, Form, FormComponentIdToFileIdMapping, FormData, FormField, FormId, QueryParams, TaskIdTaskStatusMapping, ThirdPartyData, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FileComponentId
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, NotChecked, UserId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId }

class FileUploadUtilsSpec extends AnyFlatSpecLike with Matchers with FormModelSupport with VariadicFormDataSupport {

  "FileUploadUtils.updateMapping" should "update formComponentId <-> fileId mapping" in {
    def mkForm(mapping: FormComponentIdToFileIdMapping) = Form(
      FormId("666"),
      EnvelopeId("id1"),
      UserId("usr"),
      FormTemplateId("temp"),
      None,
      FormData(List.empty[FormField]),
      Accepted,
      VisitIndex.Classic(Set.empty),
      ThirdPartyData(
        NotChecked,
        Map.empty,
        QueryParams.empty,
        None,
        BooleanExprCache.empty,
        None,
        None,
        None,
        None,
        None,
        None
      ),
      None,
      mapping,
      TaskIdTaskStatusMapping.empty
    )

    val mapping1 = FormComponentIdToFileIdMapping.empty
    val expectedMapping1 = Map("file" -> "file").toMapping
    val mapping2 = Map("file" -> "x_file").toMapping
    val expectedMapping2 = Map("file" -> "x_file").toMapping

    val variations = Table(
      ("fileComponentId", "fileId", "mapping", "expectedMapping"),
      (FileComponentId.fromString("file"), FileId("file"), mapping1, expectedMapping1),
      (FileComponentId.fromString("file"), FileId("x_file"), mapping2, expectedMapping2)
    )

    forAll(variations) { case (fileComponentId, fileId, componentIdToFileId, expectedComponentIdToFileId) =>
      val form: Form = mkForm(componentIdToFileId)
      val userData = FileUploadUtils.updateMapping(fileComponentId, fileId, form)

      userData.componentIdToFileId shouldBe expectedComponentIdToFileId
    }
  }

  "FileUploadUtils.prepareDeleteFile" should "update formComponentId <-> fileId mapping and return fileId to delete" in {
    def mkForm(mapping: FormComponentIdToFileIdMapping, formData: FormData) = Form(
      FormId("666"),
      EnvelopeId("id1"),
      UserId("usr"),
      FormTemplateId("temp"),
      None,
      formData,
      Accepted,
      VisitIndex.Classic(Set.empty),
      ThirdPartyData(
        NotChecked,
        Map.empty,
        QueryParams.empty,
        None,
        BooleanExprCache.empty,
        None,
        None,
        None,
        None,
        None,
        None
      ),
      None,
      mapping,
      TaskIdTaskStatusMapping.empty
    )

    val mapping2 = Map("file" -> "x_file").toMapping
    val expectedMapping2 = FormComponentIdToFileIdMapping.empty
    val mapping3 = Map("file" -> "x_file", "fileB" -> "x_fileB").toMapping
    val expectedMapping3 = Map("fileB" -> "x_fileB").toMapping

    val variations = Table(
      ("fileComponentId", "mapping", "formData", "expectedFileId", "expectedMapping", "expectedFormData"),
      (
        FileComponentId.fromString("file"),
        mapping2,
        Map("file" -> "invoice_pdf").toFormData,
        FileId("x_file"),
        expectedMapping2,
        Map.empty[String, String].toFormData
      ),
      (
        FileComponentId.fromString("file"),
        mapping3,
        Map("file" -> "invoice_pdf", "unrelated" -> "abc").toFormData,
        FileId("x_file"),
        expectedMapping3,
        Map("unrelated" -> "abc").toFormData
      )
    )

    forAll(variations) {
      case (
            fileComponentId,
            componentIdToFileId,
            inputFormData,
            expectedFileId,
            expectedComponentIdToFileId,
            expectedFormData
          ) =>
        val form: Form = mkForm(componentIdToFileId, inputFormData)

        val tuple = FileUploadUtils.prepareDeleteFile(fileComponentId, form)

        tuple match {
          case Some((fileId, formData, mapping)) =>
            fileId shouldBe expectedFileId
            mapping shouldBe expectedComponentIdToFileId
            formData shouldBe expectedFormData
          case _ => fail()
        }

    }
  }

  "FileUploadUtils.formatSize" should
    "format the file size" in {
      FileUploadUtils.formatSize(0L) shouldBe "0\u00A0Bytes"
      FileUploadUtils.formatSize(1L) shouldBe "1\u00A0Bytes"
      FileUploadUtils.formatSize(10L) shouldBe "10\u00A0Bytes"
      FileUploadUtils.formatSize(100L) shouldBe "100\u00A0Bytes"
      FileUploadUtils.formatSize(1000L) shouldBe "1000\u00A0Bytes"
      FileUploadUtils.formatSize((123.99 * 1024).toLong) shouldBe "124\u00A0KB"
      FileUploadUtils.formatSize((299.34 * 1024).toLong) shouldBe "300\u00A0KB"
      FileUploadUtils.formatSize((4.78 * 1024 * 1024).toLong) shouldBe "4.8\u00A0MB"
      FileUploadUtils.formatSize((25.998 * 1024 * 1024).toLong) shouldBe "26\u00A0MB"
      FileUploadUtils.formatSize((35.63 * 1024 * 1024).toLong) shouldBe "35.7\u00A0MB"
      FileUploadUtils.formatSize((1.23 * 1024 * 1024 * 1024).toLong) shouldBe "1.23\u00A0GB"
    }

  implicit class MapOps(map: Map[String, String]) {
    def toMapping: FormComponentIdToFileIdMapping =
      FormComponentIdToFileIdMapping(map.map { case (k, v) => FileComponentId.fromString(k) -> FileId(v) })

    def toFormData: FormData =
      FormData(map.toList.map { case (k, v) => FormField(FormComponentId(k).modelComponentId, v) })
  }
}
