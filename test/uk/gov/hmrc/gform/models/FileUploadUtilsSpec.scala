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
import uk.gov.hmrc.gform.sharedmodel.form.{ FormField, QueryParams }
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, NotChecked, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ Accepted, EnvelopeId, FileId, Form, FormData, FormId, ThirdPartyData, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.form.FormComponentIdToFileIdMapping

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
      mapping
    )

    val mapping1 = FormComponentIdToFileIdMapping.empty
    val expectedMapping1 = Map("file" -> "file").toMapping
    val mapping2 = Map("file" -> "x_file").toMapping
    val expectedMapping2 = Map("file" -> "x_file").toMapping

    val variations = Table(
      ("formComponentId", "fileId", "mapping", "expectedMapping"),
      (FormComponentId("file"), FileId("file"), mapping1, expectedMapping1),
      (FormComponentId("file"), FileId("x_file"), mapping2, expectedMapping2)
    )

    forAll(variations) { case (formComponentId, fileId, componentIdToFileId, expectedComponentIdToFileId) =>
      val form: Form = mkForm(componentIdToFileId)
      val userData = FileUploadUtils.updateMapping(formComponentId, fileId, form)

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
      mapping
    )

    val mapping2 = Map("file" -> "x_file").toMapping
    val expectedMapping2 = FormComponentIdToFileIdMapping.empty
    val mapping3 = Map("file" -> "x_file", "fileB" -> "x_fileB").toMapping
    val expectedMapping3 = Map("fileB" -> "x_fileB").toMapping

    val variations = Table(
      ("formComponentId", "mapping", "formData", "expectedFileId", "expectedMapping", "expectedFormData"),
      (
        FormComponentId("file"),
        mapping2,
        Map("file" -> "invoice_pdf").toFormData,
        FileId("x_file"),
        expectedMapping2,
        Map.empty[String, String].toFormData
      ),
      (
        FormComponentId("file"),
        mapping3,
        Map("file" -> "invoice_pdf", "unrelated" -> "abc").toFormData,
        FileId("x_file"),
        expectedMapping3,
        Map("unrelated" -> "abc").toFormData
      )
    )

    forAll(variations) {
      case (
            formComponentId,
            componentIdToFileId,
            inputFormData,
            expectedFileId,
            expectedComponentIdToFileId,
            expectedFormData
          ) =>
        val form: Form = mkForm(componentIdToFileId, inputFormData)

        val tuple = FileUploadUtils.prepareDeleteFile(formComponentId, form)

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
      FileUploadUtils.formatSize(0L) shouldBe "0 Bytes"
      FileUploadUtils.formatSize(1L) shouldBe "1 Bytes"
      FileUploadUtils.formatSize(10L) shouldBe "10 Bytes"
      FileUploadUtils.formatSize(100L) shouldBe "100 Bytes"
      FileUploadUtils.formatSize(1000L) shouldBe "1000 Bytes"
      FileUploadUtils.formatSize(10000L) shouldBe "9.77 KB"
      FileUploadUtils.formatSize(100000L) shouldBe "97.66 KB"
      FileUploadUtils.formatSize(1000000L) shouldBe "976.56 KB"
      FileUploadUtils.formatSize(10000000L) shouldBe "9.54 MB"
      FileUploadUtils.formatSize(1000000000L) shouldBe "953.67 MB"
      FileUploadUtils.formatSize(10000000000L) shouldBe "9.31 GB"
    }

  implicit class MapOps(map: Map[String, String]) {
    def toMapping: FormComponentIdToFileIdMapping =
      FormComponentIdToFileIdMapping(map.map { case (k, v) => FormComponentId(k) -> FileId(v) })

    def toFormData: FormData =
      FormData(map.toList.map { case (k, v) => FormField(FormComponentId(k).modelComponentId, v) })
  }
}
