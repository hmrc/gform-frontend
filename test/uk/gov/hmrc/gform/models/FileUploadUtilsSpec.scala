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

import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.fileupload.{ Available, Envelope, File }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormField, QueryParams }
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, NotChecked, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ Accepted, EnvelopeId, FileId, Form, FormData, FormId, ThirdPartyData, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.form.FormComponentIdToFileIdMapping

class FileUploadUtilsSpec extends FlatSpec with Matchers with FormModelSupport with VariadicFormDataSupport {

  "FileUploadUtils.updateMapping" should "update formComponentId <-> fileId mapping" in {
    def mkForm(mapping: FormComponentIdToFileIdMapping) = Form(
      FormId("666"),
      EnvelopeId("id1"),
      UserId("usr"),
      FormTemplateId("temp"),
      FormData(List.empty[FormField]),
      Accepted,
      VisitIndex(Set.empty),
      ThirdPartyData(None, NotChecked, Map.empty, QueryParams.empty, None, BooleanExprCache.empty),
      None,
      mapping
    )

    val mapping1 = FormComponentIdToFileIdMapping.empty
    val expectedMapping1 = Map("file" -> "file").toMapping
    val mapping2 = Map("file" -> "x_file").toMapping
    val expectedMapping2 = Map("file" -> "x_file").toMapping
    val mapping3 = Map("file" -> "x_file").toMapping
    val expectedMapping3 = Map("file" -> "x_file").toMapping

    val variations = Table(
      ("formComponentId", "fileId", "mapping", "envelopeFiles", "expectedFileId", "expectedMapping"),
      (FormComponentId("file"), FileId.empty, mapping1, List.empty[String], FileId("file"), expectedMapping1),
      (FormComponentId("file"), FileId.empty, mapping2, List.empty[String], FileId("x_file"), expectedMapping2),
      (FormComponentId("file"), FileId("dummy"), mapping3, List("x_file"), FileId("x_file"), expectedMapping3)
    )

    forAll(variations) {
      case (formComponentId, fileId, componentIdToFileId, envelopeFiles, expectedFileId, expectedComponentIdToFileId) ⇒
        val envelope: Envelope =
          Envelope(envelopeFiles.map(fileId => File(FileId(fileId), Available, fileId + "_invoice.pdf")))
        val form: Form = mkForm(componentIdToFileId)
        val (realFileId, userData) = FileUploadUtils.updateMapping(formComponentId, fileId, form, envelope)

        realFileId shouldBe expectedFileId
        userData.componentIdToFileId shouldBe expectedComponentIdToFileId
    }
  }

  "FileUploadUtils.prepareDeleteFile" should "update formComponentId <-> fileId mapping and return fileId to delete" in {
    def mkForm(mapping: FormComponentIdToFileIdMapping, formData: FormData) = Form(
      FormId("666"),
      EnvelopeId("id1"),
      UserId("usr"),
      FormTemplateId("temp"),
      formData,
      Accepted,
      VisitIndex(Set.empty),
      ThirdPartyData(None, NotChecked, Map.empty, QueryParams.empty, None, BooleanExprCache.empty),
      None,
      mapping
    )

    /* val mapping1 = FormComponentIdToFileIdMapping.empty
     * val expectedMapping1 = Map("file" -> "file").toMapping */
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
            formData,
            expectedFileId,
            expectedComponentIdToFileId,
            expectedFormData
          ) ⇒
        val form: Form = mkForm(componentIdToFileId, formData)

        val (realFileId, userData) = FileUploadUtils.prepareDeleteFile(formComponentId, form)

        realFileId shouldBe expectedFileId
        userData.componentIdToFileId shouldBe expectedComponentIdToFileId
        userData.formData shouldBe expectedFormData
    }
  }

  implicit class MapOps(map: Map[String, String]) {
    def toMapping: FormComponentIdToFileIdMapping =
      FormComponentIdToFileIdMapping(map.map { case (k, v) => FormComponentId(k) -> FileId(v) })

    def toFormData: FormData =
      FormData(map.toList.map { case (k, v) => FormField(FormComponentId(k).modelComponentId, v) })
  }
}
