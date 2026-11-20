/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.bulkupload

import cats.syntax.traverse._
import cats.instances.future._
import cats.syntax.functor._
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormIdData, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BulkUpload, FormComponentId, FormTemplateId, IsBulkUpload }
import uk.gov.hmrc.http.HeaderCarrier

sealed trait BulkUploadService[F[_]] {
  def removeRow(rowIndex: RowIndex, formComponentId: FormComponentId, form: Form, maybeAccessCode: Option[AccessCode])(
    implicit hc: HeaderCarrier
  ): Future[Unit]

  def changeCell(
    data: String,
    columnIndex: ColumnIndex,
    rowIndex: RowIndex,
    formComponentId: FormComponentId,
    form: Form,
    maybeAccessCode: Option[AccessCode]
  )(implicit
    hc: HeaderCarrier
  ): Future[Unit]

  def syncObjectStoreFilesWithUserData(
    formTemplateId: FormTemplateId,
    envelopeId: EnvelopeId,
    envelope: EnvelopeWithMapping,
    formModelVisibilityOptics: FormModelVisibilityOptics,
    bulkUploadResults: Option[Map[FormComponentId, BulkUploadResult]]
  )(implicit
    hc: HeaderCarrier
  ): Future[Unit]
}

object BulkUploadService {

  def apply(
    gformConnector: GformConnector
  )(implicit
    ex: ExecutionContext
  ): BulkUploadService[Future] = new BulkUploadService[Future] {

    def removeRow(
      rowIndex: RowIndex,
      formComponentId: FormComponentId,
      form: Form,
      maybeAccessCode: Option[AccessCode]
    )(implicit
      hc: HeaderCarrier
    ): Future[Unit] = {
      val bulkUploadResult: BulkUploadResult = form.thirdPartyData.bulkUploadFor(formComponentId)

      bulkUploadResult match {
        case BulkUploadResult.Ok(rowsData) =>
          val rowsDataUpd = rowsData.patch(rowIndex.index, Nil, 1)
          updateBulkUploadData(BulkUploadResult.Ok(rowsDataUpd), form, maybeAccessCode, formComponentId)
        case BulkUploadResult.Error(_) => Future.successful(())
      }
    }

    def changeCell(
      data: String,
      columnIndex: ColumnIndex,
      rowIndex: RowIndex,
      formComponentId: FormComponentId,
      form: Form,
      maybeAccessCode: Option[AccessCode]
    )(implicit
      hc: HeaderCarrier
    ): Future[Unit] = {
      val bulkUploadResult: BulkUploadResult = form.thirdPartyData.bulkUploadFor(formComponentId)

      bulkUploadResult match {
        case BulkUploadResult.Ok(rowsData) =>
          val row = rowsData
            .lift(rowIndex.index)
            .getOrElse(
              throw new IndexOutOfBoundsException(
                s"Invalid row index: ${rowIndex.index} for formComponentId: '$formComponentId'"
              )
            )

          val _ = row
            .lift(columnIndex.index)
            .getOrElse(
              throw new IndexOutOfBoundsException(
                s"Invalid column index: ${columnIndex.index} for formComponentId: '$formComponentId'"
              )
            )

          val rowsDataUpd = rowsData.updated(rowIndex.index, row.updated(columnIndex.index, data))

          updateBulkUploadData(BulkUploadResult.Ok(rowsDataUpd), form, maybeAccessCode, formComponentId)
        case BulkUploadResult.Error(_) => Future.successful(())
      }
    }

    private def updateBulkUploadData(
      bulkUploadResult: BulkUploadResult,
      form: Form,
      maybeAccessCode: Option[AccessCode],
      formComponentId: FormComponentId
    )(implicit
      hc: HeaderCarrier
    ): Future[Unit] = {
      val updatedThirdPartyData = form.thirdPartyData.updateBulkUpload(formComponentId, bulkUploadResult)

      val formIdData: FormIdData = FormIdData.fromForm(form, maybeAccessCode)
      val userData: UserData = UserData(
        formData = form.formData,
        formStatus = form.status,
        visitsIndex = form.visitsIndex,
        thirdPartyData = updatedThirdPartyData,
        componentIdToFileId = form.componentIdToFileId,
        taskIdTaskStatus = form.taskIdTaskStatus
      )
      gformConnector.updateUserData(formIdData, userData)
    }

    def syncObjectStoreFilesWithUserData(
      formTemplateId: FormTemplateId,
      envelopeId: EnvelopeId,
      envelope: EnvelopeWithMapping,
      formModelVisibilityOptics: FormModelVisibilityOptics,
      bulkUploadResults: Option[Map[FormComponentId, BulkUploadResult]]
    )(implicit
      hc: HeaderCarrier
    ): Future[Unit] = {
      val formComponentIds: List[(FormComponentId, BulkUpload, BulkUploadResult.Ok)] = bulkUploadResults match {
        case None => List.empty[(FormComponentId, BulkUpload, BulkUploadResult.Ok)]
        case Some(lookup) =>
          lookup.toList.flatMap {
            case (formComponentId, ok @ BulkUploadResult.Error(error)) =>
              throw new Exception(
                s"Attempt to upload invalid bulk upload data on $formTemplateId. Data for $formComponentId have error: $error"
              )
            case (formComponentId, ok @ BulkUploadResult.Ok(_)) =>
              formModelVisibilityOptics.formModel.fcLookup.get(formComponentId) match {
                case Some(IsBulkUpload(bulkUpload)) => (formComponentId, bulkUpload, ok) :: Nil
                case _                              => Nil // The file will be cleansed later
              }
          }
      }

      formComponentIds.traverse { case (formComponentId, bulkUpload, bulkUploadResult) =>
        val fileName = envelope
          .findSingle(formComponentId.modelComponentId)
          .map(_.fileName)
          .getOrElse(
            throw new Exception(s"No bulk upload file found for $formComponentId. Form template $formTemplateId")
          )
        gformConnector.createBulkUploadSpreadsheetFromData(
          envelopeId,
          fileName,
          formTemplateId,
          formComponentId,
          bulkUpload,
          bulkUploadResult
        )
      }.void

    }
  }

}
