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

package uk.gov.hmrc.gform.upscan

import akka.util.ByteString
import cats.implicits._
import java.time.Instant
import play.api.libs.json.Json
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.crypto.{ Crypted, CryptoWithKeysFromConfig, PlainText }
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormIdData, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId, SectionNumber }
import uk.gov.hmrc.gform.upscan.routes.UpscanController
import uk.gov.hmrc.http.HeaderCarrier

class UpscanService(
  upscanConnector: UpscanConnector,
  upscanRepository: UpscanRepository,
  gformConnector: GformConnector,
  queryParameterCrypto: CryptoWithKeysFromConfig,
  appConfig: AppConfig
)(implicit ec: ExecutionContext)
    extends UpscanAlgebra[Future] {

  def upscanInitiate(
    fileUploadIds: List[FormComponentId],
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    form: Form,
    formIdData: FormIdData
  )(implicit
    hc: HeaderCarrier
  ): Future[UpscanInitiate] =
    fileUploadIds
      .traverse(formComponentId =>
        upscanConnector
          .upscanInitiate(
            toRequest(formTemplateId, sectionNumber, maybeAccessCode, formComponentId, form.envelopeId, formIdData)
          )
          .map(formComponentId -> _)
      )
      .flatMap { fcIdWithResponse =>
        val upscanInititate = UpscanInitiate(fcIdWithResponse.toMap)

        val formIdData: FormIdData = FormIdData.fromForm(form, maybeAccessCode)
        val userData: UserData = UserData(
          form.formData,
          form.status,
          form.visitsIndex,
          form.thirdPartyData,
          form.componentIdToFileId
        )

        gformConnector.updateUserData(formIdData, userData).map(_ => upscanInititate)

      }

  private def toRequest(
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    envelopeId: EnvelopeId,
    formIdData: FormIdData
  ): UpscanInitiateRequest = {

    val baseUrl = appConfig.`gform-frontend-base-url`
    val baseUrlInternal = appConfig.`gform-frontend-base-url-internal`

    val formIdDataString = Json.stringify(Json.toJson(formIdData))

    val formIdDataCrypted: Crypted = queryParameterCrypto.encrypt(PlainText(formIdDataString))

    val callback: String =
      baseUrlInternal + UpscanController.callback(formComponentId, envelopeId, formIdDataCrypted).url
    val successRedirect: String =
      baseUrl + UpscanController.success(formTemplateId, sectionNumber, maybeAccessCode, formComponentId).url
    val errorRedirect: String =
      baseUrl + UpscanController.error(formTemplateId, sectionNumber, maybeAccessCode, formComponentId).url

    UpscanInitiateRequest(
      callback,
      successRedirect,
      errorRedirect
    )
  }

  def download(
    downloadUrl: String
  ): Future[ByteString] = upscanConnector.download(downloadUrl)

  def retrieveConfirmationOrFail(reference: UpscanReference): Future[UpscanFileStatus] =
    upscanRepository.find(reference).flatMap {
      case Some(upscanConfirmation) => upscanConfirmation.status.pure[Future]
      case None                     => Future.failed(new Exception(s"No confirmation received yet for ${reference.value}"))
    }

  def retrieveConfirmation(reference: UpscanReference): Future[Option[UpscanFileStatus]] =
    upscanRepository.find(reference).map(_.map(_.status))

  def confirm(upscanCallbackSuccess: UpscanCallback.Success): Future[UpscanConfirmation] =
    upscanRepository.upsert(
      UpscanConfirmation(upscanCallbackSuccess.reference, upscanCallbackSuccess.fileStatus, Instant.now())
    )

  def reject(upscanCallbackFailure: UpscanCallback.Failure): Future[UpscanConfirmation] =
    upscanRepository.upsert(
      UpscanConfirmation(upscanCallbackFailure.reference, upscanCallbackFailure.fileStatus, Instant.now())
    )

  def deleteConfirmation(reference: UpscanReference): Future[Unit] =
    upscanRepository.delete(reference)
}
