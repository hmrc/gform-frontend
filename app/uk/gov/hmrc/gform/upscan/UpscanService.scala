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

import cats.implicits._
import play.api.libs.json.Json
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.crypto.{ Crypted, CryptoWithKeysFromConfig, PlainText }
import uk.gov.hmrc.gform.config.{ AppConfig, ConfigModule }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormIdData, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId, SectionNumber }
import uk.gov.hmrc.gform.upscan.routes.UpscanController
import uk.gov.hmrc.http.HeaderCarrier

class UpscanService(
  upscanConnector: UpscanConnector,
  gformConnector: GformConnector,
  queryParameterCrypto: CryptoWithKeysFromConfig,
  configModule: ConfigModule,
  appConfig: AppConfig
)(implicit ec: ExecutionContext)
    extends UpscanAlgebra[Future] {

  private val gformBaseUrl = configModule.serviceConfig.baseUrl("gform") + "/gform"

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

    val formIdDataString = Json.stringify(Json.toJson(formIdData))

    val formIdDataCrypted: Crypted = queryParameterCrypto.encrypt(PlainText(formIdDataString))

    val callback0 = java.net.URLEncoder.encode(formIdDataCrypted.value, "UTF-8")
    val callback: String =
      gformBaseUrl + s"/upscan/callback/${formComponentId.value}/${envelopeId.value}?formIdDataCrypted=$callback0"

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

  def retrieveConfirmationOrFail(reference: UpscanReference)(implicit hc: HeaderCarrier): Future[UpscanFileStatus] =
    retrieveConfirmation(reference).flatMap {
      case Some(status) => status.pure[Future]
      case None         => Future.failed(new Exception(s"No confirmation received yet for ${reference.value}"))
    }

  def retrieveConfirmation(reference: UpscanReference)(implicit hc: HeaderCarrier): Future[Option[UpscanFileStatus]] =
    gformConnector.retrieveConfirmation(reference)

  def deleteConfirmation(reference: UpscanReference)(implicit hc: HeaderCarrier): Future[Unit] =
    gformConnector.deleteUpscanReference(reference)
}
