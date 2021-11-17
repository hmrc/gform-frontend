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
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.crypto.{ Crypted, CryptoWithKeysFromConfig }
import uk.gov.hmrc.gform.config.{ AppConfig, ConfigModule }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormIdData }
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
    form: Form,
    formIdData: FormIdData
  )(implicit
    hc: HeaderCarrier
  ): Future[UpscanInitiate] =
    for {
      formIdDataCrypted <- gformConnector.upscanEncrypt(formIdData)
      fcIdWithResponse <-
        fileUploadIds.traverse(formComponentId =>
          upscanConnector
            .upscanInitiate(
              toRequest(
                formTemplateId,
                sectionNumber,
                formComponentId,
                form.envelopeId,
                formIdData,
                formIdDataCrypted
              )
            )
            .map(formComponentId -> _)
        )
    } yield UpscanInitiate(fcIdWithResponse.toMap)

  private def toRequest(
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    formComponentId: FormComponentId,
    envelopeId: EnvelopeId,
    formIdData: FormIdData,
    formIdDataCrypted: Crypted
  ): UpscanInitiateRequest = {

    val baseUrl = appConfig.`gform-frontend-base-url`

    val callback0 = java.net.URLEncoder.encode(formIdDataCrypted.value, "UTF-8")
    val callback: String =
      gformBaseUrl + s"/upscan/callback/${formComponentId.value}/${envelopeId.value}?formIdDataCrypted=$callback0"

    val successRedirect: String =
      baseUrl + UpscanController.success(formTemplateId, sectionNumber, formIdData.maybeAccessCode, formComponentId).url
    val errorRedirect: String =
      baseUrl + UpscanController.error(formTemplateId, sectionNumber, formIdData.maybeAccessCode, formComponentId).url

    UpscanInitiateRequest(
      callback,
      successRedirect,
      errorRedirect
    )
  }

  def retrieveConfirmationOrFail(reference: UpscanReference)(implicit hc: HeaderCarrier): Future[UpscanConfirmation] =
    retrieveConfirmation(reference).flatMap {
      case Some(status) => status.pure[Future]
      case None         => Future.failed(new Exception(s"No confirmation received yet for ${reference.value}"))
    }

  def retrieveConfirmation(reference: UpscanReference)(implicit hc: HeaderCarrier): Future[Option[UpscanConfirmation]] =
    gformConnector.retrieveConfirmation(reference)

  def deleteConfirmation(reference: UpscanReference)(implicit hc: HeaderCarrier): Future[Unit] =
    gformConnector.deleteUpscanReference(reference)
}
