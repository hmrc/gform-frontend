/*
 * Copyright 2019 HM Revenue & Customs
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

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.upscan.model.UpscanMeta
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class UpscanConnectorSpec extends Spec {

  behavior of "Upscan Service"

  implicit lazy val hc = HeaderCarrier()

  //TODO: add tests when connector is integrated into service

  val mockUpscanInitiateConnector = new UpscanConnector("", null) {
    override def initiate(formId: String, fieldId: String)(
      implicit hc: HeaderCarrier,
      ec: ExecutionContext): Future[UpscanMeta] =
      Future.successful(
        UpscanMeta(
          "upscan-ref",
          "http://some-href",
          "some-acl",
          "some-key",
          "some-policy",
          "some-algo",
          "some-cred",
          "some-data",
          "some-meta-call-back",
          "some-consuming-service",
          "some-signature"
        )
      )
  }

  val upscanService = new UpscanService(mockUpscanInitiateConnector)

  def call(formId: String, fieldId: String) = upscanService.initiate(formId, fieldId)

  it should "return upscan form meta data" in {
    val result: Future[UpscanMeta] = call("some-form-id", "some-field-id")
    result.futureValue should be(
      UpscanMeta(
        "upscan-ref",
        "http://some-href",
        "some-acl",
        "some-key",
        "some-policy",
        "some-algo",
        "some-cred",
        "some-data",
        "some-meta-call-back",
        "some-consuming-service",
        "some-signature"
      ))
  }

}
