/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.auth

import javax.inject.{ Inject, Singleton }

import play.api.libs.json.Json
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.sharedmodel.formtemplate.ServiceId
import uk.gov.hmrc.play.http.HeaderCarrier

@Singleton
class EnrolmentService @Inject() (
    configModule: ConfigModule, ggConnector: GovernmentGatewayConnector, taxEnrolmentConnector: TaxEnrolmentsConnector
) {

  def enrolUser(serviceId: ServiceId, identifiers: List[Identifier], verifiers: List[Verifier])(implicit hc: HeaderCarrier) = {
    val useTaxEnrolments = configModule.serviceConfig.getConfBool("enrolment-service.use-tax-enrolments", false)
    if (useTaxEnrolments) {
      val request = buildTaxEnrolmentsRequest(serviceId, identifiers, verifiers)
      taxEnrolmentConnector.enrolGGUser(request, serviceId)
    } else {
      val request = buildGGEnrolmentRequest(serviceId, serviceId.value, verifiers)
      ggConnector.enrolGGUser(request)
    }
  }

  private def buildGGEnrolmentRequest(serviceId: ServiceId, friendlyName: String, knownFacts: List[Verifier]) = {
    GGEnrolmentRequest(
      portalId = configModule.serviceConfig.getConfString("gg.enrol.portalId", ""),
      serviceName = serviceId.value,
      friendlyName = friendlyName,
      knownFacts = knownFacts.map(_.value)
    )
  }

  private def buildTaxEnrolmentsRequest(serviceId: ServiceId, identifiers: List[Identifier], verifiers: List[Verifier]) = {
    val taxEnrolment = TaxEnrolment(
      key = serviceId,
      identifiers = identifiers,
      verifiers = verifiers
    )
    TaxEnrolmentRequest(List(taxEnrolment))
  }
}

case class Identifier(key: String, value: String)

object Identifier {
  implicit val format = Json.format[Identifier]
}

case class Verifier(key: String, value: String)

object Verifier {
  implicit val format = Json.format[Verifier]
}
