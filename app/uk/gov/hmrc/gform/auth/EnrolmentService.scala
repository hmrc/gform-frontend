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

package uk.gov.hmrc.gform.auth

import cats.data.NonEmptyList
import play.api.libs.json.Json
import uk.gov.hmrc.gform.sharedmodel.formtemplate.ServiceId
import uk.gov.hmrc.http.HttpResponse

trait EnrolmentConnect[F[_]] {
  def enrolGGUser(request: TaxEnrolment, service: ServiceId): F[HttpResponse]
}

trait GGConnect[F[_]] {
  def enrolGGUser(request: GGEnrolmentRequest): F[HttpResponse]
}

class EnrolmentService(
  useTaxEnrolments: Boolean,
  portalId: String
) {

  def enrolUser[F[_]](
    serviceId: ServiceId,
    identifiers: NonEmptyList[Identifier],
    verifiers: List[Verifier]
  )(
    implicit
    EC: EnrolmentConnect[F],
    GGC: GGConnect[F]
  ): F[HttpResponse] =
    if (useTaxEnrolments) {
      val request = buildTaxEnrolmentsRequest(identifiers, verifiers)
      EC.enrolGGUser(request, serviceId)
    } else {
      val request = buildGGEnrolmentRequest(serviceId, serviceId.value, identifiers, verifiers)
      GGC.enrolGGUser(request)
    }

  private def buildGGEnrolmentRequest(
    serviceId: ServiceId,
    friendlyName: String,
    identifiers: NonEmptyList[Identifier],
    knownFacts: List[Verifier]) =
    GGEnrolmentRequest(
      portalId = portalId,
      serviceName = serviceId.value,
      friendlyName = friendlyName,
      knownFacts = identifiers.map(_.value).toList ++ knownFacts.map(_.value)
    )

  private def buildTaxEnrolmentsRequest(identifiers: NonEmptyList[Identifier], verifiers: List[Verifier]) =
    TaxEnrolment(
      identifiers = identifiers.toList,
      verifiers = verifiers
    )
}

case class Identifier(key: String, value: String)

object Identifier {
  implicit val format = Json.format[Identifier]
}

case class Verifier(key: String, value: String)

object Verifier {
  implicit val format = Json.format[Verifier]
}
