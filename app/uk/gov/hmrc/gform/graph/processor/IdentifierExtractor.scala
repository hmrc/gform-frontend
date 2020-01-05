/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.graph.processor

import uk.gov.hmrc.auth.core
import uk.gov.hmrc.auth.core.{ EnrolmentIdentifier, Enrolments }
import uk.gov.hmrc.gform.auth.models.{ AuthenticatedRetrievals, MaterialisedRetrievals }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait IdentifierExtractor {

  def enrolmentIdentifierValue(retrievals: MaterialisedRetrievals, auth: EnrolmentAuth): String =
    retrievals match {
      case AuthenticatedRetrievals(_, enrolments, _, _, _, _, _) =>
        identifierValue(enrolments, auth)
      case _ => ""
    }

  def authorizedEnrolmentValue(enrolments: Enrolments, authConfig: AuthConfig) = authConfig match {
    case HmrcEnrolmentModule(auth)             => identifierValue(enrolments, auth)
    case HmrcAgentWithEnrolmentModule(_, auth) => identifierValue(enrolments, auth)
    case _                                     => ""
  }

  def identifierValue(enrolments: Enrolments, auth: EnrolmentAuth): String = auth match {
    case EnrolmentAuth(_, DoCheck(_, _, RegimeIdCheck(RegimeId(id)))) =>
      maybeValue(
        enrolmentsByServiceId(enrolments, auth.serviceId.value)
          .flatMap(_.identifiers)
          .find(_.value.drop(2).startsWith(id)))
    case _ =>
      maybeValue(
        enrolmentByServiceId(enrolments, auth.serviceId.value)
          .flatMap(_.identifiers.headOption))
  }

  def extractIdentifier(enrolments: Enrolments, serviceName: ServiceName, identifierName: IdentifierName): String =
    maybeValue(
      enrolmentByServiceId(enrolments, serviceName.value)
        .flatMap(_.getIdentifier(identifierName.value)))

  private val enrolmentByServiceId: (Enrolments, String) => Option[core.Enrolment] =
    (enrolments, serviceId) => enrolments.getEnrolment(serviceId)

  private val enrolmentsByServiceId: (Enrolments, String) => Set[core.Enrolment] =
    (enrolments, serviceId) => enrolments.enrolments.filter(_.key == serviceId)

  private val maybeValue: Option[EnrolmentIdentifier] => String =
    maybeIdentifier => maybeIdentifier.map(_.value).getOrElse("")
}
