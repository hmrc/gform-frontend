/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.gform

import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.models.userdetails.GroupId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.Future

class EeittService(eeittConnector: EeittConnector) {

  def getValue(eeitt: Eeitt, retrievals: Retrievals, formTemplate: FormTemplate)(implicit hc: HeaderCarrier) = {
    val regimeId = formTemplate.authConfig match {
      case EEITTAuthConfig(_, rId) => rId
      case _                       => RegimeId("")
    }
    for {
      prepopData <- (eeitt, retrievals.affinityGroup) match {
                     case (Agent, Some(AffinityGroup.Agent)) | (UserId, Some(AffinityGroup.Agent)) =>
                       eeittConnector.prepopulationAgent(GroupId(retrievals.userDetails.groupIdentifier)).map(_.arn)
                     case (BusinessUser, Some(AffinityGroup.Agent)) =>
                       Future.successful("")
                     case (BusinessUser, _) | (UserId, _) =>
                       eeittConnector
                         .prepopulationBusinessUser(GroupId(retrievals.userDetails.groupIdentifier), regimeId)
                         .map(_.registrationNumber)
                     case _ =>
                       Future.successful("")
                   }
    } yield prepopData
  }
}
