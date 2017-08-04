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

package uk.gov.hmrc.gform.connectors

import play.api.libs.json.{ Json, OFormat }
import play.utils.UriEncoding
import uk.gov.hmrc.gform.models.eeitt.{ Agent, BusinessUser }
import uk.gov.hmrc.gform.models.userdetails.{ AffinityGroup, GroupId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, RegimeId }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

case class Verification(isAllowed: Boolean)

object Verification {
  implicit val format: OFormat[Verification] = Json.format[Verification]
}

class EeittConnector(baseUrl: String, wSHttp: WSHttp) {

  def isAllowed(groupId: String, regimeId: RegimeId, affinityGroup: AffinityGroup)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Verification] =
    wSHttp.GET[Verification](baseUrl + s"/group-id/${encode(groupId)}/regime/${regimeId.value}/affinityGroup/${encode(affinityGroup.toString)}/verification")

  def prepopulationBusinessUser(groupId: GroupId, regimeId: FormTemplateId)(implicit hc: HeaderCarrier): Future[BusinessUser] = {
    wSHttp.GET[BusinessUser](baseUrl + s"/group-id/${groupId.value}/regime/${regimeId.value}/prepopulation")
  }

  def prepopulationAgent(groupId: GroupId)(implicit hc: HeaderCarrier): Future[Agent] = {
    wSHttp.GET[Agent](baseUrl + s"/group-id/${groupId.value}/prepopulation")
  }

  private def encode(p: String) = UriEncoding.encodePathSegment(p, "UTF-8")
}
