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
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.models.eeitt.{ Agent, BusinessUser }
import uk.gov.hmrc.gform.models.userdetails.GroupId
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class EeittServiceSpec extends Spec with ExampleData {

  behavior of "Eeitt Service"

  val mockEeittConnector = new EeittConnector("", null) {
    override def prepopulationBusinessUser(groupId: GroupId, regimeId: RegimeId)(
      implicit hc: HeaderCarrier,
      ec: ExecutionContext): Future[BusinessUser] =
      Future.successful(BusinessUser("TESTREGNUM"))

    override def prepopulationAgent(groupId: GroupId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Agent] =
      Future.successful(Agent("TESTARN"))
  }

  val eeittService = new EeittService(mockEeittConnector)

  it should "return a eeitt business user" in {
    val result = call(uk.gov.hmrc.gform.sharedmodel.formtemplate.BusinessUser)
    result.futureValue should be("TESTREGNUM")
  }

  it should "return a eeitt agent" in new ExampleData {
    override def affinityGroup = Some(AffinityGroup.Agent)

    val result = call(uk.gov.hmrc.gform.sharedmodel.formtemplate.Agent, authContext)
    result.futureValue should be("TESTARN")
  }

  it should "return a eeitt user id" in {
    val result = call(uk.gov.hmrc.gform.sharedmodel.formtemplate.UserId)
    result.futureValue should be("TESTREGNUM")
  }

  it should "return a eeitt user id for agent" in new ExampleData {
    override def affinityGroup = Some(AffinityGroup.Agent)

    val result = call(uk.gov.hmrc.gform.sharedmodel.formtemplate.UserId, authContext)
    result.futureValue should be("TESTARN")
  }

  def call(eeitt: Eeitt, authContext: MaterialisedRetrievals = authContext) =
    eeittService.getValue(eeitt, authContext, formTemplate)

  implicit lazy val hc = HeaderCarrier()
}
