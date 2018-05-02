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

import play.api.libs.json.Json
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.models.eeitt.{ Agent, BusinessUser }
import uk.gov.hmrc.gform.models.userdetails.GroupId
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.cache.client.CacheMap

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

class PrepopServiceSpec extends Spec with ExampleData {

  behavior of "Prepop Service"

  val mockEeittConnector = new EeittConnector("", null) {
    override def prepopulationBusinessUser(groupId: GroupId, regimeId: RegimeId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[BusinessUser] =
      Future.successful(BusinessUser("TESTREGNUM"))

    override def prepopulationAgent(groupId: GroupId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Agent] =
      Future.successful(Agent("TESTARN"))
  }

  val mockAuthPrepop = new AuthContextPrepop {
    override def values(value: AuthInfo, retrievals: Retrievals): String =
      "TESTSTRING"
  }

  val mockRepeatingGroupService = new RepeatingComponentService(null, null) {
    override def getCache(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CacheMap]] = Future.successful(None)

    override def atomicFields(repeatCache: Future[Option[CacheMap]])(section: BaseSection)(implicit hc: HeaderCarrier, ec: ExecutionContext): List[FormComponent] =
      `section - about you`.fields

    override def getAllRepeatingGroups(repeatCache: Future[Option[CacheMap]])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[CacheMap] =
      Future.successful(CacheMap("SOMESESSIONID", Map("Hello" -> Json.toJson(List(`section - about you`.fields)))))
  }

  val prepopService = new PrepopService(mockEeittConnector, mockAuthPrepop, mockRepeatingGroupService)

  it should "return a auth number in the pattern matching" in {
    val result = call(AuthCtx(GG))
    result.futureValue should be("TESTSTRING")
  }

  it should "return a eeitt business user" in {
    val result = call(EeittCtx(uk.gov.hmrc.gform.sharedmodel.formtemplate.BusinessUser))
    result.futureValue should be("TESTREGNUM")
  }

  it should "return a eeitt agent" in new ExampleData {
    override def affinityGroup = Some(AffinityGroup.Agent)

    val result = call(EeittCtx(uk.gov.hmrc.gform.sharedmodel.formtemplate.Agent), authContext)
    result.futureValue should be("TESTARN")
  }

  it should "return a eeitt user id" in {
    val result = call(EeittCtx(uk.gov.hmrc.gform.sharedmodel.formtemplate.UserId))
    result.futureValue should be("TESTREGNUM")
  }

  it should "return a eeitt user id for agent" in new ExampleData {
    override def affinityGroup = Some(AffinityGroup.Agent)

    val result = call(EeittCtx(uk.gov.hmrc.gform.sharedmodel.formtemplate.UserId), authContext)
    result.futureValue should be("TESTARN")
  }

  // TODO: need to test user.affinityGroup

  it should "return a value" in {
    val result = call(FormCtx(`formField - number`.id.value))
    result.futureValue should be("1,234")
  }

  it should "return an added value between" in {
    val result = call(Add(FormCtx(`formField - number`.id.value), FormCtx(`formField - number`.id.value)))
    result.futureValue should be("2468")
  }

  def call(expr: Expr, authContext: Retrievals = authContext) =
    prepopService.prepopData(expr, formTemplate, authContext, rawDataFromBrowser, mockRepeatingGroupService.getCache, `section - about you`)

  implicit lazy val hc = HeaderCarrier()
}
