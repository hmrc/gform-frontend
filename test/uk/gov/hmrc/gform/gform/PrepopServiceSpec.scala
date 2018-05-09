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
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.cache.client.CacheMap

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

class PrepopServiceSpec extends Spec with ExampleData {

  behavior of "Prepop Service"

  val mockAuthPrepop = new AuthContextPrepop {
    override def values(value: AuthInfo, retrievals: Retrievals): String =
      "TESTSTRING"
  }

  val mockRepeatingGroupService = new RepeatingComponentService(null, null) {
    override def atomicFields(section: BaseSection)(implicit hc: HeaderCarrier, ec: ExecutionContext): List[FormComponent] =
      `section - about you`.fields

    override def getAllRepeatingGroups(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[CacheMap] =
      Future.successful(CacheMap("SOMESESSIONID", Map("Hello" -> Json.toJson(List(`section - about you`.fields)))))
  }

  val prepopService = new PrepopService(mockAuthPrepop, mockRepeatingGroupService, new EeittService(new EeittConnector("", null)))

  it should "return a auth number in the pattern matching" in {
    val result = call(AuthCtx(GG))
    result.futureValue should be("TESTSTRING")
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
    prepopService.prepopData(expr, formTemplate, authContext, rawDataFromBrowser, `section - about you`)

  implicit lazy val hc = HeaderCarrier()
}
