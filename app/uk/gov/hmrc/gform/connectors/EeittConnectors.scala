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

import play.api.libs.json.{ JsObject, JsValue, Json }
import play.api.mvc.Action
import uk.gov.hmrc.gform.WSHttp
import uk.gov.hmrc.gform.models.eeitt.{ Agent, BusinessUser }
import uk.gov.hmrc.gform.models.form.FormTypeId
import uk.gov.hmrc.gform.models.userdetails.GroupId
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpGet, HttpPost, HttpPut, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }

trait EeittConnector {

  def httpGet: HttpGet

  def eeittUrl: String

  def prepopulationBusinessUser(groupId: GroupId, formTypeId: FormTypeId)(implicit hc: HeaderCarrier): Future[BusinessUser] = {
    httpGet.GET[BusinessUser](eeittUrl + s"/group-id/${groupId.value}/regime/${formTypeId.value}/prepopulation")
  }

  def prepopulationAgent(groupId: GroupId)(implicit hc: HeaderCarrier): Future[Agent] = {
    httpGet.GET[Agent](eeittUrl + s"/group-id/${groupId.value}/prepopulation")
  }
}

object EeittConnector extends EeittConnector with ServicesConfig {

  lazy val httpGet = WSHttp

  def eeittUrl: String = s"${baseUrl("eeitt")}/eeitt"

}
