/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import org.scalatestplus.play.{ BaseOneServerPerSuite, FakeApplicationFactory }
import play.api.ApplicationLoader.Context
import play.api._
import uk.gov.hmrc.gform.wshttp.{ StubbedHttpClientV2, WSHttpModule }
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

trait SpecWithFakeApp extends Spec with BaseOneServerPerSuite with FakeApplicationFactory {

  def configurationOverridings: Map[String, String] = Map()

  private val env: Environment = Environment.simple(mode = Mode.Test)
  private val context: Context = Context.create(env)

  lazy val applicationModule = new ApplicationModule(context) {
    override val httpFilters = Nil
    override lazy val wSHttpModule = new WSHttpModule(null) {
      override def httpClient: HttpClientV2 = new StubbedHttpClientV2(
        HttpResponse(200, null)
      )
    }
  }

  implicit lazy val hc: HeaderCarrier = HeaderCarrier()

  override def fakeApplication(): Application = applicationModule.application
}
