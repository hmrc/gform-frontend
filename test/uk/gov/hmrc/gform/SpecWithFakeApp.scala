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

package uk.gov.hmrc.gform

import org.scalatestplus.play.{ BaseOneServerPerTest, FakeApplicationFactory }
import play.api.ApplicationLoader.Context
import play.api.{ Application, Configuration, Environment }
import play.core.DefaultWebCommands
import uk.gov.hmrc.gform.wshttp.{ StubbedWSHttp, WSHttp, WSHttpModule }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

trait SpecWithFakeApp extends Spec with BaseOneServerPerTest with FakeApplicationFactory {

  def configurationOverridings: Map[String, String] = Map()

  override def fakeApplication(): Application = {
    val env: Environment = Environment.simple()
    val context: Context = Context(
      environment = env,
      sourceMapper = None,
      webCommands = new DefaultWebCommands(),
      initialConfiguration = Configuration.load(env)
    )
    val applicationModule = new ApplicationModule(context) {
      override lazy val httpFilters = Nil
      lazy val wSHttpModule = new WSHttpModule(auditingModule, configModule) {
        override val auditableWSHttp: WSHttp = new StubbedWSHttp(
          HttpResponse(200)
        )
      }
    }

    applicationModule.application
  }

  implicit lazy val hc = HeaderCarrier()
}
