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

package uk.gov.hmrc.gform.controllers.testonly

import javax.inject._

import akka.stream.scaladsl.Source
import akka.util.ByteString
import play.api.mvc._
import uk.gov.hmrc.gform.controllers.helpers.ProxyActions
import uk.gov.hmrc.play.config.ServicesConfig

@Singleton
class TestOnly @Inject()(proxy: ProxyActions ) extends Controller with ServicesConfig {

  def proxyToGform(path: String): Action[Source[ByteString, _]] = proxy(gformBaseUrl)(path)

  private lazy val gformBaseUrl = baseUrl("gform")
}
