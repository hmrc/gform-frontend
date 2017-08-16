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

package uk.gov.hmrc.gform.auth

import uk.gov.hmrc.auth.core.PlayAuthConnector
import uk.gov.hmrc.gform.auth.models.UserDetails
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.http.ws.WSHttp

import scala.concurrent.Future

class AuthConnector(baseUrl: String, wsHttp: WSHttp) extends PlayAuthConnector with ServicesConfig {
  val serviceUrl = baseUrl
  lazy val http = wsHttp

  def getUserDetails(uri: String)(implicit hc: HeaderCarrier): Future[UserDetails] = {
    http.GET[UserDetails](uri)
  }
}
