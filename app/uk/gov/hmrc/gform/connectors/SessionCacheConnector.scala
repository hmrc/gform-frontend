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

import javax.inject.Singleton

import uk.gov.hmrc.gform.{ FrontendAppConfig, WSHttp }
import uk.gov.hmrc.http.cache.client.SessionCache
import uk.gov.hmrc.play.config.{ AppName, ServicesConfig }
import uk.gov.hmrc.play.http.{ HttpDelete, HttpGet, HttpPut }

@Singleton
class SessionCacheConnector extends SessionCache with ServicesConfig with AppName {

  override def defaultSource: String = appName

  override def baseUri: String = FrontendAppConfig.sessionCacheBaseUri

  override def domain: String = FrontendAppConfig.sessionCacheDomain

  override def http: HttpGet with HttpPut with HttpDelete = WSHttp
}
