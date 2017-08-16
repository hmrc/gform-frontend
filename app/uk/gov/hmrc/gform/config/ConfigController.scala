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

package uk.gov.hmrc.gform.config

import javax.inject.Inject

import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.Results._
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext.global

class ConfigController @Inject() (gformBackendModule: GformBackendModule) extends FrontendController {

  def exposedConfig() = Action.async { implicit r =>
    gformConnector.getExposedConfig.map(ec => Ok(Json.toJson(ec)))
  }

  private lazy val gformConnector = gformBackendModule.gformConnector
}
