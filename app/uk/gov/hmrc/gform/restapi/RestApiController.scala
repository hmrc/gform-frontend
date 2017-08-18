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

package uk.gov.hmrc.gform.restapi

import javax.inject.Inject

import play.api.libs.json.Json
import play.api.mvc.{ Action, RequestHeader }
import uk.gov.hmrc.gform.controllers.ControllersModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormId }
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier

class RestApiController @Inject() (
  gformBackendModule: GformBackendModule,
  controllersModule: ControllersModule
)
    extends FrontendController {
  import uk.gov.hmrc.gform.controllers.AuthenticatedRequest._

  //TODO: use `ProxyActions` here which are backed by AkkaStreams thus we can benefit of fastter serving respons while it's still received from backend. No additional memory overhead will be engaged.
  //TODO: Make that `ProxyActions` rely on WsHttp which sets proper http headers according to tax platform standards and does proper auditing.

  def exposedConfig() = Action.async { implicit r =>
    gformConnector.getExposedConfig.map(ec => Ok(Json.toJson(ec)))
  }

  def deleteFile(formId: FormId, fileId: FileId) = authentication.async { implicit c =>
    gformConnector.deleteFile(formId, fileId).map(_ => NoContent)
  }

  private lazy val gformConnector = gformBackendModule.gformConnector
  private lazy val authentication = controllersModule.authenticatedRequestActions
}
