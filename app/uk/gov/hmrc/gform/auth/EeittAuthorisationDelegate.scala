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

import javax.inject.Inject

import play.api.Logger
import play.api.libs.json.{JsError, JsSuccess, Reads}
import play.api.mvc.Result
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.connectors.{EeittConnector, Verification}
import uk.gov.hmrc.gform.gformbackend.model.{FormTemplate, FormTypeId, RegimeId}
import uk.gov.hmrc.gform.models.UserId
import uk.gov.hmrc.gform.models.userdetails.AffinityGroup
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class EeittAuthorisationDelegate(
    eeittConnector: EeittConnector,
    appConfig: AppConfig
) {

  def legacyAuth(formTemplate: FormTemplate, userDetails: UserDetails)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[AuthResult] =
    for {
      isOk <- eeittConnector.isAllowed(userDetails.userId.value, formTemplate.authConfig.regimeId, userDetails.affinityGroup)
    } yield isAuthed(isOk, formTemplate.formTypeId, userDetails.userId)

  private def isAuthed(isOk: Verification, formTypeId: FormTypeId, userId: UserId): AuthResult = {
    if (!isOk.isAllowed)
      NeedsAuthenticated
    else
      Authenticated
  }
}