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
import play.api.libs.json.{ JsError, JsSuccess, Reads }
import play.api.mvc.Result
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.gformbackend.model.FormTypeId
import uk.gov.hmrc.gform.models.UserId
import uk.gov.hmrc.gform.models.userdetails.AffinityGroup
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

case class UserDetails(userId: UserId, affinityGroup: AffinityGroup)

object UserDetails {
  implicit val reads: Reads[UserDetails] = Reads[UserDetails] { x =>
    Logger.debug("JS" + x)
    x.asOpt[UserId] match {
      case Some(userId) =>
        x.asOpt[AffinityGroup] match {
          case Some(affinGroup) => JsSuccess(UserDetails(userId, affinGroup))
          case None => JsError("SomeError")
        }
      case None => JsError("ErroSome")
    }
  }
}

class EeittAuth @Inject() (
    eeittConnector: EeittConnector,
    authConnector: AuthConnector,
    eeittUrl: String,
    baseUrl: String
) {

  def legacyAuth(formTypeId: FormTypeId, action: UserId => Future[Result])(implicit authContext: AuthContext, hc: HeaderCarrier, ex: ExecutionContext) = {
    for {
      userDetails <- authConnector.getUserDetails[UserDetails](authContext)
      isOk <- eeittConnector.isAllowed(userDetails.userId.value, formTypeId, userDetails.affinityGroup)
      x <- somewhere(isOk.isAllowed, formTypeId, userDetails.userId, action)
    } yield x
  }

  private def somewhere(isAllowed: Boolean, formTypeId: FormTypeId, userId: UserId, action: UserId => Future[Result]) = {
    if (!isAllowed)
      redirectToEeitt(formTypeId)
    else
      action(userId)
  }

  private def redirectToEeitt(formTypeId: FormTypeId) = {
    Future.successful(Redirect(s"http://localhost:9190/eeitt-auth/enrollment-verification?callbackUrl=http://$baseUrl/submissions/new-form/$formTypeId"))
  }
}
