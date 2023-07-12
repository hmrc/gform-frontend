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

package uk.gov.hmrc.gform.gform

import cats.implicits._
import cats.Monad
import play.api.mvc.Request
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.models.UserSession
import uk.gov.hmrc.http.HeaderCarrier

object UserSessionBuilder {
  def apply[F[_]: Monad](cache: AuthCacheWithForm)(implicit hc: HeaderCarrier, request: Request[_]): F[UserSession] = {
    val userAgent = request.headers.get("User-Agent").getOrElse("")
    val deviceId = hc.deviceID.getOrElse("")
    val clientId = hc.trueClientIp.getOrElse("")
    val credentialId = cache.retrievals.ggCredId
    val authEmail = cache.retrievals.getEmail.toString
    val enrolments = cache.retrievals.getEnrolments.map(_.toJson)
    val affinityGroup = cache.retrievals.getAffinityGroup
    val userSession = new UserSession(
      clientId,
      deviceId,
      userAgent,
      "/submissions",
      credentialId,
      affinityGroup,
      authEmail,
      "", //authPhone does not exist in Gov Gtwy account
      enrolments
    )
    userSession.pure[F]
  }
}
