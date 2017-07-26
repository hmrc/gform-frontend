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

package uk.gov.hmrc.gform.auth.models

import play.api.libs.json.{ JsError, JsSuccess, Reads }
import uk.gov.hmrc.gform.models.UserId
import uk.gov.hmrc.gform.models.userdetails.AffinityGroup

case class UserDetails(userId: UserId, affinityGroup: AffinityGroup)

object UserDetails {
  implicit val reads: Reads[UserDetails] = Reads[UserDetails] { x =>
    x.asOpt[UserId] match {
      case Some(userId) =>
        x.asOpt[AffinityGroup] match {
          case Some(affinGroup) => JsSuccess(UserDetails(userId, affinGroup))
          case None => JsError("No AffinityGroup is present")
        }
      case None => JsError("No UserId is present")
    }
  }
}
