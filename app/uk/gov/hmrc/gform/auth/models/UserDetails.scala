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

import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.models.userdetails.AffinityGroup
import uk.gov.hmrc.gform.sharedmodel.UserId

case class UserDetails(userId: UserId, affinityGroup: AffinityGroup)

object UserDetails {
  implicit val format: OFormat[UserDetails] = Json.format[UserDetails]
}

case class UserDetailsResponse(groupIdentifier: String, affinityGroup: AffinityGroup) {
  def asUserDetails = UserDetails(UserId(groupIdentifier), affinityGroup)
}

object UserDetailsResponse {
  implicit val format: OFormat[UserDetailsResponse] = Json.format[UserDetailsResponse]
}
