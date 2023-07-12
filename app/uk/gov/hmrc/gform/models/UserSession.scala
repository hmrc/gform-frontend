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

package uk.gov.hmrc.gform.models

import play.api.libs.json.{ JsValue, Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.AffinityGroup

case class UserSession(
  clientIp: String,
  deviceId: String,
  userAgent: String,
  relativePath: String,
  credentialId: String,
  affinityGroup: Option[AffinityGroup],
  authEmail: String,
  authPhone: String,
  enrolments: List[JsValue]
)

object UserSession {
  val empty = UserSession("", "", "", "", "", None, "", "", Nil)
  implicit val oFormat: OFormat[UserSession] = Json.format[UserSession]
}
