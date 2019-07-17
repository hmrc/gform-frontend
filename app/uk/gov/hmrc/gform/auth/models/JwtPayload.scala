/*
 * Copyright 2019 HM Revenue & Customs
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

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{ JsPath, Reads }

/*
  These set of fields are common to the JWT coming from the public LB and the admin LB
 */
case class JwtPayload(
  sub: String,
  username: String,
  exp: Long,
  iss: String
)

object JwtPayload {
  implicit val format: Reads[JwtPayload] = (
    (JsPath \ "sub").read[String] and
      (JsPath \ "username").read[String] and
      (JsPath \ "exp").read[Long] and
      (JsPath \ "iss").read[String]
  )(JwtPayload.apply _)
}
