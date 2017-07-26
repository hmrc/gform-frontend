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

package uk.gov.hmrc.gform.gformbackend.model

import play.api.libs.json._

case class AuthConfig(
  authModule: AuthConfigModule,
  predicates: Option[List[Predicate]],
  regimeId: RegimeId
)

object AuthConfig {

  implicit val format: OFormat[AuthConfig] = Json.format[AuthConfig]
}

case class RegimeId(value: String) {
  override def toString: String = value
}

object RegimeId {

  val writes = Writes[RegimeId](id => JsString(id.value))
  val reads = Reads[RegimeId] {
    case JsString(value) => JsSuccess(RegimeId(value))
    case otherwise => JsError(s"Invalid RegimeId, expected JsString, got: $otherwise")
  }

  implicit val format: Format[RegimeId] = Format[RegimeId](reads, writes)
}

case class AuthConfigModule(value: String) {
  override def toString: String = value
}

object AuthConfigModule {
  val writes = Writes[AuthConfigModule](id => JsString(id.value))
  val reads = Reads[AuthConfigModule] {
    case JsString(value) => JsSuccess(AuthConfigModule(value))
    case otherwise => JsError(s"Invalid AuthModule, expected JsString, got: $otherwise")
  }

  val legacyEEITTAuth = "legacyEEITTAuth"

  implicit val format: Format[AuthConfigModule] = Format[AuthConfigModule](reads, writes)
}

case class Predicate(enrolment: String, identifiers: List[KeyValue], delegatedAuthRule: String)

object Predicate {
  implicit val format: OFormat[Predicate] = Json.format[Predicate]
}

case class KeyValue(key: String, value: String)

object KeyValue {
  implicit val format: OFormat[KeyValue] = Json.format[KeyValue]
}