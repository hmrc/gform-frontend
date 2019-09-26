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

package uk.gov.hmrc.gform.sharedmodel

import cats.Show
import play.api.libs.json._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals

case class UserId(value: String)

object UserId {

  def apply(materialisedRetrievals: MaterialisedRetrievals): UserId = UserId(materialisedRetrievals.groupId)

  val oformat: OFormat[UserId] = ValueClassFormat.oformat("userId", UserId.apply, _.value)
  implicit val vformat: OFormat[UserId] = ValueClassFormat.oformat("userId", UserId.apply, _.value)

  implicit val show: Show[UserId] = Show.show(_.value)

  //
  //  //TODO: move validation logic to auth service.
  //
  //  val reads: Reads[UserId] = Reads[UserId] {
  //    case JsString(str) => JsSuccess(UserId(str))
  //    case JsObject(obj) =>
  //      obj.get("groupIdentifier") match {
  //        case None => JsError(s"groupIdentifier expected inside the obj.")
  //        case Some(JsString(x)) => JsSuccess(UserId(x))
  //        case _ => JsError(s"groupIdentifier expected inside the obj.")
  //      }
  //    case unknown => JsError(s"JsString or JsObject value expected, got: $unknown")
  //  }
  //  val writes: Writes[UserId] = Writes[UserId](a => JsString(a.value))
  //
  //  implicit val format: Format[UserId] = Format[UserId](reads, writes)
  //
  //  val oformat: OFormat[UserId] = ValueClassFormat.oformat("userId", UserId.apply, _.value)

}
