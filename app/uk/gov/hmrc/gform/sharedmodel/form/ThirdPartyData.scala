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

package uk.gov.hmrc.gform.sharedmodel.form

import cats.data.Validated.Valid
import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.des.DesRegistrationResponse
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

case class ThirdPartyData(
  desRegistrationResponse: Option[DesRegistrationResponse]
) {
  def updateFrom(vr: ValidatedType[ValidationResult]): ThirdPartyData =
    (vr, desRegistrationResponse) match {
      case (Valid(ValidationResult(Some(desRegistrationResponse))), _) => ThirdPartyData(Some(desRegistrationResponse))
      case _                                                           => this
    }
}

object ThirdPartyData {
  val empty = ThirdPartyData(None)
  implicit val format: OFormat[ThirdPartyData] = Json.format

}
