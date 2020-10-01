/*
 * Copyright 2020 HM Revenue & Customs
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
import play.api.libs.json.{ Format, Json, OFormat }
import uk.gov.hmrc.gform.models.email.{ EmailFieldId, emailFieldId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, JsonUtils }
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, NotChecked, Obligations }
import uk.gov.hmrc.gform.sharedmodel.des.DesRegistrationResponse
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

case class ThirdPartyData(
  desRegistrationResponse: Option[DesRegistrationResponse],
  obligations: Obligations,
  emailVerification: Map[EmailFieldId, EmailAndCode],
  queryParams: QueryParams,
  reviewData: Option[Map[String, String]] = None,
  booleanExprCache: BooleanExprCache
) {

  def updateFrom(vr: Option[ValidatorsResult]): ThirdPartyData =
    vr match {
      case Some(ValidatorsResult(Some(desRegistrationResponse), m)) =>
        ThirdPartyData(
          Some(desRegistrationResponse),
          obligations,
          emailVerification ++ m,
          queryParams,
          reviewData,
          booleanExprCache)
      case Some(ValidatorsResult(None, m)) =>
        ThirdPartyData(
          desRegistrationResponse,
          obligations,
          emailVerification ++ m,
          queryParams,
          reviewData,
          booleanExprCache)
      case _ => this
    }

  def reviewComments: Option[String] = reviewData.flatMap(_.get("caseworkerComment"))
}

object ThirdPartyData {
  val empty = ThirdPartyData(None, NotChecked, Map.empty, QueryParams.empty, None, BooleanExprCache.empty)
  implicit val formatMap: Format[Map[EmailFieldId, EmailAndCode]] =
    JsonUtils.formatMap(a => emailFieldId(FormComponentId(a)), _.value)
  implicit val format: OFormat[ThirdPartyData] = Json.format
}
