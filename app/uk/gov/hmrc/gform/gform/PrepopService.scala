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

import play.api.i18n.Messages
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.models.mappings.{ HMRCOBTDSORG, IRCT, IRSA, NINO }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.auth.models.ItmpRetrievals
import uk.gov.hmrc.gform.eval.ExpressionResult

object AuthContextPrepop {
  def values(value: AuthInfo, retrievals: MaterialisedRetrievals, itmpRetrievals: Option[ItmpRetrievals])(implicit
    messages: Messages
  ): String = value match {
    case AuthInfo.GG                     => retrievals.ggCredId
    case AuthInfo.PayeNino               => retrievals.getTaxIdValue(NINO())
    case AuthInfo.SaUtr                  => retrievals.getTaxIdValue(IRSA())
    case AuthInfo.CtUtr                  => retrievals.getTaxIdValue(IRCT())
    case AuthInfo.EtmpRegistrationNumber => retrievals.getTaxIdValue(HMRCOBTDSORG())
    case AuthInfo.EmailId                => retrievals.getEmail.toString
    case AuthInfo.Name                   => retrievals.getName.toString
    case AuthInfo.ItmpName               => getItmpName(itmpRetrievals)
    case AuthInfo.ItmpDateOfBirth        => getItmpDateOfBirth(itmpRetrievals)
    case AuthInfo.ItmpAddress            => getItmpAddress(itmpRetrievals)
  }

  private def getItmpName(itmpRetrievals: Option[ItmpRetrievals]): String =
    itmpRetrievals.flatMap(_.itmpName).map(n => concat(n.givenName, n.middleName, n.familyName)).getOrElse("")

  private def getItmpDateOfBirth(itmpRetrievals: Option[ItmpRetrievals])(implicit messages: Messages): String =
    itmpRetrievals.flatMap(_.itmpDateOfBirth).fold("") { ld =>
      ExpressionResult.DateResult(ld).asString
    }

  private def getItmpAddress(itmpRetrievals: Option[ItmpRetrievals]): String =
    itmpRetrievals
      .flatMap(_.itmpAddress)
      .map(a => concat(a.line1, a.line2, a.line3, a.line4, a.line5, a.postCode, a.countryName))
      .getOrElse("")

  private def concat(xs: Option[String]*): String = {
    val values = xs.collect {
      case Some(s) if s.nonEmpty => s
    }
    values.mkString(" ")
  }
}
