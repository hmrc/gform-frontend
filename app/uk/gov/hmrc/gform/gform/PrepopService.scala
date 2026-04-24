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
import play.api.i18n.Messages
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.mappings.{ HMRCOBTDSORG, IRCT, IRSA, NINO, VRN }
import uk.gov.hmrc.gform.recalculation.EvaluationStatus
import uk.gov.hmrc.gform.recalculation.EvaluationStatus.StringResult
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.auth.models.ItmpRetrievals

object AuthContextPrepop {
  def values(value: AuthInfo, retrievals: MaterialisedRetrievals, itmpRetrievals: Option[ItmpRetrievals])(implicit
    messages: Messages
  ): EvaluationStatus = value match {
    case AuthInfo.GG                     => StringResult(retrievals.ggCredId)
    case AuthInfo.PayeNino               => StringResult(retrievals.getTaxIdValue(NINO()))
    case AuthInfo.SaUtr                  => StringResult(retrievals.getTaxIdValue(IRSA()))
    case AuthInfo.CtUtr                  => StringResult(retrievals.getTaxIdValue(IRCT()))
    case AuthInfo.EtmpRegistrationNumber => StringResult(retrievals.getTaxIdValue(HMRCOBTDSORG()))
    case AuthInfo.EmailId                => StringResult(retrievals.getEmail.toString)
    case AuthInfo.ItmpName               => StringResult(getItmpName(itmpRetrievals))
    case AuthInfo.ItmpNameLens(focus)    => StringResult(getItmpNameFocus(itmpRetrievals, focus))
    case AuthInfo.ItmpDateOfBirth        => StringResult(getItmpDateOfBirth(itmpRetrievals))
    case AuthInfo.ItmpAddress            => getItmpAddress(itmpRetrievals)
    case AuthInfo.PayeRef                => StringResult(retrievals.getPayeRef)
    case AuthInfo.Vrn                    => StringResult(retrievals.getTaxIdValue(VRN()))
  }

  private def getItmpName(itmpRetrievals: Option[ItmpRetrievals]): String =
    itmpRetrievals.flatMap(_.itmpName).map(n => concat(n.givenName, n.middleName, n.familyName)).getOrElse("")

  private def getItmpNameFocus(itmpRetrievals: Option[ItmpRetrievals], focus: ItmpNameFocus): String =
    itmpRetrievals
      .flatMap(_.itmpName)
      .flatMap { n =>
        focus match {
          case ItmpNameFocus.GivenName  => n.givenName
          case ItmpNameFocus.MiddleName => n.middleName
          case ItmpNameFocus.FamilyName => n.familyName
        }
      }
      .getOrElse("")

  private def getItmpDateOfBirth(itmpRetrievals: Option[ItmpRetrievals])(implicit messages: Messages): String =
    itmpRetrievals.flatMap(_.itmpDateOfBirth).fold("") { ld =>
      EvaluationStatus.DateResult.mkDate(ld).asString
    }

  def getItmpAddress(itmpRetrievals: Option[ItmpRetrievals]): EvaluationStatus =
    EvaluationStatus.AddressResult(
      itmpRetrievals
        .flatMap(_.itmpAddress)
        .map { itmpAddress =>
          val joinLines45: Option[String] =
            itmpAddress.line4.map(formatAddressLine(_) + " ") |+| itmpAddress.line5.map(formatAddressLine)
          List(
            itmpAddress.line1.map(formatAddressLine).map(Address.street1       -> _),
            itmpAddress.line2.map(formatAddressLine).map(Address.street2       -> _),
            itmpAddress.line3.map(formatAddressLine).map(Address.street3       -> _),
            joinLines45.map(Address.street4                                    -> _),
            itmpAddress.postCode.map(Address.postcode                          -> _),
            itmpAddress.countryName.map(formatCountryName).map(Address.country -> _)
          ).collect {
            case Some((atom, entry)) if entry.trim.nonEmpty => atom -> entry
          }
        }
        .getOrElse(List.empty[(Atom, String)])
    )

  private def formatAddressLine(s: String) = if (s.exists(_.isLower)) capitalizeAll(s) else capitalizeAll(s.toLowerCase)
  private def formatCountryName(s: String) = if (s.length > 3) capitalizeAll(s.toLowerCase) else s
  private def capitalizeAll(s: String) = s.split(' ').map(_.capitalize).mkString(" ")

  private def concat(xs: Option[String]*): String = {
    val values = xs.collect {
      case Some(s) if s.nonEmpty => s
    }
    values.mkString(" ")
  }
}
