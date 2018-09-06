/*
 * Copyright 2018 HM Revenue & Customs
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

import play.api.Logger
import uk.gov.hmrc.auth.core.retrieve.GGCredId
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals._
import uk.gov.hmrc.gform.commons.BigDecimalUtil.toBigDecimalDefault
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import cats.implicits._
import uk.gov.hmrc.gform.keystore.RepeatingComponentService

import scala.concurrent.Future
import scala.util.control.NonFatal
import uk.gov.hmrc.http.HeaderCarrier

import scala.math.BigDecimal.RoundingMode

class AuthContextPrepop {
  def values(value: AuthInfo, retrievals: MaterialisedRetrievals): String = value match {
    case GG                     => getGGCredId(retrievals)
    case PayeNino               => getTaxIdValue(None, "NINO", retrievals)
    case SaUtr                  => getTaxIdValue(Some("IR-SA"), "UTR", retrievals)
    case CtUtr                  => getTaxIdValue(Some("IR-CT"), "UTR", retrievals)
    case EtmpRegistrationNumber => getTaxIdValue(Some("HMRC-OBTDS-ORG"), "EtmpRegistrationNumber", retrievals)
  }

  private def getGGCredId(retrievals: MaterialisedRetrievals) = retrievals.authProviderId match {
    case GGCredId(credId) => credId
    case _                => ""
  }
}

class PrepopService(
  authContextPrepop: AuthContextPrepop,
  eeittService: EeittService
) {

  def prepopData(
    expr: Expr,
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    data: Map[FormComponentId, Seq[String]],
    section: BaseSection,
    scale: Option[Int] = None)(implicit hc: HeaderCarrier): Future[String] = {

    def round(x: BigDecimal): BigDecimal = scale match {
      case Some(s) => x.setScale(s, RoundingMode.FLOOR)
      case None    => x
    }

    expr match {
      case AuthCtx(value)  => Future.successful(authContextPrepop.values(value, retrievals))
      case Constant(value) => Future.successful(value)
      case EeittCtx(eeitt) => eeittPrepop(eeitt, retrievals, formTemplate)
      case UserCtx(_)      => Future.successful(retrievals.affinityGroupName)
      case Add(field1, field2) =>
        val value = for {
          y <- prepopData(field1, formTemplate, retrievals, data, section)
          z <- prepopData(field2, formTemplate, retrievals, data, section)
        } yield toBigDecimalDefault(y) + toBigDecimalDefault(z)
        value.map(x => round(x).toString)
      case Subtraction(field1, field2) =>
        val value = for {
          y <- prepopData(field1, formTemplate, retrievals, data, section)
          z <- prepopData(field2, formTemplate, retrievals, data, section)
        } yield toBigDecimalDefault(y) - toBigDecimalDefault(z)
        value.map(x => round(x).toString)
      case Multiply(field1, field2) =>
        val value = for {
          y <- prepopData(field1, formTemplate, retrievals, data, section)
          z <- prepopData(field2, formTemplate, retrievals, data, section)
        } yield toBigDecimalDefault(y) * toBigDecimalDefault(z)
        value.map(x => round(x).toString)
      case Sum(ctx @ FormCtx(_)) =>
        val x = RepeatingComponentService.sumFunctionality(ctx, formTemplate, data)
        Future.successful(round(x).toString)
      case id: FormCtx => data.get(id.toFieldId).map(_.head).getOrElse("").pure[Future]
      case _           => Future.successful("")
    }
  }

  private def eeittPrepop(eeitt: Eeitt, retrievals: MaterialisedRetrievals, formTemplate: FormTemplate)(
    implicit hc: HeaderCarrier) =
    eeittService.getValue(eeitt, retrievals, formTemplate).recover {
      case NonFatal(error) =>
        Logger.error(s"error when getting known facts from eeitt: " + error.getMessage)
        "" // let's return empty string
    }

}
