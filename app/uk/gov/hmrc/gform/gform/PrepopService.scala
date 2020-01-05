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

package uk.gov.hmrc.gform.gform

import play.api.Logger

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.models.mappings.{ HMRCOBTDSORG, IRCT, IRSA, NINO }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier

import scala.util.control.NonFatal

object AuthContextPrepop {
  def values(value: AuthInfo, retrievals: MaterialisedRetrievals): String = value match {
    case GG                     => retrievals.ggCredId
    case PayeNino               => retrievals.getTaxIdValue(NINO())
    case SaUtr                  => retrievals.getTaxIdValue(IRSA())
    case CtUtr                  => retrievals.getTaxIdValue(IRCT())
    case EtmpRegistrationNumber => retrievals.getTaxIdValue(HMRCOBTDSORG())
  }
}

class PrepopService(eeittService: EeittService)(
  implicit ec: ExecutionContext
) {

  def eeittPrepop(
    eeitt: Eeitt,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    hc: HeaderCarrier): Future[String] = {
    implicit val hc_ = hc
    eeittService.getValue(eeitt, retrievals, formTemplate).recover {
      case NonFatal(error) =>
        Logger.error(s"error when getting known facts from eeitt: " + error.getMessage)
        "" // let's return empty string
    }
  }

}
