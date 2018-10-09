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
import scala.util.control.NonFatal
import uk.gov.hmrc.auth.core.retrieve.GGCredId
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.http.HeaderCarrier

object AuthContextPrepop {
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
  eeittService: EeittService
) {

  def eeittPrepop(eeitt: Eeitt, retrievals: MaterialisedRetrievals, formTemplate: FormTemplate, hc: HeaderCarrier) = {
    implicit val hc_ = hc
    eeittService.getValue(eeitt, retrievals, formTemplate).recover {
      case NonFatal(error) =>
        Logger.error(s"error when getting known facts from eeitt: " + error.getMessage)
        "" // let's return empty string
    }
  }

}
