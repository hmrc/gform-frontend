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

package uk.gov.hmrc.gform.prepop

import play.api.Logger
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.models.userdetails.UserDetails
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, _ }
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

class AuthContextPrepop {
  def values(value: AuthInfo, authContext: AuthContext): String = (value match {
    case GG => authContext.user.governmentGatewayToken
    case PayeNino => authContext.principal.accounts.paye.map(_.nino.nino)
    case SaUtr => authContext.principal.accounts.sa.map(_.utr.utr)
    case CtUtr => authContext.principal.accounts.ct.map(_.utr.utr)
  }).getOrElse("")
}

class PrepopService(
    eeittConnector: EeittConnector,
    authConnector: AuthConnector,
    authContextPrepop: AuthContextPrepop
) {

  def prepopData(expr: Expr, formTemplateId: FormTemplateId)(implicit authContext: AuthContext, hc: HeaderCarrier): Future[String] = {
    expr match {
      case AuthCtx(value) => Future.successful(authContextPrepop.values(value, authContext))
      case Constant(value) => Future.successful(value)
      case EeittCtx(eeitt) =>

        val prepop =
          for {
            userDetails <- authConnector.getUserDetails[UserDetails](authContext)
            prepopData <- eeitt match {
              case BusinessUser => eeittConnector.prepopulationBusinessUser(userDetails.groupIdentifier, formTemplateId).map(_.registrationNumber)
              case Agent => eeittConnector.prepopulationAgent(userDetails.groupIdentifier).map(_.arn)
            }
          } yield prepopData

        prepop.recover {
          case NonFatal(error) =>
            Logger.error(s"error when getting known facts from eeitt: " + error.getMessage)
            "" // let's return empty string
        }
      case _ => Future.successful("")
    }
  }
}
