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

package uk.gov.hmrc.gform.service

import play.api.Logger

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.models.userdetails.UserDetails
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.gform.FrontendAuthConnector
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.gform.models.form.FormTypeId
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.util.control.NonFatal

object AuthContextPrepop {
  def values(value: AuthInfo, authContext: AuthContext): String = (value match {
    case GG => authContext.user.governmentGatewayToken
    case PayeNino => authContext.principal.accounts.paye.map(_.nino.nino)
    case SaUtr => authContext.principal.accounts.sa.map(_.utr.utr)
    case CtUtr => authContext.principal.accounts.ct.map(_.utr.utr)
  }).getOrElse("")
}

object PrepopService {

  def eeittConnector = EeittConnector // TODO - DI this
  def authConnector = new FrontendAuthConnector // TODO - DI this

  def prepopData(expr: Expr, formTypeId: FormTypeId)(implicit authContext: AuthContext, hc: HeaderCarrier): Future[String] = {
    expr match {
      case AuthCtx(value) => Future.successful(AuthContextPrepop.values(value, authContext))
      case Constant(value) => Future.successful(value)
      case EeittCtx(eeitt) =>

        val prepop =
          for {
            userDetails <- authConnector.getUserDetails[UserDetails](authContext)
            prepopData <- eeitt match {
              case BusinessUser => eeittConnector.prepopulationBusinessUser(userDetails.groupIdentifier, formTypeId).map(_.registrationNumber)
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
