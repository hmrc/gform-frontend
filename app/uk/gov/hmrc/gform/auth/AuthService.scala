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

package uk.gov.hmrc.gform.auth

import play.api.mvc._
import uk.gov.hmrc._
import uk.gov.hmrc.auth.core.authorise._
import uk.gov.hmrc.auth.core.{ AffinityGroup, AuthConnector => _, _ }
import gform.auth.models._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import cats.implicits._
import uk.gov.hmrc.gform.gform.{ AuthContextPrepop, EeittService }

import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier

class AuthService(
  appConfig: AppConfig,
  eeittDelegate: EeittAuthorisationDelegate,
  eeittService: EeittService
) {

  def authenticateAndAuthorise(
    formTemplate: FormTemplate,
    request: Request[AnyContent],
    requestUri: String,
    ggAuthorised: (Predicate, AuthConfig, FormTemplate, Request[AnyContent]) => Future[AuthResult])(
    implicit hc: HeaderCarrier): Future[AuthResult] =
    formTemplate.authConfig match {
      case authConfig: EEITTAuthConfig => performEEITTAuth(authConfig, formTemplate, request, requestUri, ggAuthorised)
      case authConfig                  => performHMRCAuth(authConfig, formTemplate, request, requestUri, ggAuthorised)
    }

  private def performEEITTAuth(
    authConfig: EEITTAuthConfig,
    formTemplate: FormTemplate,
    request: Request[AnyContent],
    requestUri: String,
    ggAuthorised: (Predicate, AuthConfig, FormTemplate, Request[AnyContent]) => Future[AuthResult]
  )(implicit hc: HeaderCarrier): Future[AuthResult] = {
    implicit val r = request
    ggAuthorised
      .apply(AuthProviders(AuthProvider.GovernmentGateway), authConfig, formTemplate, request)
      .flatMap {
        case ggSuccessfulAuth @ AuthSuccessful(retrievals) =>
          eeittDelegate.authenticate(authConfig.regimeId, retrievals.userDetails, requestUri).map {
            case EeittAuthorisationSuccessful            => ggSuccessfulAuth
            case EeittAuthorisationFailed(eeittLoginUrl) => AuthRedirectFlashingFormname(eeittLoginUrl)
          }
        case otherAuthResults => otherAuthResults.pure[Future]
      }
  }

  private def performHMRCAuth(
    authConfig: AuthConfig,
    formTemplate: FormTemplate,
    request: Request[AnyContent],
    requestUri: String,
    ggAuthorised: (Predicate, AuthConfig, FormTemplate, Request[AnyContent]) => Future[AuthResult])(
    implicit hc: HeaderCarrier): Future[AuthResult] = {
    val predicate = authConfig match {
      case config: AuthConfigWithEnrolment =>
        AuthProviders(AuthProvider.GovernmentGateway) and Enrolment(config.serviceId.value)
      case _ => AuthProviders(AuthProvider.GovernmentGateway)
    }

    val eventualGGAuthorised: Future[AuthResult] =
      ggAuthorised.apply(predicate, authConfig, formTemplate, request)

    implicit val r = request

    authConfig match {
      case config: AuthConfigWithAgentAccess if config.agentAccess.isDefined => {
        eventualGGAuthorised.map {
          case ggSuccessfulAuth @ AuthSuccessful(retrievals)
              if retrievals.affinityGroup.contains(AffinityGroup.Agent) =>
            ggAgentAuthorise(config.agentAccess.get, retrievals.enrolments, requestUri) match {
              case HMRCAgentAuthorisationSuccessful                => ggSuccessfulAuth
              case HMRCAgentAuthorisationDenied                    => AuthBlocked("Agents cannot access this form")
              case HMRCAgentAuthorisationFailed(agentSubscribeUrl) => AuthRedirect(agentSubscribeUrl)
            }

          case otherAuthResults => otherAuthResults
        }
      }
      case _ => eventualGGAuthorised
    }
  }

  private def agentSubscribeUrl(requestUri: String): String = {
    val continueUrl = java.net.URLEncoder.encode(requestUri, "UTF-8")
    val baseUrl = appConfig.`agent-subscription-frontend-base-url`
    s"$baseUrl/agent-subscription/check-business-type?continue=$continueUrl"
  }

  private def ggAgentAuthorise(agentAccess: AgentAccess, enrolments: Enrolments, requestUri: String)(
    implicit request: Request[AnyContent]): HMRCAgentAuthorisation =
    agentAccess match {
      case RequireMTDAgentEnrolment if enrolments.getEnrolment("HMRC-AS-AGENT").isDefined =>
        HMRCAgentAuthorisationSuccessful
      case DenyAnyAgentAffinityUser =>
        HMRCAgentAuthorisationDenied
      case AllowAnyAgentAffinityUser =>
        HMRCAgentAuthorisationSuccessful
      case _ =>
        HMRCAgentAuthorisationFailed(agentSubscribeUrl(requestUri))
    }

  def eeitReferenceNumber(retrievals: MaterialisedRetrievals): String = retrievals.userDetails.affinityGroup match {
    case AffinityGroup.Agent =>
      retrievals.enrolments
        .getEnrolment(AuthConfig.eeittAuth)
        .fold("")(_.getIdentifier(EEITTAuthConfig.agentIdName).fold("")(_.value))
    case _ =>
      retrievals.enrolments
        .getEnrolment(AuthConfig.eeittAuth)
        .fold("")(_.getIdentifier(EEITTAuthConfig.nonAgentIdName).fold("")(_.value))
  }

  def evaluateSubmissionReference(
    expression: TextExpression,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    data: Map[FormComponentId, Seq[String]])(implicit hc: HeaderCarrier): Future[String] =
    expression.expr match {
      case AuthCtx(value) =>
        val authContextPrepop = new AuthContextPrepop()
        authContextPrepop.values(value, retrievals).pure[Future]

      case EeittCtx(value) => eeittService.getValue(value, retrievals, formTemplate)

      case id: FormCtx => (data.get(id.toFieldId).map(_.head).getOrElse("")).pure[Future]

      case _ => "".pure[Future] //TODO change this to AuthExpr.
    }

}

sealed trait HMRCAgentAuthorisation

final object HMRCAgentAuthorisationSuccessful extends HMRCAgentAuthorisation

final object HMRCAgentAuthorisationDenied extends HMRCAgentAuthorisation

case class HMRCAgentAuthorisationFailed(subscribeUrl: String) extends HMRCAgentAuthorisation
