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

package uk.gov.hmrc.gform.auth

import cats.implicits._
import play.api.mvc._
import uk.gov.hmrc.auth.core.authorise._
import uk.gov.hmrc.auth.core.{ AffinityGroup, AuthConnector => _, _ }
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.gform.{ AuthContextPrepop, EeittService }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Enrolment => _, _ }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.gform.sharedmodel.SubmissionReferenceUtil.getSubmissionReference

import scala.concurrent.Future

class AuthService(
  appConfig: AppConfig,
  eeittDelegate: EeittAuthorisationDelegate,
  eeittService: EeittService
) {

  def authenticateAndAuthorise(
    formTemplate: FormTemplate,
    lang: Option[String],
    requestUri: String,
    getAffinityGroup: Unit => Future[Option[AffinityGroup]],
    ggAuthorised: PartialFunction[Throwable, AuthResult] => Predicate => Future[AuthResult]
  )(
    implicit hc: HeaderCarrier
  ): Future[AuthResult] =
    formTemplate.authConfig match {
      case EeittModule(regimeId) => performEEITTAuth(regimeId, requestUri, ggAuthorised(RecoverAuthResult.noop))
      case HmrcSimpleModule      => performGGAuth(ggAuthorised(RecoverAuthResult.noop))
      case HmrcEnrolmentModule(enrolmentAuth) =>
        performEnrolment(formTemplate, lang, enrolmentAuth, getAffinityGroup, ggAuthorised)
      case HmrcAgentModule(agentAccess) =>
        performAgent(agentAccess, formTemplate, lang, ggAuthorised(RecoverAuthResult.noop), Future.successful(_))
      case HmrcAgentWithEnrolmentModule(agentAccess, enrolmentAuth) =>
        def ifSuccessPerformEnrolment(authResult: AuthResult) = authResult match {
          case AuthSuccessful(_) => performEnrolment(formTemplate, lang, enrolmentAuth, getAffinityGroup, ggAuthorised)
          case authUnsuccessful  => Future.successful(authUnsuccessful)
        }
        performAgent(agentAccess, formTemplate, lang, ggAuthorised(RecoverAuthResult.noop), ifSuccessPerformEnrolment)
    }

  private def performEnrolment(
    formTemplate: FormTemplate,
    lang: Option[String],
    enrolmentAuth: EnrolmentAuth,
    getAffinityGroup: Unit => Future[Option[AffinityGroup]],
    ggAuthorised: PartialFunction[Throwable, AuthResult] => Predicate => Future[AuthResult]
  )(
    implicit hc: HeaderCarrier
  ): Future[AuthResult] =
    enrolmentAuth.enrolmentCheck match {
      case DoCheck(enrolmentCheckPredicate, needEnrolment, enrolmentPostCheck) =>
        val predicateF: Future[Predicate] = {

          val ggOnlyPredicate = AuthProviders(AuthProvider.GovernmentGateway)
          val ggAndEnrolmentPredicate = ggOnlyPredicate and Enrolment(enrolmentAuth.serviceId.value)

          enrolmentCheckPredicate match {
            case Always => Future.successful(ggAndEnrolmentPredicate)
            case ForNonAgents =>
              getAffinityGroup(()).map {
                case Some(AffinityGroup.Organisation) | Some(AffinityGroup.Individual) => ggAndEnrolmentPredicate
                case _                                                                 => ggOnlyPredicate
              }
          }
        }

        val showEnrolment = AuthRedirect(
          uk.gov.hmrc.gform.gform.routes.EnrolmentController.showEnrolment(formTemplate._id, lang).url)

        val recoverPF = needEnrolment match {
          case RequireEnrolment(enrolmentSection) => RecoverAuthResult.redirectToEnrolmentSection(showEnrolment)
          case RejectAccess                       => RecoverAuthResult.rejectInsufficientEnrolments
        }
        for {
          predicate <- predicateF
          result <- ggAuthorised(recoverPF)(predicate).map { authResult =>
                     (authResult, enrolmentPostCheck) match {
                       case (AuthSuccessful(retrievals), RegimeIdCheck(regimeId)) =>
                         enrolmentCheckPredicate match {
                           case ForNonAgents if retrievals.affinityGroup.contains(AffinityGroup.Agent) => authResult
                           case ForNonAgents | Always =>
                             val serviceEnrolments =
                               retrievals.enrolments.enrolments.filter(_.key === enrolmentAuth.serviceId.value)
                             val enrolmentIdentifiers = serviceEnrolments.flatMap(_.identifiers.map(_.value))
                             val isRegimeIdEnrolled = enrolmentIdentifiers.forall(_.drop(2).startsWith(regimeId.value))

                             if (isRegimeIdEnrolled) authResult
                             else
                               needEnrolment match {
                                 case RequireEnrolment(_) => showEnrolment
                                 case RejectAccess =>
                                   AuthBlocked(s"Enrolment for regimeId: ${regimeId.value} required.")
                               }
                         }
                       case _ => authResult
                     }
                   }
        } yield result
      case Never =>
        // From the spec 'never' seems to be the same as not providing ServiceId in the first place.
        performGGAuth(ggAuthorised(RecoverAuthResult.noop))
    }

  private def performGGAuth(ggAuthorised: Predicate => Future[AuthResult])(
    implicit hc: HeaderCarrier): Future[AuthResult] = {
    val predicate = AuthProviders(AuthProvider.GovernmentGateway)
    ggAuthorised(predicate)
  }

  private def performAgent(
    agentAccess: AgentAccess,
    formTemplate: FormTemplate,
    lang: Option[String],
    ggAuthorised: Predicate => Future[AuthResult],
    continuation: AuthResult => Future[AuthResult])(implicit hc: HeaderCarrier): Future[AuthResult] =
    performGGAuth(ggAuthorised)
      .map {
        case ggSuccessfulAuth @ AuthSuccessful(retrievals) if retrievals.affinityGroup.contains(AffinityGroup.Agent) =>
          ggAgentAuthorise(agentAccess, formTemplate, retrievals.enrolments, lang) match {
            case HMRCAgentAuthorisationSuccessful                => ggSuccessfulAuth
            case HMRCAgentAuthorisationDenied                    => AuthBlocked("Agents cannot access this form")
            case HMRCAgentAuthorisationFailed(agentSubscribeUrl) => AuthRedirect(agentSubscribeUrl)
          }

        case otherAuthResults => otherAuthResults
      }
      .flatMap(continuation)

  private def performEEITTAuth(
    regimeId: RegimeId,
    requestUri: String,
    ggAuthorised: Predicate => Future[AuthResult]
  )(implicit hc: HeaderCarrier): Future[AuthResult] =
    performGGAuth(ggAuthorised)
      .flatMap {
        case ggSuccessfulAuth @ AuthSuccessful(retrievals) =>
          eeittDelegate.authenticate(regimeId, retrievals.userDetails, requestUri).map {
            case EeittAuthorisationSuccessful            => ggSuccessfulAuth
            case EeittAuthorisationFailed(eeittLoginUrl) => AuthRedirectFlashingFormName(eeittLoginUrl)
          }
        case otherAuthResults => otherAuthResults.pure[Future]
      }

  private def agentSubscribeUrl(requestUri: String): String = {
    val continueUrl = java.net.URLEncoder.encode(requestUri, "UTF-8")
    val baseUrl = appConfig.`agent-subscription-frontend-base-url`
    s"$baseUrl/agent-subscription/check-business-type?continue=$continueUrl"
  }

  private def ggAgentAuthorise(
    agentAccess: AgentAccess,
    formTemplate: FormTemplate,
    enrolments: Enrolments,
    lang: Option[String]): HMRCAgentAuthorisation =
    agentAccess match {
      case RequireMTDAgentEnrolment if enrolments.getEnrolment("HMRC-AS-AGENT").isDefined =>
        HMRCAgentAuthorisationSuccessful
      case DenyAnyAgentAffinityUser  => HMRCAgentAuthorisationDenied
      case AllowAnyAgentAffinityUser => HMRCAgentAuthorisationSuccessful
      case _ =>
        HMRCAgentAuthorisationFailed(
          routes.AgentEnrolmentController.prologue(formTemplate._id, formTemplate.formName, lang).url)
    }

  def eeitReferenceNumber(retrievals: MaterialisedRetrievals): String = {

    val identifier = retrievals.userDetails.affinityGroup match {
      case AffinityGroup.Agent => EEITTAuthConfig.agentIdName
      case _                   => EEITTAuthConfig.nonAgentIdName
    }

    retrievals.enrolments
      .getEnrolment(EEITTAuthConfig.eeittAuth)
      .flatMap(_.getIdentifier(identifier))
      .fold("")(_.value)
  }

  def evaluateSubmissionReference(
    expression: TextExpression,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    data: Map[FormComponentId, Seq[String]])(implicit hc: HeaderCarrier): Future[String] =
    expression.expr match {
      case AuthCtx(value) => AuthContextPrepop.values(value, retrievals).pure[Future]

      case EeittCtx(value) => eeittService.getValue(value, retrievals, formTemplate)

      case id: FormCtx => (data.get(id.toFieldId).map(_.head).getOrElse("")).pure[Future]

      //case SubmissionReference => getSubmissionReference()

      case _ => "".pure[Future] //TODO change this to AuthExpr.
    }

}

sealed trait HMRCAgentAuthorisation

final object HMRCAgentAuthorisationSuccessful extends HMRCAgentAuthorisation

final object HMRCAgentAuthorisationDenied extends HMRCAgentAuthorisation

case class HMRCAgentAuthorisationFailed(subscribeUrl: String) extends HMRCAgentAuthorisation
