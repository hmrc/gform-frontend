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

import java.util.Base64

import cats.implicits._
import play.api.Logger
import play.api.libs.json.{ JsDefined, JsUndefined, Json }
import uk.gov.hmrc.auth.core.authorise._
import uk.gov.hmrc.auth.core.{ AffinityGroup, AuthConnector => _, _ }
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.gform
import uk.gov.hmrc.gform.gform.{ AuthContextPrepop, EeittService }
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Enrolment => _, _ }
import uk.gov.hmrc.gform.submission.SubmissionRef
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class AuthService(
  appConfig: AppConfig,
  eeittDelegate: EeittAuthorisationDelegate,
  eeittService: EeittService
)(
  implicit ec: ExecutionContext
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
      case Anonymous =>
        hc.sessionId
          .fold[AuthResult](AuthAnonymousSession(gform.routes.FormController.dashboard(formTemplate._id, lang)))(
            sessionId => AuthSuccessful(AnonymousRetrievals(sessionId)))
          .pure[Future]
      case AWSALBAuth            => performAWSALBAuth().pure[Future]
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

  private def performAWSALBAuth()(implicit hc: HeaderCarrier): AuthResult = {
    val encodedJWT: Seq[(String, String)] = hc.otherHeaders.filter(header => header._1 == "x-amzn-oidc-data")

    encodedJWT.headOption match {
      case None => AuthBlocked("You are not authorized to access this service")
      case Some((_, jwt)) => {
        val decoder = Base64.getDecoder
        val splitJwt = jwt.split("\\.")
        if (!(jwt.split("\\.").length == 3)) {
          Logger.error(s"Corrupt JWT received from AWS ALB: [$jwt]")
          AuthBlocked("You are not authorized to access this service")
        } else {
          val payload = new String(decoder.decode(splitJwt(1)))
          Json.parse(payload) \ "username" match {
            case JsDefined(value) => AuthSuccessful(AWSALBRetrievals(value.as[String]))
            case JsUndefined()    => AuthBlocked("Username does not exist in JWT")
          }
        }
      }
    }
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

        val showEnrolment = AuthRedirect(gform.routes.EnrolmentController.showEnrolment(formTemplate._id, lang).url)

        val recoverPF = needEnrolment match {
          case RequireEnrolment(enrolmentSection, _) => RecoverAuthResult.redirectToEnrolmentSection(showEnrolment)
          case RejectAccess                          => RecoverAuthResult.rejectInsufficientEnrolments
        }
        for {
          predicate <- predicateF
          result <- ggAuthorised(recoverPF)(predicate).map { authResult =>
                     (authResult, enrolmentPostCheck) match {
                       case (AuthSuccessful(retrievals: AuthenticatedRetrievals), RegimeIdCheck(regimeId)) =>
                         enrolmentCheckPredicate match {
                           case ForNonAgents if retrievals.affinityGroup.contains(AffinityGroup.Agent) => authResult
                           case ForNonAgents | Always =>
                             val serviceEnrolments =
                               retrievals.enrolments.enrolments.filter(_.key === enrolmentAuth.serviceId.value)
                             val enrolmentIdentifiers = serviceEnrolments.flatMap(_.identifiers.map(_.value))
                             val isRegimeIdEnrolled = enrolmentIdentifiers.exists(_.drop(2).startsWith(regimeId.value))

                             if (isRegimeIdEnrolled) authResult
                             else
                               needEnrolment match {
                                 case RequireEnrolment(_, _) => showEnrolment
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
        case ggSuccessfulAuth @ AuthSuccessful(AuthenticatedRetrievals(_, enrolments, affinityGroup, _, _, _, _, _))
            if affinityGroup.contains(AffinityGroup.Agent) =>
          ggAgentAuthorise(agentAccess, formTemplate, enrolments, lang) match {
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
        case ggSuccessfulAuth @ AuthSuccessful(AuthenticatedRetrievals(_, _, _, _, _, userDetails, _, _)) =>
          eeittDelegate.authenticate(regimeId, userDetails, requestUri).map {
            case EeittAuthorisationSuccessful            => ggSuccessfulAuth
            case EeittAuthorisationFailed(eeittLoginUrl) => AuthRedirectFlashingFormName(eeittLoginUrl)
          }
        case otherAuthResults => otherAuthResults.pure[Future]
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

  def eeitReferenceNumber(retrievals: MaterialisedRetrievals): String =
    retrievals match {
      case AnonymousRetrievals(_) => ""
      case AuthenticatedRetrievals(_, enrolments, _, _, _, userDetails, _, _) =>
        val identifier = userDetails.affinityGroup match {
          case AffinityGroup.Agent => EEITTAuthConfig.agentIdName
          case _                   => EEITTAuthConfig.nonAgentIdName
        }

        enrolments
          .getEnrolment(EEITTAuthConfig.eeittAuth)
          .flatMap(_.getIdentifier(identifier))
          .fold("")(_.value)

    }

  def evaluateSubmissionReference(
    expression: TextExpression,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    data: Map[FormComponentId, Seq[String]],
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Future[String] =
    expression.expr match {
      case AuthCtx(value) => AuthContextPrepop.values(value, retrievals).pure[Future]

      case EeittCtx(value) => eeittService.getValue(value, retrievals, formTemplate)

      case id: FormCtx => (data.get(id.toFieldId).map(_.head).getOrElse("")).pure[Future]

      case SubmissionReference => SubmissionRef(envelopeId).toString.pure[Future]

      case Constant(value) => Future.successful(value)

      case _ => "".pure[Future] //TODO change this to AuthExpr.
    }

}

sealed trait HMRCAgentAuthorisation

final object HMRCAgentAuthorisationSuccessful extends HMRCAgentAuthorisation

final object HMRCAgentAuthorisationDenied extends HMRCAgentAuthorisation

case class HMRCAgentAuthorisationFailed(subscribeUrl: String) extends HMRCAgentAuthorisation
