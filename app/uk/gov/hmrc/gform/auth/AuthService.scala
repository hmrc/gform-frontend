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

package uk.gov.hmrc.gform.auth

import java.net.URLEncoder
import java.util.Base64

import cats.implicits._
import play.api.Logger
import play.api.libs.json.{ JsError, JsSuccess, Json }
import play.api.mvc.Cookie
import uk.gov.hmrc.auth.core.EnrolmentIdentifier
import uk.gov.hmrc.auth.core.authorise._
import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
import uk.gov.hmrc.auth.core.{ AffinityGroup, AuthConnector => _, _ }
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.gform
import uk.gov.hmrc.gform.gform.EeittService
import uk.gov.hmrc.gform.models.mappings.IRSA
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Enrolment => _, _ }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

class AuthService(
  appConfig: AppConfig,
  eeittDelegate: EeittAuthorisationDelegate,
  eeittService: EeittService
)(
  implicit ec: ExecutionContext
) {

  def authenticateAndAuthorise(
    formTemplate: FormTemplate,
    requestUri: String,
    getAffinityGroup: Unit => Future[Option[AffinityGroup]],
    ggAuthorised: PartialFunction[Throwable, AuthResult] => Predicate => Future[AuthResult],
    assumedIdentity: Option[Cookie])(
    implicit hc: HeaderCarrier,
    l: LangADT
  ): Future[AuthResult] =
    formTemplate.authConfig match {
      case Anonymous =>
        hc.sessionId
          .fold[AuthResult](AuthAnonymousSession(gform.routes.NewFormController.dashboard(formTemplate._id)))(
            sessionId => AuthSuccessful(AnonymousRetrievals(sessionId), Role.Customer))
          .pure[Future]
      case AWSALBAuth            => performAWSALBAuth(assumedIdentity).pure[Future]
      case EeittModule(regimeId) => performEEITTAuth(regimeId, requestUri, ggAuthorised(RecoverAuthResult.noop))
      case HmrcAny               => performHmrcAny(ggAuthorised(RecoverAuthResult.noop))
      case HmrcVerified(_, _) =>
        performGGAuth(ggAuthorised(RecoverAuthResult.noop)).map(authResult => isHmrcVerified(authResult, formTemplate))
      case HmrcSimpleModule => performGGAuth(ggAuthorised(RecoverAuthResult.noop))
      case HmrcEnrolmentModule(enrolmentAuth) =>
        performEnrolment(formTemplate, enrolmentAuth, getAffinityGroup, ggAuthorised)
      case HmrcAgentModule(agentAccess) =>
        performAgent(agentAccess, formTemplate, ggAuthorised(RecoverAuthResult.noop), Future.successful(_))
      case HmrcAgentWithEnrolmentModule(agentAccess, enrolmentAuth) =>
        def ifSuccessPerformEnrolment(authResult: AuthResult) = authResult match {
          case AuthSuccessful(_, _) => performEnrolment(formTemplate, enrolmentAuth, getAffinityGroup, ggAuthorised)
          case authUnsuccessful     => Future.successful(authUnsuccessful)
        }
        performAgent(agentAccess, formTemplate, ggAuthorised(RecoverAuthResult.noop), ifSuccessPerformEnrolment)
    }

  private val notAuthorized: AuthResult = AuthBlocked("You are not authorized to access this service")
  private val decoder = Base64.getDecoder

  private def performAWSALBAuth(assumedIdentity: Option[Cookie])(implicit hc: HeaderCarrier): AuthResult = {
    Logger.info("ALB-AUTH: Start authorization...")

    val encodedJWT: Option[String] = hc.otherHeaders.collectFirst {
      case (header, value) if header === "X-Amzn-Oidc-Data" => value
    }

    Logger.info(s"ALB-AUTH: JWT -> [${encodedJWT.getOrElse("No ALB JWT")}]")

    encodedJWT.fold(notAuthorized) { jwt =>
      jwt.split("\\.") match {
        case Array(header, payload, signature) =>
          val payloadJson = new String(decoder.decode(payload))
          Try(Json.parse(payloadJson)) match {
            case Success(json) =>
              Json.fromJson[JwtPayload](json) match {
                case JsSuccess(jwtPayload, _) =>
                  assumedIdentity match {
                    case Some(cookie) =>
                      if (jwtPayload.iss === appConfig.albAdminIssuerUrl) {
                        Logger.info(
                          s"ALB-AUTH: Authorizing with following credentials : [JWT: ${jwtPayload.toString}], [Case worker Cookie: ${cookie.value}]")
                        AuthSuccessful(awsAlbAuthenticatedRetrieval(AffinityGroup.Agent, cookie.value), Role.Reviewer)
                      } else {
                        Logger.error(
                          s"ALB-AUTH: Attempted unauthorized access with following credentials : [JWT: ${jwtPayload.toString}], [Case worker Cookie: ${cookie.value}]")
                        notAuthorized
                      }
                    case None =>
                      Logger.info(s"ALB-AUTH: Authorizing with following credentials : [JWT: ${jwtPayload.toString}]")
                      AuthSuccessful(
                        awsAlbAuthenticatedRetrieval(AffinityGroup.Individual, jwtPayload.username),
                        Role.Customer)
                  }
                case JsError(_) => AuthBlocked("Not authorized")
              }
            case Failure(_) =>
              Logger.error(s"ALB-AUTH : Corrupt JWT received from AWS ALB: payload is not a json: $payloadJson")
              notAuthorized
          }
        case _ =>
          Logger.error(s"ALB-AUTH : Corrupt JWT received from AWS ALB: [$jwt]")
          notAuthorized
      }
    }
  }

  private def awsAlbAuthenticatedRetrieval(affinityGroup: AffinityGroup, identity: String): AuthenticatedRetrievals =
    AuthenticatedRetrievals(
      GovernmentGatewayId(""),
      Enrolments(Set.empty),
      affinityGroup,
      identity,
      None
    )

  private def performEnrolment(
    formTemplate: FormTemplate,
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

        val showEnrolment = AuthRedirect(gform.routes.EnrolmentController.showEnrolment(formTemplate._id).url)

        val recoverPF = needEnrolment match {
          case RequireEnrolment(enrolmentSection, _) => RecoverAuthResult.redirectToEnrolmentSection(showEnrolment)
          case RejectAccess                          => RecoverAuthResult.rejectInsufficientEnrolments
        }
        for {
          predicate <- predicateF
          result <- ggAuthorised(recoverPF)(predicate).map { authResult =>
                     (authResult, enrolmentPostCheck) match {
                       case (AuthSuccessful(retrievals: AuthenticatedRetrievals, _), RegimeIdCheck(regimeId)) =>
                         enrolmentCheckPredicate match {
                           case ForNonAgents if retrievals.affinityGroup == AffinityGroup.Agent => authResult
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

  private def performHmrcAny(ggAuthorised: Predicate => Future[AuthResult])(
    implicit hc: HeaderCarrier): Future[AuthResult] = {
    val predicate = EmptyPredicate
    ggAuthorised(predicate)
  }

  private def performGGAuth(ggAuthorised: Predicate => Future[AuthResult])(
    implicit hc: HeaderCarrier): Future[AuthResult] = {
    val predicate = AuthProviders(AuthProvider.GovernmentGateway)
    ggAuthorised(predicate)
  }

  private def performAgent(
    agentAccess: AgentAccess,
    formTemplate: FormTemplate,
    ggAuthorised: Predicate => Future[AuthResult],
    continuation: AuthResult => Future[AuthResult])(implicit hc: HeaderCarrier, l: LangADT): Future[AuthResult] =
    performGGAuth(ggAuthorised)
      .map {
        case ggSuccessfulAuth @ AuthSuccessful(ar @ AuthenticatedRetrievals(_, enrolments, _, _, _), _)
            if ar.affinityGroup == AffinityGroup.Agent =>
          ggAgentAuthorise(agentAccess, formTemplate, enrolments) match {
            case HMRCAgentAuthorisationSuccessful                => ggSuccessfulAuth
            case HMRCAgentAuthorisationDenied                    => AuthBlocked("Agents cannot access this form")
            case HMRCAgentAuthorisationFailed(agentSubscribeUrl) => AuthRedirect(agentSubscribeUrl)
          }

        case otherAuthResults => otherAuthResults
      }
      .flatMap(continuation)

  private def isHmrcVerified(authResult: AuthResult, formTemplate: FormTemplate): AuthResult =
    authResult match {
      case AuthSuccessful(AuthenticatedRetrievals(_, _, AffinityGroup.Individual, _, None), _) =>
        val completionUrl = URLEncoder.encode(gform.routes.NewFormController.dashboard(formTemplate._id).url, "UTF-8")
        val failureUrl =
          URLEncoder.encode(gform.routes.IdentityVerificationController.failure(formTemplate._id).url, "UTF-8")
        AuthRedirect(
          s"/mdtp/uplift?origin=gForm&completionURL=$completionUrl&failureURL=$failureUrl&confidenceLevel=200")
      case AuthSuccessful(AuthenticatedRetrievals(_, enrolments, AffinityGroup.Organisation, _, None), _) =>
        val irsa = IRSA()
        val maybeEnrolmentId: Option[EnrolmentIdentifier] =
          enrolments.getEnrolment(irsa.name).flatMap(_.getIdentifier(irsa.id))

        maybeEnrolmentId.fold[AuthResult](
          AuthRedirect(
            gform.routes.IdentityVerificationController.enrolmentsNeeded(formTemplate._id).url
          ))(_ => authResult)

      case AuthSuccessful(AuthenticatedRetrievals(_, _, AffinityGroup.Agent, _, _), _) =>
        AuthBlocked("Agents cannot access this form")
      case _ => authResult

    }

  private def performEEITTAuth(
    regimeId: RegimeId,
    requestUri: String,
    ggAuthorised: Predicate => Future[AuthResult]
  )(implicit hc: HeaderCarrier): Future[AuthResult] =
    performGGAuth(ggAuthorised)
      .flatMap {
        case ggSuccessfulAuth @ AuthSuccessful(AuthenticatedRetrievals(_, _, affinityGroup, groupIdentifier, _), _) =>
          eeittDelegate.authenticate(regimeId, affinityGroup, groupIdentifier, requestUri).map {
            case EeittAuthorisationSuccessful            => ggSuccessfulAuth
            case EeittAuthorisationFailed(eeittLoginUrl) => AuthRedirectFlashingFormName(eeittLoginUrl)
          }
        case otherAuthResults => otherAuthResults.pure[Future]
      }

  private def ggAgentAuthorise(agentAccess: AgentAccess, formTemplate: FormTemplate, enrolments: Enrolments)(
    implicit l: LangADT): HMRCAgentAuthorisation =
    agentAccess match {
      case RequireMTDAgentEnrolment if enrolments.getEnrolment("HMRC-AS-AGENT").isDefined =>
        HMRCAgentAuthorisationSuccessful
      case DenyAnyAgentAffinityUser  => HMRCAgentAuthorisationDenied
      case AllowAnyAgentAffinityUser => HMRCAgentAuthorisationSuccessful
      case _ =>
        HMRCAgentAuthorisationFailed(
          routes.AgentEnrolmentController.prologue(formTemplate._id, formTemplate.formName.value).url)
    }

  def eeitReferenceNumber(retrievals: MaterialisedRetrievals): String =
    retrievals match {
      case AuthenticatedRetrievals(_, enrolments, affinityGroup, _, _) =>
        val identifier = affinityGroup match {
          case AffinityGroup.Agent => EEITTAuthConfig.agentIdName
          case _                   => EEITTAuthConfig.nonAgentIdName
        }

        enrolments
          .getEnrolment(EEITTAuthConfig.eeittAuth)
          .flatMap(_.getIdentifier(identifier))
          .fold("")(_.value)

      case _ => ""
    }

}

sealed trait HMRCAgentAuthorisation

final object HMRCAgentAuthorisationSuccessful extends HMRCAgentAuthorisation

final object HMRCAgentAuthorisationDenied extends HMRCAgentAuthorisation

case class HMRCAgentAuthorisationFailed(subscribeUrl: String) extends HMRCAgentAuthorisation
