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

package uk.gov.hmrc.gform.auth

import java.util.Base64
import cats.implicits._
import org.apache.commons.codec.net.URLCodec
import org.slf4j.LoggerFactory
import play.api.libs.json.{ JsError, JsSuccess, Json }
import play.api.mvc.{ AnyContent, Cookie, Request }
import uk.gov.hmrc.auth.core.authorise._
import uk.gov.hmrc.auth.core.{ AuthConnector => _, _ }
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.controllers.GformRequestAttrKeys.{ compositeAuthSessionClearAttrKey, compositeAuthSessionClearAttrKeyName, emailSessionClearAttrKey, emailSessionClearAttrKeyName }
import uk.gov.hmrc.gform.controllers.GformSessionKeys.COMPOSITE_AUTH_DETAILS_SESSION_KEY
import uk.gov.hmrc.gform.gform
import uk.gov.hmrc.gform.gform.EmailAuthUtils.isEmailConfirmed
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.gform.URIUtils.addQueryParams
import uk.gov.hmrc.gform.models.EmailId
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.sharedmodel.AffinityGroup

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

class AuthService(
  appConfig: AppConfig
)(implicit
  ec: ExecutionContext
) {

  private val logger = LoggerFactory.getLogger(getClass)

  def authenticateAndAuthorise(
    formTemplateContext: FormTemplateContext,
    getAffinityGroup: Unit => Future[Option[AffinityGroup]],
    getGovermentGatewayId: Unit => Future[Option[GovernmentGatewayId]],
    ggAuthorised: PartialFunction[Throwable, AuthResult] => Predicate => Future[AuthResult],
    assumedIdentity: Option[Cookie]
  )(implicit
    request: Request[AnyContent],
    hc: HeaderCarrier,
    l: LangADT
  ): Future[AuthResult] = {
    val formTemplate = formTemplateContext.formTemplate
    formTemplate.authConfig match {
      case Anonymous =>
        hc.sessionId
          .fold[AuthResult](AuthAnonymousSession(gform.routes.NewFormController.dashboard(formTemplate._id)))(
            sessionId => AuthSuccessful(AnonymousRetrievals(sessionId), Role.Customer)
          )
          .pure[Future]
      case AWSALBAuth => performAWSALBAuth(assumedIdentity).pure[Future]
      case HmrcAny    => performHmrcAny(ggAuthorised(RecoverAuthResult.noop))
      case HmrcVerified(_, agentAccess, minimumCL, allowOrganisations) =>
        performGGAuth(ggAuthorised(RecoverAuthResult.noop)).map(authResult =>
          isHmrcVerified(authResult, formTemplate, agentAccess, minimumCL, allowOrganisations)
        )
      case HmrcSimpleModule => performGGAuth(ggAuthorised(RecoverAuthResult.noop))
      case HmrcEnrolmentModule(enrolmentAuth) =>
        performEnrolment(formTemplate, enrolmentAuth, getAffinityGroup, ggAuthorised)
      case HmrcAgentModule(agentAccess) =>
        performAgent(agentAccess, formTemplate, ggAuthorised(RecoverAuthResult.noop), Future.successful)
      case HmrcAgentWithEnrolmentModule(agentAccess, enrolmentAuth) =>
        def ifSuccessPerformEnrolment(authResult: AuthResult) = authResult match {
          case AuthSuccessful(_, _) => performEnrolment(formTemplate, enrolmentAuth, getAffinityGroup, ggAuthorised)
          case authUnsuccessful     => Future.successful(authUnsuccessful)
        }
        performAgent(agentAccess, formTemplate, ggAuthorised(RecoverAuthResult.noop), ifSuccessPerformEnrolment)
      case EmailAuthConfig(_, _, _, _) =>
        isEmailConfirmed(formTemplateContext) match {
          case Some(email) =>
            AuthSuccessful(EmailRetrievals(EmailId(email)), Role.Customer)
              .pure[Future]
          case None =>
            AuthEmailRedirect(
              gform.routes.EmailAuthController.emailIdForm(
                formTemplate._id,
                addQueryParams(
                  request.uri,
                  request.attrs
                    .get[String](emailSessionClearAttrKey)
                    .map((emailSessionClearAttrKeyName, _))
                    .toList: _*
                )
              )
            ).pure[Future]
        }
      case Composite(configs) =>
        val compositeAuthDetails =
          jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)
            .get(formTemplateContext)

        getGovermentGatewayId(()) flatMap {
          case Some(id) if List(AuthConfig.hmrcSimpleModule, id.ggId) contains compositeAuthDetails.getOrElse("") =>
            performGGAuth(ggAuthorised(RecoverAuthResult.noop))

          case Some(id) if compositeAuthDetails.isEmpty =>
            AuthCustomRedirect(
              gform.routes.CompositeAuthController
                .authSelectionForm(
                  formTemplate._id,
                  Some(id.ggId),
                  compositeAuthUrlParameters
                )
            )
              .pure[Future]

          case _ if compositeAuthDetails.isDefined =>
            AuthConfig
              .getAuthConfig(compositeAuthDetails.getOrElse(""), configs) match {
              case Some(config) =>
                authenticateAndAuthorise(
                  formTemplateContext.copy(formTemplate = formTemplateContext.formTemplate.copy(authConfig = config)),
                  getAffinityGroup,
                  getGovermentGatewayId,
                  ggAuthorised,
                  assumedIdentity
                )
              case None =>
                AuthCustomRedirect(
                  gform.routes.CompositeAuthController
                    .authSelectionForm(
                      formTemplate._id,
                      None,
                      compositeAuthUrlParameters
                    )
                )
                  .pure[Future]
            }

          case unknown =>
            val continueUrl = gform.routes.NewFormController.dashboard(formTemplate._id).url
            logger.info(s"Composite auth - no active session. GG: $unknown. Redirecting user to: $continueUrl")
            AuthCustomRedirect(
              gform.routes.CompositeAuthController.authSelectionForm(
                formTemplate._id,
                None,
                continueUrl
              )
            )
              .pure[Future]
        }
    }
  }

  private def compositeAuthUrlParameters(implicit request: Request[AnyContent]) =
    addQueryParams(
      request.uri,
      request.attrs
        .get[String](compositeAuthSessionClearAttrKey)
        .map((compositeAuthSessionClearAttrKeyName, _))
        .toList: _*
    )

  private val notAuthorized: AuthResult = AuthBlocked("You are not authorized to access this service")
  private val decoder = Base64.getDecoder

  private def performAWSALBAuth(assumedIdentity: Option[Cookie])(implicit hc: HeaderCarrier): AuthResult = {
    logger.info("ALB-AUTH: Start authorization...")

    val encodedJWT: Option[String] = hc.otherHeaders.collectFirst {
      case (header, value) if header === "X-Amzn-Oidc-Data" => value
    }

    logger.info(s"ALB-AUTH: JWT -> [${encodedJWT.getOrElse("No ALB JWT")}]")

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
                        logger.info(
                          s"ALB-AUTH: Authorizing with following credentials : [JWT: ${jwtPayload.toString}], [Case worker Cookie: ${cookie.value}]"
                        )
                        AuthSuccessful(awsAlbAuthenticatedRetrieval(AffinityGroup.Agent, cookie.value), Role.Reviewer)
                      } else {
                        logger.error(
                          s"ALB-AUTH: Attempted unauthorized access with following credentials : [JWT: ${jwtPayload.toString}], [Case worker Cookie: ${cookie.value}]"
                        )
                        notAuthorized
                      }
                    case None =>
                      logger.info(s"ALB-AUTH: Authorizing with following credentials : [JWT: ${jwtPayload.toString}]")
                      AuthSuccessful(
                        awsAlbAuthenticatedRetrieval(AffinityGroup.Individual, jwtPayload.username),
                        Role.Customer
                      )
                  }
                case JsError(_) => AuthBlocked("Not authorized")
              }
            case Failure(_) =>
              logger.error(s"ALB-AUTH : Corrupt JWT received from AWS ALB: payload is not a json: $payloadJson")
              notAuthorized
          }
        case _ =>
          logger.error(s"ALB-AUTH : Corrupt JWT received from AWS ALB: [$jwt]")
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
      None,
      OtherRetrievals.empty,
      ConfidenceLevel.L50,
      None
    )

  private def performEnrolment(
    formTemplate: FormTemplate,
    enrolmentAuth: EnrolmentAuth,
    getAffinityGroup: Unit => Future[Option[AffinityGroup]],
    ggAuthorised: PartialFunction[Throwable, AuthResult] => Predicate => Future[AuthResult]
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
          case RejectAccess                          => RecoverAuthResult.rejectInsufficientEnrolments(formTemplate._id)
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
                                  case RequireEnrolment(_, _) =>
                                    showEnrolment
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

  private def performHmrcAny(ggAuthorised: Predicate => Future[AuthResult]): Future[AuthResult] = {
    val predicate = EmptyPredicate
    ggAuthorised(predicate)
  }

  private def performGGAuth(ggAuthorised: Predicate => Future[AuthResult]): Future[AuthResult] = {
    val predicate = AuthProviders(AuthProvider.GovernmentGateway)
    ggAuthorised(predicate)
  }

  private def performAgent(
    agentAccess: AgentAccess,
    formTemplate: FormTemplate,
    ggAuthorised: Predicate => Future[AuthResult],
    continuation: AuthResult => Future[AuthResult]
  ): Future[AuthResult] =
    performGGAuth(ggAuthorised)
      .map {
        case ggSuccessfulAuth @ AuthSuccessful(ar @ AuthenticatedRetrievals(_, enrolments, _, _, _, _, _, _), _)
            if ar.affinityGroup == AffinityGroup.Agent =>
          ggAgentAuthorise(agentAccess, formTemplate, enrolments) match {
            case HMRCAgentAuthorisationSuccessful                => ggSuccessfulAuth
            case HMRCAgentAuthorisationDenied                    => AuthBlocked("Agents cannot access this form")
            case HMRCAgentAuthorisationFailed(agentSubscribeUrl) => AuthRedirect(agentSubscribeUrl)
          }

        case otherAuthResults => otherAuthResults
      }
      .flatMap(continuation)

  private def isHmrcVerified(
    authResult: AuthResult,
    formTemplate: FormTemplate,
    agentAccess: AgentAccess,
    minimumCL: String,
    allowOrganisations: Boolean
  ): AuthResult =
    authResult match {
      case AuthSuccessful(
            AuthenticatedRetrievals(_, _, AffinityGroup.Individual, _, maybeNino, _, confidenceLevel, _),
            _
          ) if maybeNino.isEmpty || confidenceLevel.level < Try(minimumCL.toInt).getOrElse(0) =>
        logger.info(
          s"Redirect to IV journey - nino: ${maybeNino.map(_ => "non-empty").getOrElse("empty")}, confidenceLevel: ${confidenceLevel.level}"
        )
        val codec = new URLCodec("UTF-8")
        val completionUrl = codec.encode(gform.routes.NewFormController.dashboard(formTemplate._id).url)
        val failureUrl = codec.encode(gform.routes.IdentityVerificationController.failure(formTemplate._id).url)
        AuthRedirect(
          s"/mdtp/uplift?origin=gForm&completionURL=$completionUrl&failureURL=$failureUrl&confidenceLevel=$minimumCL"
        )
      case AuthSuccessful(AuthenticatedRetrievals(_, enrolments, AffinityGroup.Organisation, _, _, _, _, _), _)
          if !allowOrganisations =>
        logger.info(s"Organisations cannot access this form - ${formTemplate._id.value}")
        AuthBlocked("Organisations cannot access this form")
      case AuthSuccessful(AuthenticatedRetrievals(_, enrolments, AffinityGroup.Agent, _, _, _, _, _), _) =>
        agentAccess match {
          case RequireMTDAgentEnrolment if enrolments.getEnrolment("HMRC-AS-AGENT").isDefined => authResult
          case RequireMTDAgentEnrolment                                                       => AuthRedirect(routes.AgentEnrolmentController.prologue(formTemplate._id).url)
          case AllowAnyAgentAffinityUser                                                      => authResult
          case _ =>
            logger.info(s"Agents cannot access this form - ${formTemplate._id.value}")
            AuthBlocked("Agents cannot access this form")
        }
      case _ => authResult
    }

  private def ggAgentAuthorise(
    agentAccess: AgentAccess,
    formTemplate: FormTemplate,
    enrolments: Enrolments
  ): HMRCAgentAuthorisation =
    agentAccess match {
      case RequireMTDAgentEnrolment if enrolments.getEnrolment("HMRC-AS-AGENT").isDefined =>
        HMRCAgentAuthorisationSuccessful
      case DenyAnyAgentAffinityUser  => HMRCAgentAuthorisationDenied
      case AllowAnyAgentAffinityUser => HMRCAgentAuthorisationSuccessful
      case _                         => HMRCAgentAuthorisationFailed(routes.AgentEnrolmentController.prologue(formTemplate._id).url)
    }
}

sealed trait HMRCAgentAuthorisation

final object HMRCAgentAuthorisationSuccessful extends HMRCAgentAuthorisation

final object HMRCAgentAuthorisationDenied extends HMRCAgentAuthorisation

case class HMRCAgentAuthorisationFailed(subscribeUrl: String) extends HMRCAgentAuthorisation
