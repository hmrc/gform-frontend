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

package uk.gov.hmrc.gform
package controllers

import play.api.Logger
import play.api.http.HeaderNames
import play.api.i18n.I18nSupport
import play.api.mvc.Results._
import play.api.mvc._
import uk.gov.hmrc._
import uk.gov.hmrc.auth.core.authorise._
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector => _, _}
import uk.gov.hmrc.gform.auth._
import gform.auth.models._
import uk.gov.hmrc.gform.config.{AppConfig, FrontendAppConfig}
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.form.{Form, FormId}
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.auth.core.retrieve.{GGCredId, LegacyCredentials, OneTimeLogin, PAClientId, Retrievals, VerifyPid}
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

class AuthenticatedRequestActions(
  gformConnector: GformConnector,
  appConfig: AppConfig,
  frontendAppConfig: FrontendAppConfig,
  val authConnector: AuthConnector,
  eeittDelegate: EeittAuthorisationDelegate,
  i18nSupport: I18nSupport,
  errResponder: ErrResponder
) extends AuthorisedFunctions {

  import i18nSupport._

  // format: OFF
  val defaultRetrievals = Retrievals.authProviderId     and Retrievals.allEnrolments  and
                          Retrievals.affinityGroup      and Retrievals.internalId     and
                          Retrievals.externalId         and Retrievals.userDetailsUri and
                          Retrievals.credentialStrength and Retrievals.agentCode
  // format: ON

  implicit def hc(implicit request: Request[_]): HeaderCarrier =
    HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

  private def authFormUser(form: Form)(retrievals: MaterialisedRetrievals)(implicit ec: ExecutionContext) : Future[AuthResult] =
    (if(form.userId.value == retrievals.userDetails.groupIdentifier)
      AuthSuccessful(retrievals)
    else
      AuthForbidden("You cannot access this page")).pure[Future]

  private def authUserWhitelist(retrievals: MaterialisedRetrievals)(implicit
  hc: HeaderCarrier) : Future[AuthResult] = {
    if (frontendAppConfig.whitelistEnabled) {
      for {
        isValid <- gformConnector.whiteList(retrievals.userDetails.email)
      } yield
        isValid match {
          case Some(idx) =>
            Logger.info(s"Passed successful through white listing: $idx user index")
            AuthSuccessful(retrievals)
          case None =>
            Logger.warn(s"User failed whitelisting and is denied access : ${idForLog(retrievals.authProviderId)}")
            AuthBlocked("Non-whitelisted User")
        }
    } else Future.successful(AuthSuccessful(retrievals))
  }

  def async(formTemplateId: FormTemplateId)(
    f: Request[AnyContent] => AuthCacheWithoutForm => Future[Result]): Action[AnyContent] = Action.async {
    implicit request =>
      for {
        formTemplate <- gformConnector.getFormTemplate(formTemplateId)
        authResult   <- authenticateAndAuthorise(formTemplate, authUserWhitelist(_))
        newRequest = removeEeittAuthIdFromSession(request, formTemplate.authConfig)
        result <- handleAuthResults(
                   authResult,
                   formTemplate,
                   request,
                   onSuccess = retrievals => f(newRequest)(AuthCacheWithoutForm(retrievals, formTemplate))
                 )
      } yield result
  }

  def async(formId: FormId)(
    f: Request[AnyContent] => AuthCacheWithForm => Future[Result]): Action[AnyContent] = Action.async {
    implicit request =>
      for {
        form         <- gformConnector.getForm(formId)
        formTemplate <- gformConnector.getFormTemplate(form.formTemplateId)
        authResult   <- authenticateAndAuthorise(formTemplate, authFormUser(form))
        newRequest = removeEeittAuthIdFromSession(request, formTemplate.authConfig)
        result <- handleAuthResults(
          authResult,
          formTemplate,
          request,
          onSuccess = retrievals => f(newRequest)(AuthCacheWithForm(retrievals, form, formTemplate))
        )
      } yield result
    }

  private def handleAuthResults(
    result: AuthResult,
    formTemplate: FormTemplate,
    request: Request[_],
    onSuccess: MaterialisedRetrievals => Future[Result]
  )(
    implicit
    hc: HeaderCarrier): Future[Result] =
    result match {
      case AuthSuccessful(retrievals)        => onSuccess(retrievals)
      case AuthRedirect(loginUrl, flashing) => Redirect(loginUrl).flashing(flashing: _*).pure[Future]
      case AuthRedirectFlashingFormname(loginUrl) =>
        Redirect(loginUrl).flashing("formTitle" -> formTemplate.formName).pure[Future]
      case AuthBlocked(message) =>
        Ok(
          views.html.error_template(
            pageTitle = "Access denied",
            heading = "Access denied",
            message = message,
            frontendAppConfig = frontendAppConfig)).pure[Future]
      case AuthForbidden(message) =>
        errResponder.forbidden(request, message)
    }

  private def authenticateAndAuthorise(formTemplate: FormTemplate,
                                       authGivenRetrievals: MaterialisedRetrievals => Future[AuthResult])(
    implicit request: Request[AnyContent],
    hc: HeaderCarrier): Future[AuthResult] =
    formTemplate.authConfig match {
      case authConfig: EEITTAuthConfig => performEEITTAuth(authConfig, formTemplate, authGivenRetrievals)
      case authConfig                  => performHMRCAuth(authConfig, formTemplate, authGivenRetrievals)
    }

  private def performEEITTAuth(
    authConfig: EEITTAuthConfig,
    formTemplate: FormTemplate,
    authGivenRetrievals: MaterialisedRetrievals => Future[AuthResult]
  )(
    implicit
    request: Request[AnyContent],
    hc: HeaderCarrier
  ): Future[AuthResult] =
    ggAuthorised(AuthProviders(AuthProvider.GovernmentGateway), authConfig, formTemplate, authGivenRetrievals).flatMap {
      case ggSuccessfulAuth @ AuthSuccessful(retrievals) =>
        eeittDelegate.authenticate(authConfig.regimeId, retrievals.userDetails).map {
          case EeittAuthorisationSuccessful            => updateEnrolments(ggSuccessfulAuth, request)
          case EeittAuthorisationFailed(eeittLoginUrl) => AuthRedirectFlashingFormname(eeittLoginUrl)
        }
      case otherAuthResults => Future.successful(otherAuthResults)
    }

  private def updateEnrolments(authSuccessful: AuthSuccessful, request: Request[_]): AuthSuccessful = {
    // the registrationNumber will be stored in the session by eeittAuth
    def updateFor(authBy: String): Option[AuthSuccessful] =
      request.session.get(authBy).map { regNum =>
        val newEnrolment = Enrolment(AuthConfig.eeittAuth).withIdentifier(authBy, regNum)
        val newEnrolments = Enrolments(authSuccessful.retrievals.enrolments.enrolments + newEnrolment)
        authSuccessful.copy(retrievals = authSuccessful.retrievals.copy(enrolments = newEnrolments))
      }

    updateFor(EEITTAuthConfig.nonAgentIdName)
      .orElse(updateFor(EEITTAuthConfig.agentIdName))
      .getOrElse(authSuccessful)
  }

  private def performHMRCAuth(authConfig: AuthConfig, formTemplate: FormTemplate,
                              authGivenRetrievals: MaterialisedRetrievals => Future[AuthResult])(
    implicit request: Request[AnyContent],
    hc: HeaderCarrier): Future[AuthResult] = {
    val predicate = authConfig match {
      case config: AuthConfigWithEnrolment =>
        AuthProviders(AuthProvider.GovernmentGateway) and Enrolment(config.serviceId.value)
      case _ => AuthProviders(AuthProvider.GovernmentGateway)
    }

    val eventualGGAuthorised: Future[AuthResult] = ggAuthorised(predicate, authConfig, formTemplate, authGivenRetrievals)

    authConfig match {
      case config: AuthConfigWithAgentAccess if config.agentAccess.isDefined => {
        eventualGGAuthorised.map {
          case ggSuccessfulAuth @ AuthSuccessful(retrievals)
              if retrievals.affinityGroup.contains(AffinityGroup.Agent) =>
            ggAgentAuthorise(config.agentAccess.get, retrievals.enrolments) match {
              case HMRCAgentAuthorisationSuccessful                => updateEnrolments(ggSuccessfulAuth, request)
              case HMRCAgentAuthorisationDenied                    => AuthBlocked("Agents cannot access this form")
              case HMRCAgentAuthorisationFailed(agentSubscribeUrl) => AuthRedirect(agentSubscribeUrl)
            }

          case otherAuthResults => otherAuthResults
        }
      }
      case _ => eventualGGAuthorised
    }
  }

  private def agentSubscribeUrl()(implicit request: Request[AnyContent]): String = {
    val continueUrl = java.net.URLEncoder.encode(request.uri, "UTF-8")
    val baseUrl = appConfig.`agent-subscription-frontend-base-url`
    s"$baseUrl/agent-subscription/check-business-type?continue=$continueUrl"
  }

  private def ggAgentAuthorise(agentAccess: AgentAccess, enrolments: Enrolments)(
    implicit request: Request[AnyContent]): HMRCAgentAuthorisation =
    agentAccess match {
      case RequireMTDAgentEnrolment if enrolments.getEnrolment("HMRC-AS-AGENT").isDefined =>
        HMRCAgentAuthorisationSuccessful
      case DenyAnyAgentAffinityUser =>
        HMRCAgentAuthorisationDenied
      case _ =>
        HMRCAgentAuthorisationFailed(agentSubscribeUrl)
    }

  private def ggAuthorised(
                            predicate: Predicate,
                            authConfig: AuthConfig,
                            formTemplate: FormTemplate,
                            authGivenRetrievals: MaterialisedRetrievals => Future[AuthResult]
                          )(implicit request: Request[AnyContent], hc: HeaderCarrier) = {
    import uk.gov.hmrc.auth.core.retrieve.~

    authorised(predicate)
      .retrieve(defaultRetrievals) {
        case authProviderId ~ enrolments ~ affinityGroup ~ internalId ~ externalId ~ userDetailsUri ~ credentialStrength ~ agentCode =>
          for {
            userDetails <- authConnector.getUserDetails(userDetailsUri.get)
            retrievals = MaterialisedRetrievals(
              authProviderId,
              enrolments,
              affinityGroup,
              internalId,
              externalId,
              userDetails,
              credentialStrength,
              agentCode)
            result <- authGivenRetrievals(retrievals)
          } yield result
      }
      .recover(handleErrorCondition(request, authConfig, formTemplate))
  }

  def idForLog(id: LegacyCredentials) = id match {
    case GGCredId(str)   => str
    case VerifyPid(str)  => str
    case PAClientId(str) => str
    case OneTimeLogin    => "One Time Login"
  }

  private def handleErrorCondition(
    request: Request[AnyContent],
    authConfig: AuthConfig,
    formTemplate: FormTemplate): PartialFunction[scala.Throwable, AuthResult] = {
    case _: InsufficientEnrolments =>
      authConfig match {
        case _: AuthConfigWithEnrolment =>
          Logger.debug("Enrolment required")
          AuthRedirect(uk.gov.hmrc.gform.gform.routes.EnrolmentController.showEnrolment(formTemplate._id, None).url)
        case _ =>
          Logger.debug("Auth Failed")
          AuthRedirectFlashingFormname(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments().url)
      }
    case _: NoActiveSession =>
      Logger.debug("No Active Session")
      val continueUrl = java.net.URLEncoder.encode(appConfig.`gform-frontend-base-url` + request.uri, "UTF-8")
      val ggLoginUrl = appConfig.`government-gateway-sign-in-url`
      val url = s"$ggLoginUrl?continue=$continueUrl"
      AuthRedirectFlashingFormname(url)
    case otherException =>
      Logger.debug(s"Exception thrown on authorization with message : ${otherException.getMessage}")
      throw otherException
  }

  private def removeEeittAuthIdFromSession(
    request: Request[AnyContent],
    authConfig: AuthConfig
  ): Request[AnyContent] = authConfig match {

    // a bit of session clean up due to the session's size restrictions.
    // The registrationNumber/arn passed by eeitt-auth in the session is saved in the user's enrolments field after
    // successful authentication, in which case there is no need to keep it in the session anymore
    case _: EEITTAuthConfig =>
      val sessionCookie =
        Session.encodeAsCookie(request.session - EEITTAuthConfig.nonAgentIdName - EEITTAuthConfig.agentIdName)
      val updatedCookies = request.cookies
        .filterNot(cookie => cookie.name.equals(Session.COOKIE_NAME))
        .toSeq :+ sessionCookie
      val updatedHeaders = request.headers.replace(HeaderNames.COOKIE -> Cookies.encodeCookieHeader(updatedCookies))
      Request[AnyContent](request.copy(headers = updatedHeaders), request.body)
    case _ => request
  }
}

sealed trait HMRCAgentAuthorisation
final object HMRCAgentAuthorisationSuccessful extends HMRCAgentAuthorisation
final object HMRCAgentAuthorisationDenied extends HMRCAgentAuthorisation
case class HMRCAgentAuthorisationFailed(subscribeUrl: String) extends HMRCAgentAuthorisation

sealed trait AuthCache {
  def retrievals: MaterialisedRetrievals
  def formTemplate: FormTemplate
}

case class AuthCacheWithForm(
  retrievals: MaterialisedRetrievals,
  form: Form,
  formTemplate: FormTemplate
) extends AuthCache

case class AuthCacheWithoutForm(
  retrievals: MaterialisedRetrievals,
  formTemplate: FormTemplate
) extends AuthCache
