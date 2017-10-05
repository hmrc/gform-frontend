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

package uk.gov.hmrc.gform.controllers

import play.api.http.HeaderNames
import play.api.Logger
import play.api.mvc.Results._
import play.api.mvc._
import play.api.i18n.I18nSupport
import uk.gov.hmrc._
import uk.gov.hmrc.auth.core.authorise._
import uk.gov.hmrc.auth.core.retrieve.{ AuthProvider, AuthProviders, LegacyCredentials, Retrievals, ~ }
import uk.gov.hmrc.auth.core.retrieve.{ GGCredId, OneTimeLogin, PAClientId, VerifyPid }
import uk.gov.hmrc.auth.core.{ AuthorisedFunctions, InsufficientEnrolments, NoActiveSession }
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.auth.{ AuthModule, EeittAuthorisationFailed, EeittAuthorisationSuccessful }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.Future

class AuthenticatedRequestActions(
    gformConnector: GformConnector,
    authMod: AuthModule,
    configModule: ConfigModule,
    whiteListUser: List[String],
    i18nSupport: I18nSupport
) extends AuthorisedFunctions {

  import i18nSupport._
  val authConnector = authMod.authConnector
  val eeittDelegate = authMod.eeittAuthorisationDelegate
  // format: OFF
  val defaultRetrievals = Retrievals.authProviderId     and Retrievals.allEnrolments  and
                          Retrievals.affinityGroup      and Retrievals.internalId     and
                          Retrievals.externalId         and Retrievals.userDetailsUri and
                          Retrievals.credentialStrength and Retrievals.agentCode
  // format: ON

  implicit def hc(implicit request: Request[_]): HeaderCarrier = HeaderCarrier.fromHeadersAndSession(request.headers, Some(request.session))

  def async(formTemplateId: FormTemplateId)(f: Request[AnyContent] => AuthCacheWithoutForm => Future[Result]): Action[AnyContent] = Action.async { implicit request =>
    // format: OFF
    for {
      formTemplate <- gformConnector.getFormTemplate(formTemplateId)
      authResult   <- authenticateAndAuthorise(formTemplate, isNewForm = true)
      newRequest   = removeEeittAuthIdFromSession(request, formTemplate.authConfig)
      result       <- authResult match {
        case GGAuthSuccessful(retrievals) => f(newRequest)(AuthCacheWithoutForm(retrievals, formTemplate))
        case AuthenticationWhiteListFailed => Future.successful(Ok(uk.gov.hmrc.gform.views.html.error_template("Non WhiteListed User", "Non WhiteListed User", "You are not authorised to access this form, if you believe you need access talk too: barry.johnson@hmrc.gsi.gov.uk or rob.lees@hmrc.gsi.gov.uk")))
        case otherStatus => handleCommonAuthResults(otherStatus, formTemplate)
      }
    } yield result
    // format: ON
  }

  def async(formId: FormId)(f: Request[AnyContent] => AuthCacheWithForm => Future[Result]): Action[AnyContent] = Action.async { implicit request =>
    // format: OFF
    for {
      form         <- gformConnector.getForm(formId)
      formTemplate <- gformConnector.getFormTemplate(form.formTemplateId)
      authResult   <- authenticateAndAuthorise(formTemplate, isNewForm = false)
      newRequest    = removeEeittAuthIdFromSession(request, formTemplate.authConfig)
      result       <- authResult match {
        case GGAuthSuccessful(retrievals) => checkUser(form, retrievals)(f(newRequest)(AuthCacheWithForm(retrievals, form, formTemplate)))
        case AuthenticationWhiteListFailed => Future.successful(Ok(uk.gov.hmrc.gform.views.html.error_template("Non WhiteListed User", "Non WhiteListed User", "Non WhiteListed User")))
        case otherStatus => handleCommonAuthResults(otherStatus, formTemplate)
      }
    } yield result
    // format: ON
  }

  private def handleCommonAuthResults(result: AuthResult, formTemplate: FormTemplate)(implicit hc: HeaderCarrier) = {
    result match {
      case AuthenticationFailed(loginUrl) => Future.successful(Redirect(loginUrl))
      case AuthorisationFailed(errorUrl) => Future.successful(Redirect(errorUrl).flashing("formTitle" -> formTemplate.formName))
      case EnrolmentRequired => Future.successful(Redirect(routes.EnrolmentController.showEnrolment(formTemplate._id, None).url))
      case GGAuthSuccessful(_) => Future.failed(new RuntimeException("Invalid state: GGAuthSuccessful case should not be handled here"))
    }
  }

  private def checkUser(form: Form, retrievals: Retrievals)(actionResult: Future[Result]): Future[Result] = {
    if (form.userId.value == retrievals.userDetails.groupIdentifier)
      actionResult
    else
      Future.successful(Forbidden)
  }

  private def authenticateAndAuthorise(template: FormTemplate, isNewForm: Boolean)(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[AuthResult] = {
    template.authConfig match {
      case authConfig: EEITTAuthConfig => performEEITTAuth(authConfig, isNewForm)
      case authConfig => performHMRCAuth(authConfig, isNewForm)
    }
  }

  private def performEEITTAuth(authConfig: EEITTAuthConfig, isNewForm: Boolean)(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[AuthResult] = {
    ggAuthorised(AuthProviders(AuthProvider.GovernmentGateway), authConfig, isNewForm).flatMap {
      case ggSuccessfulAuth @ GGAuthSuccessful(retrievals) =>
        eeittDelegate.authenticate(authConfig.regimeId, retrievals.userDetails).map {
          case EeittAuthorisationSuccessful => updateEnrolments(ggSuccessfulAuth, request)
          case EeittAuthorisationFailed(eeittLoginUrl) => AuthorisationFailed(eeittLoginUrl)
        }
      case otherAuthResults => Future.successful(otherAuthResults)
    }
  }

  private def updateEnrolments(authSuccessful: GGAuthSuccessful, request: Request[_]): GGAuthSuccessful = {
    // the registrationNumber will be stored in the session by eeittAuth
    val nonAgentUpdate = request.session.get(EEITTAuthConfig.nonAgentIdName) match {
      case Some(regNum) =>
        val newEnrolment = Enrolment(AuthConfig.eeittAuth).withIdentifier(EEITTAuthConfig.nonAgentIdName, regNum)
        val newEnrolments = Enrolments(authSuccessful.retrievals.enrolments.enrolments + newEnrolment)
        authSuccessful.copy(retrievals = authSuccessful.retrievals.copy(enrolments = newEnrolments))
      case None => authSuccessful
    }

    request.session.get(EEITTAuthConfig.agentIdName) match {
      case Some(regNum) =>
        val newEnrolment = Enrolment(AuthConfig.eeittAuth).withIdentifier(EEITTAuthConfig.agentIdName, regNum)
        val newEnrolments = Enrolments(authSuccessful.retrievals.enrolments.enrolments + newEnrolment)
        nonAgentUpdate.copy(retrievals = nonAgentUpdate.retrievals.copy(enrolments = newEnrolments))
      case None => nonAgentUpdate
    }
  }

  private def performHMRCAuth(authConfig: AuthConfig, isNewForm: Boolean)(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[AuthResult] = {
    val predicate = authConfig match {
      case config: AuthConfigWithEnrolment => AuthProviders(AuthProvider.GovernmentGateway) and Enrolment(config.serviceId.value)
      case _ => AuthProviders(AuthProvider.GovernmentGateway)
    }
    ggAuthorised(predicate, authConfig, isNewForm)
  }

  private def ggAuthorised(predicate: Predicate, authConfig: AuthConfig, isNewForm: Boolean)(implicit request: Request[AnyContent], hc: HeaderCarrier) = {
    authorised(predicate).retrieve(defaultRetrievals) {
      case authProviderId ~ enrolments ~ affinityGroup ~ internalId ~ externalId ~ userDetailsUri ~ credentialStrength ~ agentCode =>
        for {
          userDetails <- authConnector.getUserDetails(userDetailsUri.get)
          _ <- whiteListing(userDetails, authProviderId, isNewForm)
          retrievals = gform.auth.models.Retrievals(authProviderId, enrolments, affinityGroup, internalId, externalId, userDetails, credentialStrength, agentCode)
        } yield GGAuthSuccessful(retrievals)
    }.recover(handleErrorCondition(request, authConfig))
  }

  def log(id: LegacyCredentials) = id match {
    case GGCredId(str) => str
    case VerifyPid(str) => str
    case PAClientId(str) => str
    case OneTimeLogin => "One Time Login"
  }

  case class WhiteListException(id: LegacyCredentials) extends Exception

  private def whiteListing(userDetails: UserDetails, authId: LegacyCredentials, isNewForm: Boolean)(implicit hc: HeaderCarrier): Future[Unit] = {
    if (isNewForm) {
      for {
        isValid <- gformConnector.whiteList(userDetails.email)
      } yield isValid match {
        case Some(idx) =>
          Logger.info(s"Passed successful through white listing: $idx user index")
          ()
        case None => throw WhiteListException(authId)
      }
    } else Future.successful(())
  }

  private def handleErrorCondition(request: Request[AnyContent], authConfig: AuthConfig): PartialFunction[scala.Throwable, AuthResult] = {
    case _: InsufficientEnrolments => authConfig match {
      case _: AuthConfigWithEnrolment => EnrolmentRequired
      case _ => AuthorisationFailed(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments().url)
    }
    case _: NoActiveSession =>
      val continueUrl = java.net.URLEncoder.encode(configModule.appConfig.`gform-frontend-base-url` + request.uri, "UTF-8")
      val ggLoginUrl = configModule.appConfig.`government-gateway-sign-in-url`
      val url = s"${ggLoginUrl}?continue=${continueUrl}"
      AuthenticationFailed(url)
    case x: WhiteListException =>
      Logger.warn(s"user failed whitelisting and is denied access : ${log(x.id)}")
      AuthenticationWhiteListFailed
    case otherException => throw otherException
  }

  private def removeEeittAuthIdFromSession(
    request: Request[AnyContent],
    authConfig: AuthConfig
  ): Request[AnyContent] = authConfig match {

    // a bit of session clean up due to the session's size restrictions.
    // The registrationNumber/arn passed by eeitt-auth in the session is saved in the user's enrolments field after
    // successful authentication, in which case there is no need to keep it in the session anymore
    case _: EEITTAuthConfig =>
      val sessionCookie = Session.encodeAsCookie(request.session - EEITTAuthConfig.nonAgentIdName - EEITTAuthConfig.agentIdName)
      val updatedCookies = request.cookies.filterNot(cookie => cookie.name.equals(Session.COOKIE_NAME)).toSeq :+ sessionCookie
      val updatedHeaders = request.headers.replace(HeaderNames.COOKIE -> Cookies.encodeCookieHeader(updatedCookies))
      Request[AnyContent](request.copy(headers = updatedHeaders), request.body)
    case _ => request
  }
}

sealed trait AuthCache {
  def retrievals: gform.auth.models.Retrievals
  def formTemplate: FormTemplate
}

case class AuthCacheWithForm(
  retrievals: gform.auth.models.Retrievals,
  form: Form,
  formTemplate: FormTemplate
) extends AuthCache

case class AuthCacheWithoutForm(
  retrievals: gform.auth.models.Retrievals,
  formTemplate: FormTemplate
) extends AuthCache
