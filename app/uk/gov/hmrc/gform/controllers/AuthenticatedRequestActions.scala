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

package uk.gov.hmrc.gform
package controllers

import cats.data.NonEmptyList
import cats.instances.future._
import cats.syntax.applicative._
import play.api.Logger
import play.api.http.HeaderNames
import play.api.i18n.I18nSupport
import play.api.mvc.Results._
import play.api.mvc._
import uk.gov.hmrc.auth.core.{ AuthConnector => _, _ }
import uk.gov.hmrc.gform.auth._
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Enrolment => _, _ }
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.auth.core.retrieve.{ GGCredId, LegacyCredentials, OneTimeLogin, PAClientId, VerifyPid }
import uk.gov.hmrc.auth.core.retrieve.v2._
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.gform.obligation.ObligationService
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, TaxPeriods }

import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

class AuthenticatedRequestActions(
  gformConnector: GformConnector,
  authService: AuthService,
  appConfig: AppConfig,
  frontendAppConfig: FrontendAppConfig,
  val authConnector: AuthConnector,
  i18nSupport: I18nSupport,
  errResponder: ErrResponder,
  obligationService: ObligationService
) extends AuthorisedFunctions {

  def getAffinityGroup(implicit request: Request[AnyContent]): Unit => Future[Option[AffinityGroup]] =
    Function.const {

      val predicate = AuthProviders(AuthProvider.GovernmentGateway)

      authorised(predicate)
        .retrieve(Retrievals.affinityGroup) {
          case affinityGroup => Future.successful(affinityGroup)
        }
    }

  import i18nSupport._

  implicit def hc(implicit request: Request[_]): HeaderCarrier =
    HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

  def checkEnrolment(serviceId: ServiceId, identifiers: NonEmptyList[Identifier])(
    implicit hc: HeaderCarrier): Future[CheckEnrolmentsResult] = {

    val predicate = Enrolment(serviceId.value)

    authorised(predicate)
      .retrieve(Retrievals.allEnrolments) {
        case enrolments => checkIdentifiers(identifiers)(enrolments).pure[Future]
      }
  }

  private def toIdentifier(ei: EnrolmentIdentifier): Identifier = Identifier(ei.key, ei.value)

  private def checkIdentifiers(identifiers: NonEmptyList[Identifier])(enrolments: Enrolments): CheckEnrolmentsResult = {

    val matIdentifiers: Set[Identifier] = enrolments.enrolments.flatMap(_.identifiers).map(toIdentifier)
    if (identifiers.toList.toSet.subsetOf(matIdentifiers))
      EnrolmentSuccessful
    else
      EnrolmentFailed
  }

  def keepAlive(): Action[AnyContent] = Action.async { implicit request =>
    val predicate = AuthProviders(AuthProvider.GovernmentGateway)
    for {
      authResult <- ggAuthorised(request)(AuthSuccessful(_).pure[Future])(RecoverAuthResult.noop)(predicate)
      result <- authResult match {
                 case AuthSuccessful(retrievals) => Future.successful(Ok("success"))
                 case _                          => errResponder.forbidden(request, "Access denied")
               }
    } yield result
  }

  def async(formTemplateId: FormTemplateId, lang: Option[String])(
    f: Request[AnyContent] => AuthCacheWithoutForm => Future[Result]): Action[AnyContent] = Action.async {
    implicit request =>
      for {
        formTemplate <- gformConnector.getFormTemplate(formTemplateId)
        authResult <- authService
                       .authenticateAndAuthorise(
                         formTemplate,
                         lang,
                         request.uri,
                         getAffinityGroup,
                         ggAuthorised(request)(authUserWhitelist(_)))
        newRequest = removeEeittAuthIdFromSession(request, formTemplate.authConfig)
        obligations <- obligationService.lookupObligations(formTemplate)
        result <- handleAuthResults(
                   authResult,
                   formTemplate,
                   request,
                   onSuccess = retrievals => f(newRequest)(AuthCacheWithoutForm(retrievals, formTemplate, obligations))
                 )
      } yield result
  }

  def asyncGGAuth(formTemplateId: FormTemplateId)(
    f: Request[AnyContent] => AuthCacheWithoutForm => Future[Result]): Action[AnyContent] = Action.async {
    implicit request =>
      val predicate = AuthProviders(AuthProvider.GovernmentGateway)

      for {
        formTemplate <- gformConnector.getFormTemplate(formTemplateId)
        authResult   <- ggAuthorised(request)(AuthSuccessful(_).pure[Future])(RecoverAuthResult.noop)(predicate)
        obligations  <- obligationService.lookupObligations(formTemplate)
        result <- authResult match {
                   case AuthSuccessful(retrievals) =>
                     f(request)(AuthCacheWithoutForm(retrievals, formTemplate, obligations))
                   case _ => errResponder.forbidden(request, "Access denied")
                 }
      } yield result
  }

  def async(formTemplateId: FormTemplateId, lang: Option[String], maybeAccessCode: Option[AccessCode])(
    f: Request[AnyContent] => AuthCacheWithForm => Future[Result]): Action[AnyContent] =
    Action.async { implicit request =>
      for {
        formTemplate <- gformConnector.getFormTemplate(formTemplateId)
        authResult <- authService
                       .authenticateAndAuthorise(
                         formTemplate,
                         lang,
                         request.uri,
                         getAffinityGroup,
                         ggAuthorised(request)(AuthSuccessful(_).pure[Future]))
        newRequest = removeEeittAuthIdFromSession(request, formTemplate.authConfig)
        result <- handleAuthResults(
                   authResult,
                   formTemplate,
                   request,
                   onSuccess = withForm(f(newRequest))(maybeAccessCode, formTemplate)
                 )
      } yield result
    }

  private def withForm(f: AuthCacheWithForm => Future[Result])(
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate)(retrievals: MaterialisedRetrievals)(implicit hc: HeaderCarrier): Future[Result] =
    for {
      form        <- gformConnector.getForm(FormId(retrievals.userDetails, formTemplate._id, maybeAccessCode))
      obligations <- obligationService.lookupObligationsMultiple(formTemplate)
      result      <- f(AuthCacheWithForm(retrievals, form, formTemplate, obligations))
    } yield result

  private def authUserWhitelist(retrievals: MaterialisedRetrievals)(
    implicit
    hc: HeaderCarrier): Future[AuthResult] =
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

  private def handleAuthResults(
    result: AuthResult,
    formTemplate: FormTemplate,
    request: Request[_],
    onSuccess: MaterialisedRetrievals => Future[Result]
  )(
    implicit
    hc: HeaderCarrier): Future[Result] =
    result match {
      case AuthSuccessful(retrievals)       => onSuccess(updateEnrolments(formTemplate.authConfig, retrievals, request))
      case AuthRedirect(loginUrl, flashing) => Redirect(loginUrl).flashing(flashing: _*).pure[Future]
      case AuthRedirectFlashingFormName(loginUrl) =>
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

  private def updateEnrolments(
    authConfig: AuthConfig,
    retrievals: MaterialisedRetrievals,
    request: Request[_]): MaterialisedRetrievals = {
    // the registrationNumber will be stored in the session by eeittAuth
    // is this needed for new form and existing form?
    def updateFor(authBy: String): Option[MaterialisedRetrievals] =
      request.session.get(authBy).map { regNum =>
        val newEnrolment = Enrolment(EEITTAuthConfig.eeittAuth).withIdentifier(authBy, regNum)
        val newEnrolments = Enrolments(retrievals.enrolments.enrolments + newEnrolment)
        retrievals.copy(enrolments = newEnrolments)
      }

    authConfig match {
      case EeittModule(_) =>
        updateFor(EEITTAuthConfig.nonAgentIdName)
          .orElse(updateFor(EEITTAuthConfig.agentIdName))
          .getOrElse(retrievals)
      case _ => retrievals
    }
  }

  private def removeEeittAuthIdFromSession(
    request: Request[AnyContent],
    authConfig: AuthConfig
  ): Request[AnyContent] = authConfig match {

    // a bit of session clean up due to the session's size restrictions.
    // The registrationNumber/arn passed by eeitt-auth in the session is saved in the user's enrolments field after
    // successful authentication, in which case there is no need to keep it in the session anymore
    case EeittModule(_) =>
      val sessionCookie =
        Session.encodeAsCookie(request.session - EEITTAuthConfig.nonAgentIdName - EEITTAuthConfig.agentIdName)
      val updatedCookies = request.cookies
        .filterNot(cookie => cookie.name.equals(Session.COOKIE_NAME))
        .toSeq :+ sessionCookie
      val updatedHeaders = request.headers.replace(HeaderNames.COOKIE -> Cookies.encodeCookieHeader(updatedCookies))
      Request[AnyContent](request.copy(headers = updatedHeaders), request.body)
    case _ => request
  }

  val defaultRetrievals = Retrievals.authProviderId and Retrievals.allEnrolments and
    Retrievals.affinityGroup and Retrievals.internalId and
    Retrievals.externalId and Retrievals.userDetailsUri and
    Retrievals.credentialStrength and Retrievals.agentCode

  private def ggAuthorised(
    request: Request[AnyContent]
  )(
    authGivenRetrievals: MaterialisedRetrievals => Future[AuthResult]
  )(
    recoverPF: PartialFunction[Throwable, AuthResult]
  )(
    predicate: Predicate
  ): Future[AuthResult] = {
    import uk.gov.hmrc.auth.core.retrieve.~

    implicit val hc: HeaderCarrier =
      HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))

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
      .recover(recoverPF orElse RecoverAuthResult.basicRecover(request, appConfig))
  }

  def idForLog(id: LegacyCredentials) = id match {
    case GGCredId(str)   => str
    case VerifyPid(str)  => str
    case PAClientId(str) => str
    case OneTimeLogin    => "One Time Login"
  }
}

sealed trait AuthCache {
  def retrievals: MaterialisedRetrievals
  def formTemplate: FormTemplate
}

case class AuthCacheWithForm(
  retrievals: MaterialisedRetrievals,
  form: Form,
  formTemplate: FormTemplate,
  obligations: Map[HmrcTaxPeriod, TaxPeriods]
) extends AuthCache

case class AuthCacheWithoutForm(
  retrievals: MaterialisedRetrievals,
  formTemplate: FormTemplate
  obligations: Map[HmrcTaxPeriod, TaxPeriods]
) extends AuthCache {
  def toAuthCacheWithForm(form: Form) = AuthCacheWithForm(retrievals, form, formTemplate)
}