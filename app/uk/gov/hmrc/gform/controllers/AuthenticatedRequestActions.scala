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
import java.util.UUID

import play.api.Logger
import play.api.http.HeaderNames
import play.api.i18n.{ I18nSupport, Lang, Langs, MessagesApi }
import play.api.mvc.Results._
import play.api.mvc._

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.auth.core.{ AffinityGroup, AuthConnector => _, _ }
import uk.gov.hmrc.gform.auth._
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormIdData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Enrolment => _, _ }
import uk.gov.hmrc.http.SessionKeys
import uk.gov.hmrc.auth.core.retrieve.{ Retrievals => _ }
import uk.gov.hmrc.auth.core.retrieve.v2._
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.InsufficientEnrolments
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluator, SmartStringEvaluatorFactory }
import uk.gov.hmrc.gform.views.ViewHelpersAlgebra

import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

trait AuthenticatedRequestActionsAlgebra[F[_]] {
  def keepAlive(): Action[AnyContent]

  def authWithoutRetrievingForm(formTemplateId: FormTemplateId, operation: OperationWithoutForm)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => F[Result]): Action[AnyContent]

  def authAndRetrieveForm(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    operation: OperationWithForm)(
    f: Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => F[Result]): Action[AnyContent]
}

class AuthenticatedRequestActions(
  gformConnector: GformConnector,
  authService: AuthService,
  appConfig: AppConfig,
  frontendAppConfig: FrontendAppConfig,
  val authConnector: AuthConnector,
  i18nSupport: I18nSupport,
  langs: Langs,
  actionBuilder: ActionBuilder[Request, AnyContent],
  errResponder: ErrResponder,
  sessionCookieBaker: SessionCookieBaker,
  recalculation: Recalculation[Future, Throwable],
  smartStringEvaluatorFactory: SmartStringEvaluatorFactory
)(
  implicit
  ec: ExecutionContext,
  messagesApi: MessagesApi,
  viewHelpers: ViewHelpersAlgebra
) extends AuthenticatedRequestActionsAlgebra[Future] with AuthorisedFunctions {

  def getAffinityGroup(implicit request: Request[AnyContent]): Unit => Future[Option[AffinityGroup]] =
    _ => {

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
      .recoverWith {
        case ex @ InsufficientEnrolments(enrolment) =>
          Logger.error("tax-enrolment service returned 201, but enrolment check in auth failed", ex)
          CheckEnrolmentsResult.InsufficientEnrolments.pure[Future]
        case ex =>
          Logger.error("tax-enrolment service returned 201, but auth call failed unexpectedly", ex)
          CheckEnrolmentsResult.Failed.pure[Future]
      }
  }

  private def toIdentifier(ei: EnrolmentIdentifier): Identifier = Identifier(ei.key, ei.value)

  private def checkIdentifiers(identifiers: NonEmptyList[Identifier])(enrolments: Enrolments): CheckEnrolmentsResult = {

    val matIdentifiers: Set[Identifier] = enrolments.enrolments.flatMap(_.identifiers).map(toIdentifier)
    if (identifiers.toList.toSet.subsetOf(matIdentifiers))
      CheckEnrolmentsResult.Successful
    else
      CheckEnrolmentsResult.InvalidIdentifiers
  }

  def keepAlive(): Action[AnyContent] = actionBuilder.async { implicit request =>
    val predicate = AuthProviders(AuthProvider.GovernmentGateway)
    for {
      authResult <- ggAuthorised(request)(RecoverAuthResult.noop)(predicate)
      result <- authResult match {
                 case _: AuthSuccessful => Future.successful(Ok("success"))
                 case _                 => errResponder.forbidden(request, "Access denied")
               }
    } yield result
  }

  def getCurrentLanguage(request: Request[AnyContent]) = {
    val maybeLangFromCookie = request.cookies.get(messagesApi.langCookieName).flatMap(c => Lang.get(c.value))
    val lang: Lang = langs.preferred(maybeLangFromCookie.toSeq ++ request.acceptLanguages)
    LangADT.stringToLangADT(lang.code)
  }

  private def getCaseWorkerIdentity(request: Request[AnyContent]): Option[Cookie] =
    request.cookies.get(appConfig.`case-worker-assumed-identity-cookie`)

  private def authWithoutRetrievingForm(formTemplateId: FormTemplateId)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]): Action[AnyContent] =
    async(formTemplateId)(f)

  override def authWithoutRetrievingForm(formTemplateId: FormTemplateId, operation: OperationWithoutForm)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]): Action[AnyContent] =
    authWithoutRetrievingForm(formTemplateId) { request => lang => cache =>
      Permissions.apply(operation, cache.role) match {
        case PermissionResult.Permitted     => f(request)(lang)(cache)
        case PermissionResult.NotPermitted  => errResponder.forbidden(request, "Access denied")
        case PermissionResult.FormSubmitted => errResponder.forbidden(request, "Access denied")
      }
    }

  def asyncNoAuth(formTemplateId: FormTemplateId)(
    f: Request[AnyContent] => LangADT => FormTemplate => Future[Result]): Action[AnyContent] = actionBuilder.async {
    implicit request =>
      implicit val l: LangADT = getCurrentLanguage(request)

      for {
        formTemplate <- gformConnector.getFormTemplate(formTemplateId)
        result       <- f(request)(l)(formTemplate)
      } yield result
  }

  private def async(formTemplateId: FormTemplateId)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]): Action[AnyContent] =
    actionBuilder.async { implicit request =>
      implicit val l: LangADT = getCurrentLanguage(request)

      for {
        _            <- MDCHelpers.addFormTemplateIdToMdc(formTemplateId)
        formTemplate <- gformConnector.getFormTemplate(formTemplateId)
        authResult <- authService
                       .authenticateAndAuthorise(
                         formTemplate,
                         request.uri,
                         getAffinityGroup,
                         ggAuthorised(request),
                         getCaseWorkerIdentity(request))
        newRequest = removeEeittAuthIdFromSession(request, formTemplate.authConfig)
        result <- handleAuthResults(
                   authResult,
                   formTemplate,
                   request,
                   onSuccess =
                     retrievals => role => f(newRequest)(l)(AuthCacheWithoutForm(retrievals, formTemplate, role))
                 )
      } yield result
    }

  def asyncGGAuth(formTemplateId: FormTemplateId)(
    f: Request[AnyContent] => LangADT => AuthCacheWithoutForm => Future[Result]): Action[AnyContent] =
    actionBuilder.async { implicit request =>
      val predicate = AuthProviders(AuthProvider.GovernmentGateway)
      for {
        _            <- MDCHelpers.addFormTemplateIdToMdc(formTemplateId)
        formTemplate <- gformConnector.getFormTemplate(formTemplateId)
        authResult   <- ggAuthorised(request)(RecoverAuthResult.noop)(predicate)
        result <- authResult match {
                   case AuthSuccessful(retrievals, role) =>
                     f(request)(getCurrentLanguage(request))(AuthCacheWithoutForm(retrievals, formTemplate, role))
                   case _ => errResponder.forbidden(request, "Access denied")
                 }
      } yield result
    }

  def authAndRetrieveForm(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    operation: OperationWithForm)(
    f: Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => Future[Result])
    : Action[AnyContent] =
    async(formTemplateId, maybeAccessCode) { request => lang => cache => smartStringEvaluator =>
      Permissions.apply(operation, cache.role, cache.form.status) match {
        case PermissionResult.Permitted    => f(request)(lang)(cache)(smartStringEvaluator)
        case PermissionResult.NotPermitted => errResponder.forbidden(request, "Access denied")
        case PermissionResult.FormSubmitted =>
          Redirect(
            uk.gov.hmrc.gform.gform.routes.AcknowledgementController
              .showAcknowledgement(maybeAccessCode, formTemplateId)).pure[Future]
      }
    }

  def async(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode])(
    f: Request[AnyContent] => LangADT => AuthCacheWithForm => SmartStringEvaluator => Future[Result])
    : Action[AnyContent] =
    actionBuilder.async { implicit request =>
      implicit val l: LangADT = getCurrentLanguage(request)

      for {
        _            <- MDCHelpers.addFormTemplateIdToMdc(formTemplateId)
        _            <- MDCHelpers.addAccessCodeToMdc(maybeAccessCode)
        formTemplate <- gformConnector.getFormTemplate(formTemplateId)
        authResult <- authService
                       .authenticateAndAuthorise(
                         formTemplate,
                         request.uri,
                         getAffinityGroup,
                         ggAuthorised(request),
                         getCaseWorkerIdentity(request))
        newRequest = removeEeittAuthIdFromSession(request, formTemplate.authConfig)
        result <- handleAuthResults(
                   authResult,
                   formTemplate,
                   request,
                   onSuccess = withForm(f(newRequest)(l))(maybeAccessCode, formTemplate)
                 )
      } yield result
    }

  private def withForm(f: AuthCacheWithForm => SmartStringEvaluator => Future[Result])(
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate)(retrievals: MaterialisedRetrievals)(
    role: Role)(implicit hc: HeaderCarrier, l: LangADT): Future[Result] =
    for {
      form <- gformConnector.getForm(FormIdData(retrievals, formTemplate._id, maybeAccessCode))
      _    <- MDCHelpers.addFormIdToMdc(form._id)
      cache = AuthCacheWithForm(retrievals, form, formTemplate, role)
      recalculatedData <- recalculation.recalculateFormData(
                           cache.variadicFormData,
                           formTemplate,
                           cache.retrievals,
                           form.thirdPartyData,
                           form.envelopeId)
      smartStringEvaluator = smartStringEvaluatorFactory.apply(recalculatedData, retrievals, form, formTemplate)
      result <- f(cache)(smartStringEvaluator)
    } yield result

  private def handleAuthResults(
    result: AuthResult,
    formTemplate: FormTemplate,
    request: Request[_],
    onSuccess: MaterialisedRetrievals => Role => Future[Result]
  )(implicit l: LangADT, hc: HeaderCarrier): Future[Result] =
    result match {
      case AuthSuccessful(retrievals: AnonymousRetrievals, role) =>
        onSuccess(retrievals)(role)
      case AuthSuccessful(retrievals: AuthenticatedRetrievals, role) =>
        onSuccess(updateEnrolments(formTemplate.authConfig, retrievals, request))(role)
      case AuthRedirect(loginUrl, flashing) => Redirect(loginUrl).flashing(flashing: _*).pure[Future]
      case AuthAnonymousSession(redirectUrl) =>
        Redirect(redirectUrl)
          .withSession(SessionKeys.sessionId -> s"anonymous-session-${UUID.randomUUID()}")
          .pure[Future]
      case AuthRedirectFlashingFormName(loginUrl) =>
        Redirect(loginUrl).flashing("formTitle" -> formTemplate.formName.value).pure[Future]
      case AuthBlocked(message) =>
        Ok(
          views.html.error_template(
            pageTitle = "Access denied",
            heading = "Access denied",
            message = message,
            frontendAppConfig = frontendAppConfig)(request.messages, viewHelpers)).pure[Future]
      case AuthForbidden(message) =>
        errResponder.forbidden(request, message)
    }

  private def updateEnrolments(
    authConfig: AuthConfig,
    retrievals: AuthenticatedRetrievals,
    request: Request[_]): AuthenticatedRetrievals = {
    // the registrationNumber will be stored in the session by eeittAuth
    // is this needed for new form and existing form?
    def updateFor(authBy: String): Option[AuthenticatedRetrievals] =
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
        .filterNot(cookie => cookie.name.equals(sessionCookieBaker.COOKIE_NAME))
        .toSeq :+ sessionCookie
      val updatedHeaders = request.headers.replace(HeaderNames.COOKIE -> Cookies.encodeCookieHeader(updatedCookies))
      Request[AnyContent](request.withHeaders(updatedHeaders), request.body)
    case _ => request
  }

  val defaultRetrievals = Retrievals.authProviderId and Retrievals.allEnrolments and
    Retrievals.internalId and
    Retrievals.externalId and Retrievals.userDetailsUri and
    Retrievals.credentialStrength and Retrievals.agentCode

  private def ggAuthorised(
    request: Request[AnyContent]
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
        case authProviderId ~ enrolments ~ internalId ~ externalId ~ userDetailsUri ~ credentialStrength ~ agentCode =>
          for {
            userDetails <- authConnector.getUserDetails(userDetailsUri.get)
            retrievals = AuthenticatedRetrievals(
              authProviderId,
              enrolments,
              internalId,
              externalId,
              userDetails,
              credentialStrength,
              agentCode)
            result <- AuthSuccessful(retrievals, ggRoleFromAffinityGroup(userDetails.affinityGroup)).pure[Future]
          } yield result
      }
      .recover(recoverPF orElse RecoverAuthResult.basicRecover(request, appConfig))
  }

  private def ggRoleFromAffinityGroup(affinityGroup: AffinityGroup) = affinityGroup match {
    case AffinityGroup.Individual   => Role.Customer
    case AffinityGroup.Organisation => Role.Customer
    case AffinityGroup.Agent        => Role.Agent
  }
}

sealed trait AuthCache {
  def retrievals: MaterialisedRetrievals
  def formTemplate: FormTemplate
  def role: Role
}

case class AuthCacheWithForm(
  retrievals: MaterialisedRetrievals,
  form: Form,
  formTemplate: FormTemplate,
  role: Role
) extends AuthCache {
  lazy val variadicFormData: VariadicFormData = VariadicFormData.buildFromMongoData(formTemplate, form.formData.toData)
}

case class AuthCacheWithoutForm(
  retrievals: MaterialisedRetrievals,
  formTemplate: FormTemplate,
  role: Role
) extends AuthCache {
  def toAuthCacheWithForm(form: Form) =
    AuthCacheWithForm(retrievals, form, formTemplate, role)
}
