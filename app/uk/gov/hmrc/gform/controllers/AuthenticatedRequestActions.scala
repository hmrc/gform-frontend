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

import play.api.mvc.Results._
import play.api.mvc.{ Action, AnyContent, Request, Result }
import uk.gov.hmrc._
import uk.gov.hmrc.auth.core.authorise.{ Enrolment, Predicate }
import uk.gov.hmrc.auth.core.retrieve.{ AuthProvider, AuthProviders, Retrievals, ~ }
import uk.gov.hmrc.auth.core.{ AuthorisedFunctions, InsufficientEnrolments, NoActiveSession }
import uk.gov.hmrc.gform.auth.{ AuthModule, EeittAuthorisationFailed, EeittAuthorisationSuccessful }
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthenticatedRequestActions(gformConnector: GformConnector, authMod: AuthModule, configModule: ConfigModule) extends AuthorisedFunctions {
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
      authResult   <- authenticateAndAuthorise(formTemplate)
      result       <- authResult match {
        case GGAuthSuccessful(retrievals) => f(request)(AuthCacheWithoutForm(retrievals, formTemplate))
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
      authResult   <- authenticateAndAuthorise(formTemplate)
      result       <- authResult match {
        case GGAuthSuccessful(retrievals) => checkUser(form, retrievals)(f(request)(AuthCacheWithForm(retrievals, form, formTemplate)))
        case otherStatus => handleCommonAuthResults(otherStatus, formTemplate)
      }
    } yield result
    // format: ON
  }

  private def handleCommonAuthResults(result: AuthResult, formTemplate: FormTemplate) = {
    result match {
      case AuthenticationFailed(loginUrl) => Future.successful(Redirect(loginUrl))
      case AuthorisationFailed(errorUrl) => Future.successful(Redirect(errorUrl).flashing("formTitle" -> formTemplate.formName))
      case EnrolmentRequired => Future.successful(Redirect(routes.EnrolmentController.showEnrolment(formTemplate._id, None).url))
      case GGAuthSuccessful(_) => Future.failed(new RuntimeException("Invalid state: GGAuthSuccessful case should not be handled here"))
    }
  }

  private def checkUser(form: Form, retrievals: Retrievals)(success: Future[Result]): Future[Result] = {
    if (form.userId.value == retrievals.userDetails.groupIdentifier)
      success
    else
      Future.successful(Forbidden)
  }

  private def authenticateAndAuthorise(template: FormTemplate)(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[AuthResult] = {
    template.authConfig match {
      case authConfig: EEITTAuthConfig => performEEITTAuth(authConfig)
      case authConfig => performHMRCAuth(authConfig)
    }
  }

  private def performEEITTAuth(authConfig: EEITTAuthConfig)(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[AuthResult] = {
    ggAuthorised(AuthProviders(AuthProvider.GovernmentGateway), authConfig).flatMap {
      case ggSuccessfulAuth @ GGAuthSuccessful(retrievals) =>
        eeittDelegate.authenticate(authConfig.regimeId, retrievals.userDetails).map {
          case EeittAuthorisationSuccessful => ggSuccessfulAuth
          case EeittAuthorisationFailed(eeittLoginUrl) => AuthorisationFailed(eeittLoginUrl)
        }
      case otherAuthResults => Future.successful(otherAuthResults)
    }
  }

  private def performHMRCAuth(authConfig: AuthConfig)(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[AuthResult] = {
    val predicate = authConfig match {
      case config: AuthConfigWithEnrolment => AuthProviders(AuthProvider.GovernmentGateway) and Enrolment(config.serviceId.value)
      case _ => AuthProviders(AuthProvider.GovernmentGateway)
    }
    ggAuthorised(predicate, authConfig)
  }

  private def ggAuthorised(predicate: Predicate, authConfig: AuthConfig)(implicit request: Request[AnyContent], hc: HeaderCarrier) = {
    authorised(predicate).retrieve(defaultRetrievals) {
      case authProviderId ~ enrolments ~ affinityGroup ~ internalId ~ externalId ~ userDetailsUri ~ credentialStrength ~ agentCode =>
        for {
          userDetails <- authConnector.getUserDetails(userDetailsUri.get)
          retrievals = gform.auth.models.Retrievals(authProviderId, enrolments, affinityGroup, internalId, externalId, userDetails, credentialStrength, agentCode)
        } yield GGAuthSuccessful(retrievals)
    }.recover(handleErrorCondition(request, authConfig))
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

    case otherException => throw otherException
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
