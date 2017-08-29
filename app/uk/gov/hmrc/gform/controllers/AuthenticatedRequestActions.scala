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
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthConfigModule, FormTemplate, FormTemplateId }
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

  def async(formTemplateId: FormTemplateId)(f: Request[AnyContent] => CacheWithoutForm => Future[Result]): Action[AnyContent] = Action.async { implicit request =>
    gformConnector.getFormTemplate(formTemplateId).flatMap { formTemplate =>
      authenticateAndAuthorise(formTemplate).flatMap {
        case GGAuthSuccessful(retrievals) => f(request)(CacheWithoutForm(retrievals, formTemplate))
        case AuthenticationFailed(loginUrl) => Future.successful(Redirect(loginUrl))
        case AuthorisationFailed(errorUrl) => Future.successful(Redirect(errorUrl).flashing("formTitle" -> formTemplate.formName))
      }
    }
  }

  def async(formId: FormId)(f: Request[AnyContent] => CacheWithForm => Future[Result]): Action[AnyContent] = Action.async { implicit request =>
    val resultF = for {
      form <- gformConnector.getForm(formId)
      formTemplate <- gformConnector.getFormTemplate(form.formTemplateId)
      authResult <- authenticateAndAuthorise(formTemplate)
    } yield (form, formTemplate, authResult)

    resultF.flatMap {
      case (form, formTemplate, authResult) =>
        authResult match {
          case GGAuthSuccessful(retrievals) => f(request)(CacheWithForm(retrievals, form, formTemplate))
          case AuthenticationFailed(loginUrl) => Future.successful(Redirect(loginUrl))
          case AuthorisationFailed(errorUrl) => Future.successful(Redirect(errorUrl).flashing("formTitle" -> formTemplate.formName))
        }
    }
  }

  private def authenticateAndAuthorise(template: FormTemplate)(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[AuthResult] = {
    template.authConfig.authModule match {
      case AuthConfigModule("legacyEEITTAuth") => performEEITTAuth(template)
      case AuthConfigModule("hmrc") => performHMRCAuth(template)
      case others => Future.failed(new RuntimeException(s"Invalid authModule value in template's authConfig section: ${others.value}"))
    }
  }

  private def performEEITTAuth(template: FormTemplate)(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[AuthResult] = {
    ggAuthorised(AuthProviders(AuthProvider.GovernmentGateway)).flatMap {
      case ggSuccessfulAuth @ GGAuthSuccessful(retrievals) =>
        eeittDelegate.authenticate(template.authConfig.regimeId, retrievals.userDetails).map {
          case EeittAuthorisationSuccessful => ggSuccessfulAuth
          case EeittAuthorisationFailed(eeittLoginUrl) => AuthorisationFailed(eeittLoginUrl)
        }
      case otherAuthResults => Future.successful(otherAuthResults)
    }
  }

  private def performHMRCAuth(template: FormTemplate)(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[AuthResult] = {
    val predicate = template.authConfig.serviceId match {
      case Some(serviceId) => AuthProviders(AuthProvider.GovernmentGateway) and Enrolment(serviceId.value)
      case None => AuthProviders(AuthProvider.GovernmentGateway)
    }
    ggAuthorised(predicate)
  }

  private def ggAuthorised(predicate: Predicate)(implicit request: Request[AnyContent], hc: HeaderCarrier) = {
    authorised(predicate).retrieve(defaultRetrievals) {
      case authProviderId ~ enrolments ~ affinityGroup ~ internalId ~ externalId ~ userDetailsUri ~ credentialStrength ~ agentCode =>
        for {
          userDetails <- authConnector.getUserDetails(userDetailsUri.get)
          retrievals = gform.auth.models.Retrievals(authProviderId, enrolments, affinityGroup, internalId, externalId, userDetails, credentialStrength, agentCode)
        } yield GGAuthSuccessful(retrievals)
    }.recover(handleErrorCondition(request))
  }

  private def handleErrorCondition(request: Request[AnyContent]): PartialFunction[scala.Throwable, AuthResult] = {
    case _: InsufficientEnrolments =>
      AuthorisationFailed(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments().url)

    case _: NoActiveSession =>
      val continueUrl = java.net.URLEncoder.encode(configModule.appConfig.`gform-frontend-base-url` + request.uri, "UTF-8")
      val ggLoginUrl = configModule.appConfig.`government-gateway-sign-in-url`
      val url = s"${ggLoginUrl}?continue=${continueUrl}"
      AuthenticationFailed(url)

    case otherException => throw otherException
  }
}

sealed trait Cache {
  def retrievals: gform.auth.models.Retrievals
  def formTemplate: FormTemplate
}

case class CacheWithForm(
  retrievals: gform.auth.models.Retrievals,
  form: Form,
  formTemplate: FormTemplate
) extends Cache

case class CacheWithoutForm(
  retrievals: gform.auth.models.Retrievals,
  formTemplate: FormTemplate
) extends Cache
