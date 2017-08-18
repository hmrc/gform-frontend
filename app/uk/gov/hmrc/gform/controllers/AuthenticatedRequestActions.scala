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
import uk.gov.hmrc.auth.core.authorise.Enrolment
import uk.gov.hmrc.auth.core.retrieve.{ AuthProvider, AuthProviders, Retrievals, ~ }
import uk.gov.hmrc.auth.core.{ AuthorisedFunctions, InsufficientEnrolments, NoActiveSession }
import uk.gov.hmrc.gform.auth.{ AuthModule, EeittAuthResult, EeittAuthorisationSuccessful, EeittUnauthorisationFailed }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthConfigModule, FormTemplate, FormTemplateId }
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

private case class FormAndTemplate(form: Option[Form], template: FormTemplate)

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

  def async(formTemplateId: FormTemplateId)(f: AuthenticatedRequestWithoutForm => Future[Result]): Action[AnyContent] = Action.async { implicit request =>
    gformConnector.getFormTemplate(formTemplateId).flatMap { formTemplate =>
      authenticateAndAuthorise(FormAndTemplate(None, formTemplate), f.asInstanceOf[AuthenticatedRequest => Future[Result]])
    }
  }

  def async(formId: FormId)(f: AuthenticatedRequestWithForm => Future[Result]): Action[AnyContent] = Action.async { implicit request =>
    for {
      form <- gformConnector.getForm(formId)
      formTemplate <- gformConnector.getFormTemplate(form.formTemplateId)
      result <- authenticateAndAuthorise(FormAndTemplate(Some(form), formTemplate), f.asInstanceOf[AuthenticatedRequest => Future[Result]])
    } yield result
  }

  private def authenticateAndAuthorise(formAndTemplate: FormAndTemplate, f: AuthenticatedRequest => Future[Result])(implicit request: Request[AnyContent], hc: HeaderCarrier) = {
    formAndTemplate.template.authConfig.authModule match {
      case AuthConfigModule("legacyEEITTAuth") => performEEITTAuth(formAndTemplate, f)
      case AuthConfigModule("hmrc") => performHMRCAuth(formAndTemplate, f)
      case others => Future.failed(new RuntimeException(s"Invalid authModule value in template's authConfig section: ${others.value}"))
    }
  }

  private def performEEITTAuth(formAndTemplate: FormAndTemplate, f: AuthenticatedRequest => Future[Result])(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[Result] = {

    def getResult(authResult: EeittAuthResult, retrievals: gform.auth.models.Retrievals) = authResult match {
      case EeittAuthorisationSuccessful =>
        f(buildAuthenticationRequest(retrievals, request, formAndTemplate))
      case EeittUnauthorisationFailed(loginUrl) =>
        Future.successful(Redirect(loginUrl))
    }

    authorised(AuthProviders(AuthProvider.GovernmentGateway)).retrieve(defaultRetrievals) {
      case authProviderId ~ enrolments ~ affinityGroup ~ internalId ~ externalId ~ userDetailsUri ~ credentialStrength ~ agentCode =>
        for {
          userDetails <- authConnector.getUserDetails(userDetailsUri.get)
          retrievals = gform.auth.models.Retrievals(authProviderId, enrolments, affinityGroup, internalId, externalId, userDetails, credentialStrength, agentCode)
          authenticationResult <- eeittDelegate.authenticate(formAndTemplate.template.authConfig.regimeId, userDetails)
          result <- getResult(authenticationResult, retrievals)
        } yield result

    }.recover(handleErrorCondition(request, formAndTemplate))
  }

  private def performHMRCAuth(formAndTemplate: FormAndTemplate, f: AuthenticatedRequest => Future[Result])(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[Result] = {

    val predicate = formAndTemplate.template.authConfig.serviceId match {
      case Some(serviceId) => AuthProviders(AuthProvider.GovernmentGateway) and Enrolment(serviceId.value)
      case None => AuthProviders(AuthProvider.GovernmentGateway)
    }

    authorised(predicate).retrieve(defaultRetrievals) {
      case authProviderId ~ enrolments ~ affinityGroup ~ internalId ~ externalId ~ userDetailsUri ~ credentialStrength ~ agentCode =>
        for {
          userDetails <- authConnector.getUserDetails(userDetailsUri.get)
          retrievals = gform.auth.models.Retrievals(authProviderId, enrolments, affinityGroup, internalId, externalId, userDetails, credentialStrength, agentCode)
          result <- f(buildAuthenticationRequest(retrievals, request, formAndTemplate))
        } yield result

    }.recover(handleErrorCondition(request, formAndTemplate))
  }

  private def handleErrorCondition(request: Request[AnyContent], formAndTemplate: FormAndTemplate): PartialFunction[scala.Throwable, Result] = {
    case _: InsufficientEnrolments =>
      Redirect(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments())
        .flashing("formTitle" -> formAndTemplate.template.formName)

    case _: NoActiveSession =>
      val continueUrl = configModule.appConfig.`gform-frontend-base-url` + request.uri
      val ggLoginUrl = configModule.appConfig.`government-gateway-sign-in-url`
      val parameters = Map("continue" -> Seq(continueUrl))
      Redirect(ggLoginUrl, parameters)

    case otherException => throw otherException
  }

  private def buildAuthenticationRequest(retrievals: gform.auth.models.Retrievals, request: Request[AnyContent], formAndTemplate: FormAndTemplate) = {
    formAndTemplate.form match {
      case Some(form) => AuthenticatedRequestWithForm(retrievals, request, form, formAndTemplate.template)
      case None => AuthenticatedRequestWithoutForm(retrievals, request, formAndTemplate.template)
    }
  }
}

sealed trait AuthenticatedRequest {
  def retrievals: gform.auth.models.Retrievals
  def request: Request[AnyContent]
  def formTemplate: FormTemplate
}

case class AuthenticatedRequestWithForm(
  retrievals: gform.auth.models.Retrievals,
  request: Request[AnyContent],
  form: Form,
  formTemplate: FormTemplate
) extends AuthenticatedRequest

case class AuthenticatedRequestWithoutForm(
  retrievals: gform.auth.models.Retrievals,
  request: Request[AnyContent],
  formTemplate: FormTemplate
) extends AuthenticatedRequest

object AuthenticatedRequest {
  implicit def retrievals(implicit ar: AuthenticatedRequest): gform.auth.models.Retrievals = ar.retrievals
  implicit def request(implicit ar: AuthenticatedRequest): Request[AnyContent] = ar.request
  implicit def theForm(implicit ar: AuthenticatedRequestWithForm): Form = ar.form
  implicit def formTemplate(implicit ar: AuthenticatedRequest): FormTemplate = ar.formTemplate
}
