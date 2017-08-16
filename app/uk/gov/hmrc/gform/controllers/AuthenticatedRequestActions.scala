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

import play.api.mvc.{ Action, AnyContent, Request, Result }
import play.api.mvc.Results._
import uk.gov.hmrc.auth.core.AuthorisedFunctions
import uk.gov.hmrc.auth.core.retrieve.{ AuthProvider, AuthProviders, Retrievals, ~ }
import uk.gov.hmrc._
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthConfigModule, FormTemplate, FormTemplateId }
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthenticatedRequestActions(gformConnector: GformConnector, authMod: AuthModule, configModule: ConfigModule) extends AuthorisedFunctions {
  val authConnector = authMod.authConnector
  val eeittDelegate = authMod.eeittAuthorisationDelegate

  implicit def hc(implicit request: Request[_]): HeaderCarrier = HeaderCarrier.fromHeadersAndSession(request.headers, Some(request.session))

  def async(formTemplateIdOpt: Option[FormTemplateId] = None, formId: Option[FormId] = None)(f: AuthenticatedRequest => Future[Result]): Action[AnyContent] = Action.async { implicit request =>

    // For security reasons every page access requires user authorisation.
    // TODO: Network round trips should be reduced
    // TODO: Implement caching of at least some parameters to avoid calling the backend so many times
    val formTemplateF = formTemplateIdOpt match {
      case Some(formTemplateId) => gformConnector.getFormTemplate(formTemplateId)
      case None => for {
        form <- gformConnector.getForm(formId.get)
        formTemplate <- gformConnector.getFormTemplate(form.formTemplateId)
      } yield formTemplate
    }

    formTemplateF.flatMap { formTemplate =>
      formTemplate.authConfig.authModule match {
        case AuthConfigModule("legacyEEITTAuth") => performEEITTAuth(formTemplate, f)
        case AuthConfigModule("hmrc") => performHMRCAuth(f)
        case others => Future.failed(new RuntimeException(s"Invalid authModule value in template's authConfig section: ${others.value}"))
      }
    }
  }

  private def performEEITTAuth(formTemplate: FormTemplate, f: AuthenticatedRequest => Future[Result])(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[Result] = {
    authorised(
      AuthProviders(AuthProvider.GovernmentGateway)
    ).retrieve(
        Retrievals.authProviderId and
          Retrievals.allEnrolments and
          Retrievals.affinityGroup and
          Retrievals.internalId and
          Retrievals.externalId and
          Retrievals.userDetailsUri and
          Retrievals.credentialStrength and
          Retrievals.agentCode
      ) {
          case authProviderId ~ enrolments ~ affinityGroup ~ internalId ~ externalId ~ userDetailsUri ~ credentialStrength ~ agentCode =>
            authConnector.getUserDetails(userDetailsUri.get).flatMap { userDetails =>
              eeittDelegate.legacyAuth(formTemplate, userDetails).flatMap {
                case Ok =>
                  val retrievals = gform.auth.models.Retrievals(authProviderId, enrolments, affinityGroup, internalId, externalId, userDetails, credentialStrength, agentCode)
                  f(AuthenticatedRequest(retrievals, request))
                case authRedirect => Future.successful(authRedirect)
              }
            }
        }.recover(redirectToGGLogin(request))
  }

  private def performHMRCAuth(f: AuthenticatedRequest => Future[Result])(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[Result] = {
    authorised(
      AuthProviders(AuthProvider.GovernmentGateway)
    ).retrieve(
        Retrievals.authProviderId and
          Retrievals.allEnrolments and
          Retrievals.affinityGroup and
          Retrievals.internalId and
          Retrievals.externalId and
          Retrievals.userDetailsUri and
          Retrievals.credentialStrength and
          Retrievals.agentCode
      ) {
          case authProviderId ~ enrolments ~ affinityGroup ~ internalId ~ externalId ~ userDetailsUri ~ credentialStrength ~ agentCode =>

            val retrievalsF = authConnector.getUserDetails(userDetailsUri.get).map {
              gform.auth.models.Retrievals(authProviderId, enrolments, affinityGroup, internalId, externalId, _, credentialStrength, agentCode)
            }

            retrievalsF.flatMap(retrievals => f(AuthenticatedRequest(retrievals, request)))
        }.recover(redirectToGGLogin(request))
  }

  private def redirectToGGLogin(request: Request[AnyContent]): PartialFunction[scala.Throwable, Result] = {
    case _ =>
      val continueUrl = configModule.appConfig.`gform-frontend-base-url` + request.uri
      val ggLoginUrl = configModule.appConfig.`government-gateway-sign-in-url`
      val parameters = Map("continue" -> Seq(continueUrl))
      Redirect(ggLoginUrl, parameters)
  }
}

case class AuthenticatedRequest(retrievals: gform.auth.models.Retrievals, request: Request[AnyContent])

object AuthenticatedRequest {
  implicit def retrievals(implicit authenticatedRequest: AuthenticatedRequest): gform.auth.models.Retrievals = authenticatedRequest.retrievals
  implicit def request(implicit authenticatedRequest: AuthenticatedRequest): Request[AnyContent] = authenticatedRequest.request
}
