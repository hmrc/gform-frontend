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
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthConfigModule, FormTemplate, FormTemplateId }
import uk.gov.hmrc.play.http.HeaderCarrier
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

private case class FormAndTemplate(form: Option[Form], template: FormTemplate)

class AuthenticatedRequestActions(gformConnector: GformConnector, authMod: AuthModule, configModule: ConfigModule) extends AuthorisedFunctions {
  val authConnector = authMod.authConnector
  val eeittDelegate = authMod.eeittAuthorisationDelegate

  implicit def hc(implicit request: Request[_]): HeaderCarrier = HeaderCarrier.fromHeadersAndSession(request.headers, Some(request.session))

  def async(formTemplateIdOpt: Option[FormTemplateId] = None, formIdOpt: Option[FormId] = None)(f: AuthenticatedRequest => Future[Result]): Action[AnyContent] = Action.async { implicit request =>
    require(formTemplateIdOpt.isDefined || formIdOpt.isDefined, "formTemplateIdOpt or formIdOpt must be provided")
    val formAndTemplateF = formTemplateIdOpt match {
      case Some(formTemplateId) => gformConnector.getFormTemplate(formTemplateId).map(t => FormAndTemplate(None, t))
      case None => for {
        form <- gformConnector.getForm(formIdOpt.get)
        formTemplate <- gformConnector.getFormTemplate(form.formTemplateId)
      } yield FormAndTemplate(Some(form), formTemplate)
    }

    formAndTemplateF.flatMap { formAndTemplate =>
      formAndTemplate.template.authConfig.authModule match {
        case AuthConfigModule("legacyEEITTAuth") => performEEITTAuth(formAndTemplate, f)
        //case AuthConfigModule("hmrc") => performHMRCAuth(formAndTemplate, f) THIS WILL BE ENABLED IN ANOTHER TICKET
        case others => Future.failed(new RuntimeException(s"Invalid authModule value in template's authConfig section: ${others.value}"))
      }
    }
  }

  private def performEEITTAuth(formAndTemplate: FormAndTemplate, f: AuthenticatedRequest => Future[Result])(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[Result] = {
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
              eeittDelegate.legacyAuth(formAndTemplate.template, userDetails).flatMap {
                case Ok =>
                  val retrievals = gform.auth.models.Retrievals(authProviderId, enrolments, affinityGroup, internalId, externalId, userDetails, credentialStrength, agentCode)
                  f(AuthenticatedRequest(retrievals, request, formAndTemplate.form, formAndTemplate.template))
                case authRedirect => Future.successful(authRedirect)
              }
            }
        }.recover(redirectToGGLogin(request))
  }

  private def performHMRCAuth(formAndTemplate: FormAndTemplate, f: AuthenticatedRequest => Future[Result])(implicit request: Request[AnyContent], hc: HeaderCarrier): Future[Result] = {
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

            retrievalsF.flatMap(retrievals => f(AuthenticatedRequest(retrievals, request, formAndTemplate.form, formAndTemplate.template)))
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

case class AuthenticatedRequest(
  retrievals: gform.auth.models.Retrievals,
  request: Request[AnyContent],
  maybeForm: Option[Form],
  formTemplate: FormTemplate
)

object AuthenticatedRequest {
  implicit def retrievals(implicit authenticatedRequest: AuthenticatedRequest): gform.auth.models.Retrievals = authenticatedRequest.retrievals
  implicit def request(implicit authenticatedRequest: AuthenticatedRequest): Request[AnyContent] = authenticatedRequest.request
  implicit def maybeForm(implicit authenticatedRequest: AuthenticatedRequest): Option[Form] = authenticatedRequest.maybeForm
  implicit def formTemplate(implicit authenticatedRequest: AuthenticatedRequest): FormTemplate = authenticatedRequest.formTemplate
}
