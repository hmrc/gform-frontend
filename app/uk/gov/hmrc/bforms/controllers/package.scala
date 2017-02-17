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

package uk.gov.hmrc.bforms

import javax.inject.{ Inject, Singleton }
import play.api.Configuration
import play.api.mvc.{ Action, ActionBuilder, ActionRefiner, AnyContent, Request, Result, Results, WrappedRequest }
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.bforms.models.{ FormTypeId, FormTemplate }
import uk.gov.hmrc.bforms.service.RetrieveService
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.frontend.auth._
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.ExecutionContext.Implicits.global

package controllers {

  case class RequestWithTemplate[B](request: Request[B], formTemplate: FormTemplate) extends WrappedRequest(request)

  class WithFormTemplate private (formTypeId: FormTypeId, version: String)(implicit ec: ExecutionContext) extends ActionRefiner[Request, RequestWithTemplate] {
    protected def refine[A](request: Request[A]): Future[Either[Result, RequestWithTemplate[A]]] = {

      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, Some(request.session))

      RetrieveService.getFormTemplate(formTypeId, version).map {
        case Right(formTemplate)=> Right(RequestWithTemplate(request, formTemplate))
        case Left(error) => Left(Results.BadRequest(error))
      }
    }
  }

  object WithFormTemplate {
    def apply(formTypeId: FormTypeId, version: String)(implicit ec: ExecutionContext) = new WithFormTemplate(formTypeId, version)
  }

  object BformsPageVisibilityPredicate extends uk.gov.hmrc.play.frontend.auth.PageVisibilityPredicate {
    def apply(authContext: AuthContext, request: Request[AnyContent]): Future[PageVisibilityResult] =
      Future.successful(PageIsVisible)
  }

  class BformsAuthenticationProvider(configuration: Configuration) extends GovernmentGateway {

    private val bformsFrontendBaseUrl = configuration.getString("bforms-frontend-base-url").getOrElse("")
    private val governmentGatewaySignInUrl = configuration.getString("government-gateway-sign-in-url").getOrElse("")

    override def redirectToLogin(implicit request: Request[_]): Future[Result] = {
      val queryStringParams = Map("continue" -> Seq(bformsFrontendBaseUrl + request.uri))
      Future.successful(Redirect(loginURL, queryStringParams))
    }

    def continueURL: String = "not used since we override redirectToLogin"

    def loginURL: String = governmentGatewaySignInUrl
  }

  trait SecuredActions extends uk.gov.hmrc.play.frontend.auth.Actions {
    val SecureWithTemplate: (FormTypeId, String) => (AuthContext => RequestWithTemplate[AnyContent] => Result) => Action[AnyContent]
    val SecureWithTemplateAsync: (FormTypeId, String) => (AuthContext => RequestWithTemplate[AnyContent] => Future[Result]) => Action[AnyContent]
  }

  @Singleton
  class SecuredActionsImpl @Inject()(configuration: Configuration, val authConnector: AuthConnector) extends SecuredActions {

    private def ActionWithTemplate(
      formTypeId: FormTypeId,
      version: String
    )(
      implicit ec: ExecutionContext
    ): ActionBuilder[RequestWithTemplate] =
      Action.andThen(WithFormTemplate(formTypeId, version))

    // Workaround for impossible Action composition thanks to UserActions#AuthenticatedBy from play-authorised-frontend being no ActionBuilder
    private def SecureActionWithTemplate(authenticatedBy: UserActions#AuthenticatedBy)(
      formTypeId: FormTypeId,
      version: String
    )(body: AuthContext => RequestWithTemplate[AnyContent] => Result): Action[AnyContent] = authenticatedBy.async { authContext => request =>
      ActionWithTemplate(formTypeId, version).apply(body(authContext))(request)
    }

    // Workaround for impossible Action composition thanks to UserActions#AuthenticatedBy from play-authorised-frontend being no ActionBuilder
    private def SecureActionWithTemplateAsync(authenticatedBy: UserActions#AuthenticatedBy)(
      formTypeId: FormTypeId,
      version: String
    )(body: AuthContext => RequestWithTemplate[AnyContent] => Future[Result]): Action[AnyContent] = authenticatedBy.async { authContext => request =>
      ActionWithTemplate(formTypeId, version).async(body(authContext))(request)
    }

    private val authenticatedBy = AuthenticatedBy(new BformsAuthenticationProvider(configuration), BformsPageVisibilityPredicate)

    override val SecureWithTemplate = SecureActionWithTemplate(authenticatedBy) _
    override val SecureWithTemplateAsync = SecureActionWithTemplateAsync(authenticatedBy) _
  }

}
