/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.testonly

import play.api.Mode
import play.api.libs.json.Json
import cats.implicits._
import play.api.mvc._

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.SdesDestination
import uk.gov.hmrc.http.SessionKeys
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class ObjectStoreAdminController(
  controllerComponents: MessagesControllerComponents,
  objectStoreAdminConnector: ObjectStoreAdminConnector,
  mode: Mode
)(implicit ec: ExecutionContext)
    extends FrontendController(controllerComponents: MessagesControllerComponents) {

  private def withValidToken(request: Request[AnyContent])(path: String): Future[Result] =
    mode match {
      case Mode.Prod =>
        Redirect(
          "https://admin.qa.tax.service.gov.uk/object-store-admin-frontend" + path
        ).pure[Future]
      case Mode.Dev | Mode.Test =>
        val maybeToken = request.session.get(SessionKeys.authToken)

        maybeToken match {
          case None => BadRequest("""No token to verify. Use "authModule": "hmrc"""").pure[Future]
          case Some(token) =>
            val payload = Json.obj(
              "token"     -> token,
              "principal" -> "gform-generated-token",
              "permissions" -> List(
                Json.obj(
                  "resourceType"     -> "object-store-admin-frontend",
                  "resourceLocation" -> "*",
                  "actions"          -> List("*")
                ),
                Json.obj(
                  "resourceType"     -> "object-store",
                  "resourceLocation" -> "gform",
                  "actions"          -> List("*")
                ),
                Json.obj(
                  "resourceType"     -> "object-store",
                  "resourceLocation" -> "submission-consolidator",
                  "actions"          -> List("*")
                )
              )
            )
            for {
              isAuthorized <- objectStoreAdminConnector.isAuthorized(token)
              _            <- if (isAuthorized) ().pure[Future] else objectStoreAdminConnector.login(payload).void
            } yield Redirect(
              "http://localhost:8467/object-store-admin-frontend" + path
            )
        }
    }

  def objectStoreZip(envelopeId: EnvelopeId) = Action.async { request =>
    withValidToken(request) {
      "/objects/gform/sdes/" + envelopeId.value + ".zip"
    }

  }

  def objectStoreContent(
    envelopeId: EnvelopeId,
    destination: SdesDestination
  ) = Action.async { request =>
    withValidToken(request) {
      s"/objects/gform/${destination.viewPath}envelopes/${envelopeId.value}"
    }
  }
}
