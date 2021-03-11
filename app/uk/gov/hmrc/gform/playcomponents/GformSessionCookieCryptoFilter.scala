/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.playcomponents

import _root_.akka.stream.Materializer
import cats.syntax.eq._
import com.typesafe.config.Config
import play.api.mvc._
import play.api.routing.Router.RequestImplicits._
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.controllers.CookieNames._
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Anonymous, FormTemplate, FormTemplateId }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.{ SessionCookieCrypto, SessionCookieCryptoFilter }

import scala.concurrent.{ ExecutionContext, Future }

class GformSessionCookieCryptoFilter(
  sessionCookieCrypto: SessionCookieCrypto,
  nonanonymouscookieCryptoFilter: SessionCookieCryptoFilter,
  anonymousCookieCryptoFilter: SessionCookieCryptoFilter,
  val sessionBaker: SessionCookieBaker,
  gformConnector: GformConnector,
  config: Config
)(implicit override val ec: ExecutionContext, override val mat: Materializer)
    extends SessionCookieCryptoFilter {

  override protected lazy val encrypter: Encrypter = sessionCookieCrypto.crypto
  override protected lazy val decrypter: Decrypter = sessionCookieCrypto.crypto

  private val AnonymousAuthConfig = "anonymous"
  private val NonAnonymousAuthConfig = "nonanonymous"

  override def apply(next: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] = {

    lazy val authconfigCookieValue = Crypted(rh.cookies.get(authConfigCookieName).fold("")(_.value))

    val formTemplateIdParamIndex: Option[Int] = {
      val handlerDefPath = rh.handlerDef.map(_.path.replaceFirst("/submissions", "/submissions/"))
      val mayContainsFormTemplateId: Option[Array[Boolean]] =
        handlerDefPath.map(_.split("/")).map(v => v.map(v2 => v2.containsSlice("$formTemplateId")))
      mayContainsFormTemplateId.map(_.indexOf(true))
    }

    val maybeFormTemplate: Future[Either[Unit, FormTemplate]] = formTemplateIdParamIndex match {
      case Some(i) if i =!= -1 =>
        val templateId = rh.uri.split("/")(i)
        implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(rh.headers, Some(rh.session))
        gformConnector.getFormTemplate(FormTemplateId(templateId)).map(Right(_))
      case _ =>
        Future.successful(Left(()))
    }

    maybeFormTemplate.flatMap {
      case Right(formTemplate) =>
        val (result, cookieValue) =
          formTemplate.authConfig match {
            case Anonymous =>
              (
                anonymousCookieCryptoFilter(next)(rh.addAttr(FormTemplateKey, formTemplate)),
                encrypter.encrypt(PlainText(AnonymousAuthConfig)))
            case _ =>
              (
                nonanonymouscookieCryptoFilter(next)(rh.addAttr(FormTemplateKey, formTemplate)),
                encrypter.encrypt(PlainText(NonAnonymousAuthConfig)))
          }
        result.map(_.withCookies(Cookie(authConfigCookieName, cookieValue.value)))

      case Left(_) =>
        if (decrypter.decrypt(authconfigCookieValue).value === AnonymousAuthConfig)
          anonymousCookieCryptoFilter(next)(rh)
        else
          nonanonymouscookieCryptoFilter(next)(rh)
    }
  }
}
