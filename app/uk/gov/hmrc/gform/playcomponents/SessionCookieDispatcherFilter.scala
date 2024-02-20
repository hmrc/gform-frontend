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

package uk.gov.hmrc.gform.playcomponents

import org.apache.pekko.stream.Materializer
import play.api.http.HeaderNames
import play.api.mvc._
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.CookieNames._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Anonymous, EmailAuthConfig, FormTemplateContext }
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.{ SessionCookieCrypto, SessionCookieCryptoFilter }
import uk.gov.hmrc.gform.views.html

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.sharedmodel.LangADT

class SessionCookieDispatcherFilter(
  sessionCookieCrypto: SessionCookieCrypto,
  hmrcCookieCryptoFilter: SessionCookieCryptoFilter,
  anonymousCookieCryptoFilter: SessionCookieCryptoFilter,
  emailCookieCryptoFilter: SessionCookieCryptoFilter,
  requestHeaderService: RequestHeaderService,
  configModule: ConfigModule,
  playBuiltInsModule: PlayBuiltInsModule
)(implicit ec: ExecutionContext, override val mat: Materializer)
    extends Filter {

  protected lazy val encrypter: Encrypter = sessionCookieCrypto.crypto
  protected lazy val decrypter: Decrypter = sessionCookieCrypto.crypto

  private val AnonymousAuth = "anonymous"
  private val EmailAuth = "email"
  private val HmrcAuth = "hmrc"

  private val cookieHeaderEncoding = new DefaultCookieHeaderEncoding(configModule.httpConfiguration.cookies)

  override def apply(next: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] = {

    val maybeFormTemplateContext: Future[Option[FormTemplateContext]] =
      requestHeaderService.formTemplateContext(rh)

    def findAuthConfigCookie(rh: RequestHeader): Option[Cookie] =
      rh.headers
        .getAll(HeaderNames.COOKIE)
        .flatMap(cookieHeaderEncoding.decodeCookieHeader)
        .find(_.name == authConfigCookieName)

    maybeFormTemplateContext.flatMap {
      case Some(FormTemplateContext(formTemplate, _, _, Some(shutter), _)) =>
        val langs = playBuiltInsModule.langs
        implicit val messagesApi = playBuiltInsModule.messagesApi
        implicit val request = Request(rh, AnyContentAsEmpty)
        implicit val currentLanguge = LangADT.fromRequest(request, langs)(messagesApi)
        implicit val messages = messagesApi.preferred(rh.acceptLanguages)
        Future.successful(
          Results.Forbidden(
            html.form.shutterForm(
              formTemplate,
              configModule.frontendAppConfig,
              shutter.toHtmlMessage
            )
          )
        )

      case Some(formTemplateContext) =>
        val formTemplate =
          formTemplateContext.formTemplate
        val (result, cookieValue) =
          formTemplate.authConfig match {
            case Anonymous =>
              (
                anonymousCookieCryptoFilter(next)(rh.addAttr(FormTemplateKey, formTemplateContext)),
                encrypter.encrypt(PlainText(AnonymousAuth))
              )
            case EmailAuthConfig(_, _, _, _) =>
              (
                emailCookieCryptoFilter(next)(rh.addAttr(FormTemplateKey, formTemplateContext)),
                encrypter.encrypt(PlainText(EmailAuth))
              )
            case _ =>
              (
                hmrcCookieCryptoFilter(next)(rh.addAttr(FormTemplateKey, formTemplateContext)),
                encrypter.encrypt(PlainText(HmrcAuth))
              )
          }
        result.map(_.withCookies(Cookie(authConfigCookieName, cookieValue.value, secure = true)))

      case None =>
        findAuthConfigCookie(rh).map(v => decrypter.decrypt(Crypted(v.value)).value) match {
          case Some(AnonymousAuth) =>
            anonymousCookieCryptoFilter(next)(rh)
          case Some(EmailAuth) =>
            emailCookieCryptoFilter(next)(rh)
          case _ =>
            hmrcCookieCryptoFilter(next)(rh)
        }
    }
  }
}
