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

import akka.stream.Materializer
import play.api.Logger
import play.api.http.HeaderNames
import play.api.libs.crypto.CookieSigner
import play.api.mvc.request.{ Cell, RequestAttrKey }
import play.api.mvc._
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.gform.controllers.CookieNames
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Anonymous, AuthConfig, FormTemplate, FormTemplateId }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.controllers.CookieNames._
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.{ SessionCookieCrypto, SessionCookieCryptoFilter }

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

class GformSessionCookieCryptoFilter(
  sessionCookieCrypto: SessionCookieCrypto,
  gformConnector: GformConnector,
  cookieSigner: CookieSigner,
  val sessionBaker: SessionCookieBaker,
  sessionBakerAnonymous: SessionCookieBaker
)(
  implicit
  override val mat: Materializer,
  override val ec: ExecutionContext)
    extends SessionCookieCryptoFilter {

  implicit val hc = HeaderCarrier()

  override protected lazy val encrypter: Encrypter = sessionCookieCrypto.crypto
  override protected lazy val decrypter: Decrypter = sessionCookieCrypto.crypto
  override protected val decodeCookieHeader: String => Seq[Cookie] = Cookies.decodeCookieHeader

  private val logger = Logger(getClass)

  override def apply(next: (RequestHeader) => Future[Result])(rh: RequestHeader): Future[Result] =
    decryptSession(rh).flatMap(dsResult => encryptSession(next(dsResult.rh), dsResult))

  private def encryptSession(f: Future[Result], dsResult: DecryptSessionResult): Future[Result] =
    f.map { result =>
      val newSessionAsCookie: Option[Cookie] =
        result.newSession.map { session =>
          val sessionCookie: Cookie = dsResult.scBaker.encodeAsCookie(session)
          sessionCookie.copy(value = encrypter.encrypt(sessionCookie.value))
        }

      val resultWithSessionReplacedByCookie: Option[Result] = {
        newSessionAsCookie
          .map { c =>
            result
              .copy(newSession = None)
              .withCookies(c)
          }
      }

      val cResult = resultWithSessionReplacedByCookie.getOrElse(result)

      dsResult.fId.fold(cResult) { fid =>
        cResult.withCookies(Cookie(formTemplateIdCookieName, fid.value))
      }
    }

  private def decryptSession(rh: RequestHeader): Future[DecryptSessionResult] = {
    println(s"===== Sandy : decryptSession : rh.attrs : ${rh.addAttr()}")
    for {
      formTemplate <- getFormTemplate(rh.path.split("/").apply(3)) flatMap {
                       case ft @ Some(_) => Future.successful(ft)
                       case None         => getFormTemplate(rh.cookies.apply(CookieNames.formTemplateIdCookieName).value)
                     }

      scBaker = formTemplate.map(_.authConfig) match {
        case Some(Anonymous) =>
          sessionBakerAnonymous
        case _ =>
          sessionBaker
      }

      encryptedSessionCookie <- Future.successful(formTemplate.flatMap(ft => findSessionCookie(rh, ft.authConfig)))

      decryptedSessionCookie <- Future.successful(encryptedSessionCookie.flatMap(ec => decrypt(ec, scBaker)))
    } yield {
      val r = rh.addAttr(
        key = RequestAttrKey.Session,
        value = Cell(scBaker.decodeFromCookie(decryptedSessionCookie))
      )

      DecryptSessionResult(r, scBaker, formTemplate.map(_._id))
    }
  }

  private def findSessionCookie(rh: RequestHeader, authConfig: AuthConfig): Option[Cookie] = {
    val sessionCookieName = authConfig match {
      case Anonymous =>
        anonymousFormSessionCookieName
      case _ =>
        mdtpFormSessionCookieName
    }

    rh.headers
      .getAll(HeaderNames.COOKIE)
      .flatMap(decodeCookieHeader)
      .find(_.name == sessionCookieName)
  }

  private def decrypt(cookie: Cookie, scBaker: SessionCookieBaker): Option[Cookie] =
    Try(decrypter.decrypt(cookie.value)) match {
      case Success(decryptedValue) => Some(cookie.copy(value = decryptedValue))
      case Failure(ex) =>
        logger.warn(s"Could not decrypt cookie ${scBaker.COOKIE_NAME} got exception:${ex.getMessage}")
        None
    }

  private def getFormTemplate(fid: String): Future[Option[FormTemplate]] =
    gformConnector.getFormTemplate(FormTemplateId(fid))(hc, ec).map(Some(_)).recover {
      case _ => None
    }
}

case class DecryptSessionResult(rh: RequestHeader, scBaker: SessionCookieBaker, fId: Option[FormTemplateId])
