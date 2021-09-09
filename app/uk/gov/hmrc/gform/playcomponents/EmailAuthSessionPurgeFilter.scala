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
import play.api.mvc.{ Filter, RequestHeader, Result }
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.auth.models.EmailRetrievals
import uk.gov.hmrc.gform.gform.EmailAuthUtils.isEmailConfirmed
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.EmailId
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form.{ FormIdData, Submitted }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendHeaderCarrierProvider
import cats.syntax.eq._
import org.slf4j.{ Logger, LoggerFactory }
import play.api.mvc.request.{ Cell, RequestAttrKey }
import uk.gov.hmrc.gform.controllers.GformRequestAttrKeys.{ emailSessionClearAttrKey, emailSessionClearAttrKeyName }
import uk.gov.hmrc.gform.gform.EmailAuthUtils

import scala.concurrent.{ ExecutionContext, Future }

/** This filter only applies to authConfig=email and triggered on the new form route only (/xxxx/new-form/formTemplateId)
  * It creates a new session when accessing a new instance of the form, following a successful form submission
  * in the same session.
  *
  * @param gformConnector
  * @param ec
  * @param mat
  */
class EmailAuthSessionPurgeFilter(gformConnector: GformConnector)(implicit
  ec: ExecutionContext,
  override val mat: Materializer
) extends Filter with FrontendHeaderCarrierProvider {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private val NEW_FORM_PATTERN = """^(.+)/new-form/([^/]+)$""".r

  override def apply(next: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] = {
    implicit val requestHeader: RequestHeader = rh
    if (isNewFormRoute) {
      val formTemplate = rh.attrs(FormTemplateKey)
      if (formTemplate.authConfig.isEmailAuthConfig) {
        isEmailConfirmed(formTemplate._id) match {
          case Some(email) =>
            logger.info(
              s"Accessing new form and email confirmed in session. Checking for form status for template ${formTemplate._id}"
            )
            val formIdData = FormIdData.Plain(UserId(EmailRetrievals(EmailId(email))), formTemplate._id)
            for {
              maybeForm <- gformConnector.maybeForm(formIdData, formTemplate)
              result <- maybeForm.fold(next(rh)) { form =>
                          if (form.status === Submitted && !rh.queryString.contains(emailSessionClearAttrKeyName)) {
                            logger.info(
                              s"Form status is SUBMITTED. Removing email auth data for template ${formTemplate._id} from session, to restart auth"
                            )
                            next(
                              rh.addAttr(
                                RequestAttrKey.Session,
                                Cell(rh.session.+(EmailAuthUtils.removeFormTemplateFromAuthSession(formTemplate._id)))
                              ).addAttr(emailSessionClearAttrKey, "true")
                            )
                          } else {
                            next(rh)
                          }
                        }
            } yield result
          case None =>
            next(rh.addAttr(emailSessionClearAttrKey, "true"))
        }
      } else {
        next(rh)
      }
    } else {
      next(rh)
    }
  }

  private def isNewFormRoute(implicit rh: RequestHeader) =
    rh.method == "GET" && NEW_FORM_PATTERN.pattern.matcher(rh.path).matches()
}
