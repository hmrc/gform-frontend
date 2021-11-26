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
import cats.data.NonEmptyList
import play.api.mvc.{ Filter, RequestHeader, Result }
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.auth.models.{ CompositeAuthDetails, EmailRetrievals }
import uk.gov.hmrc.gform.gform.EmailAuthUtils.isEmailConfirmed
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.EmailId
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form.{ FormIdData, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateWithRedirects
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendHeaderCarrierProvider
import cats.syntax.eq._
import org.slf4j.{ Logger, LoggerFactory }
import play.api.mvc.request.{ Cell, RequestAttrKey }
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.{ AuthConnector, AuthProvider, AuthProviders, AuthorisedFunctions }
import uk.gov.hmrc.gform.controllers.GformRequestAttrKeys.{ compositeAuthSessionClearAttrKey, compositeAuthSessionClearAttrKeyName, emailSessionClearAttrKey, emailSessionClearAttrKeyName }
import uk.gov.hmrc.gform.gform.EmailAuthUtils
import uk.gov.hmrc.gform.controllers.GformSessionKeys.COMPOSITE_AUTH_DETAILS_SESSION_KEY
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthConfig, Composite, EmailAuthConfig, FormTemplate }

import scala.concurrent.{ ExecutionContext, Future }

/** This filter only applies to authConfig=email and triggered on the new form route only (/xxxx/new-form/formTemplateId)
  * It creates a new session when accessing a new instance of the form, following a successful form submission
  * in the same session.
  *
  * @param gformConnector
  * @param ec
  * @param mat
  */
class EmailAuthSessionPurgeFilter(
  gformConnector: GformConnector,
  val authConnector: AuthConnector
)(implicit
  ec: ExecutionContext,
  override val mat: Materializer
) extends Filter with FrontendHeaderCarrierProvider with AuthorisedFunctions {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private val NEW_FORM_PATTERN = """^(.+)/new-form/([^/]+)$""".r

  def apply(next: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] = {
    implicit val requestHeader: RequestHeader = rh
    if (isNewFormRoute) {
      val formTemplateWithRedirects = rh.attrs(FormTemplateKey)
      val formTemplate = formTemplateWithRedirects.formTemplate
      formTemplate.authConfig match {
        case _: EmailAuthConfig => handleEmail(next, formTemplateWithRedirects)
        case Composite(configs) => handleCompositeAuth(next, formTemplateWithRedirects, configs)
        case _                  => next(rh)
      }
    } else {
      next(rh)
    }
  }

  def getGovernmentGatewayGroupIdentifier(implicit request: RequestHeader): Future[Option[String]] =
    authorised(AuthProviders(AuthProvider.GovernmentGateway))
      .retrieve(Retrievals.groupIdentifier) {
        case Some(maybeGroupIdentifier) =>
          Future.successful(Some(maybeGroupIdentifier))
        case _ =>
          Future.successful(None)
      }
      .recover { case _ =>
        None
      }

  def handleCompositeAuth(
    next: RequestHeader => Future[Result],
    formTemplateWithRedirects: FormTemplateWithRedirects,
    configs: NonEmptyList[AuthConfig]
  )(implicit rh: RequestHeader): Future[Result] = {

    val formTemplate = formTemplateWithRedirects.formTemplate
    val currentAuthProvider = jsonFromSession(rh, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)
      .get(formTemplateWithRedirects)

    currentAuthProvider match {
      case Some("email") => handleEmail(next, formTemplateWithRedirects)
      case Some(_) =>
        getGovernmentGatewayGroupIdentifier(rh).flatMap {
          case Some(groupIdentifier) =>
            isGgFormSubmitted(formTemplate, UserId(groupIdentifier)).flatMap { isSubmitted =>
              if (isSubmitted && !rh.queryString.contains(compositeAuthSessionClearAttrKeyName)) {
                logger.info(
                  s"Form status is SUBMITTED. Removing composite auth data for template ${formTemplate._id} from session, to restart auth"
                )
                next(
                  rh.addAttr(
                    RequestAttrKey.Session,
                    Cell(
                      rh.session
                        .-(COMPOSITE_AUTH_DETAILS_SESSION_KEY)
                    )
                  ).addAttr(compositeAuthSessionClearAttrKey, "true")
                )
              } else {
                next(rh)
              }
            }
          case None =>
            next(rh.addAttr(compositeAuthSessionClearAttrKey, "true"))
        }
      case None =>
        next(rh.addAttr(compositeAuthSessionClearAttrKey, "true"))
    }
  }

  def isGgFormSubmitted(formTemplate: FormTemplate, userId: UserId)(implicit rh: RequestHeader): Future[Boolean] = {
    val formIdData = FormIdData.Plain(userId, formTemplate._id)
    gformConnector.maybeForm(formIdData, formTemplate).map { form =>
      form.exists(_.status === Submitted)
    }
  }

  def handleEmail(
    next: RequestHeader => Future[Result],
    formTemplateWithRedirects: FormTemplateWithRedirects
  )(implicit rh: RequestHeader): Future[Result] =
    isEmailConfirmed(formTemplateWithRedirects) match {
      case Some(email) =>
        val formTemplate = formTemplateWithRedirects.formTemplate
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
                            Cell(
                              rh.session
                                .+(EmailAuthUtils.removeFormTemplateFromAuthSession(formTemplate._id))
                                .-(COMPOSITE_AUTH_DETAILS_SESSION_KEY)
                            )
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

  private def isNewFormRoute(implicit rh: RequestHeader) =
    rh.method == "GET" && NEW_FORM_PATTERN.pattern.matcher(rh.path).matches()
}
