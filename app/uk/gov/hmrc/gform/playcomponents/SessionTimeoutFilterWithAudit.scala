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

import akka.stream.Materializer
import uk.gov.hmrc.play.bootstrap.frontend.filters.{ SessionTimeoutFilter, SessionTimeoutFilterConfig }

import uk.gov.hmrc.gform.FormTemplateKey
import play.api.mvc._
import scala.concurrent.{ ExecutionContext, Future }
import java.time.Instant
import play.api.mvc.Results.Ok
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.http.HeaderCarrier

class SessionTimeoutFilterWithAudit(
  config: SessionTimeoutFilterConfig,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  auditService: AuditService
)(implicit
  ec: ExecutionContext,
  override val mat: Materializer
) extends SessionTimeoutFilter(config) {

  private def timestampToInstant(timestampMs: String): Option[Instant] =
    try Some(Instant.ofEpochMilli(timestampMs.toLong))
    catch {
      case e: NumberFormatException => None
    }

  private def hasExpired(timestamp: Instant): Boolean = {
    val timeOfExpiry = timestamp.plus(config.timeoutDuration)
    clock().isAfter(timeOfExpiry)
  }

  override def apply(f: (RequestHeader) => Future[Result])(rh: RequestHeader): Future[Result] = {
    val lastRequestTimestamp = "ts"
    val timestamp = rh.session.get(lastRequestTimestamp)

    val formTemplateWithRedirects = rh.attrs.get(FormTemplateKey)
    val maybeFormTemplate = formTemplateWithRedirects.map(_.formTemplate)
    (timestamp.flatMap(timestampToInstant), maybeFormTemplate) match {
      case (Some(ts), Some(formTemplate)) if hasExpired(ts) =>
        sendTimeOutEvent(formTemplate._id)(rh.withBody(null))
          .flatMap(_ => super.apply(f)(rh))
      case _ => super.apply(f)(rh)
    }
  }

  private def sendTimeOutEvent(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.AuditSessionEnd) {
      _ => _ => cache => _ => formModelOptics =>
        implicit val hc: HeaderCarrier = HeaderCarrier()
        auditService.sendFormTimoutEvent(cache.form, cache.retrievals)
        Future.successful(Ok("success"))
    }
}
