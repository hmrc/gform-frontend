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

import cats.implicits.catsSyntaxEq
import play.api.mvc.RequestHeader
import uk.gov.hmrc.gform.cache.FormTemplateCacheService

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateBehavior, FormTemplateCache, FormTemplateContext, FormTemplateContextCacheManager, FormTemplateId }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import java.time.Instant

final class RequestHeaderService(
  gformConnector: GformConnector,
  formTemplateContextCacheManager: FormTemplateContextCacheManager,
  formTemplateCacheService: FormTemplateCacheService
)(implicit ec: ExecutionContext) {

  def formTemplateContext(rh: RequestHeader): Future[Option[FormTemplateContext]] =
    RequestFormTemplateId.formTemplateId(rh) match {
      case Some(FormTemplateId(formTemplateId)) =>
        implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequest(rh)
        val lowerCaseFormTemplateId = FormTemplateId(formTemplateId.toLowerCase)
        for {
          updatedAt <-
            formTemplateCacheService.find(lowerCaseFormTemplateId).map(_.map(_.updatedAt).getOrElse(Instant.now))
          formTemplateContext <- getFormTemplateContextIfUpdated(lowerCaseFormTemplateId, updatedAt)
          formTemplateBehavior <- if (rh.method === "GET")
                                    gformConnector.getFormTemplateBehavior(lowerCaseFormTemplateId)
                                  else Future.successful(FormTemplateBehavior.empty)
        } yield formTemplateContext.map(
          _.copy(shutter = formTemplateBehavior.shutter, notificationBanner = formTemplateBehavior.notificationBanner)
        )
      case None => Future.successful(None)
    }

  private def getFormTemplateContextIfUpdated(formTemplateId: FormTemplateId, updatedAt: Instant)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Option[FormTemplateContext]] = {
    val cachedContext = formTemplateContextCacheManager.getFormTemplateContext(formTemplateId, updatedAt)
    cachedContext match {
      case Some(cachedContext) => Future.successful(Some(cachedContext))
      case _ =>
        for {
          formTemplateContext <- gformConnector.getFormTemplateContext(formTemplateId)
          _ <-
            formTemplateCacheService
              .save(FormTemplateCache(formTemplateId, updatedAt))
              .fold(
                invalid =>
                  new RuntimeException(
                    s"Saving form template cache is getting error, form template id: ${formTemplateId.value} error: ${invalid.error}"
                  ),
                _ => formTemplateContextCacheManager.putFormTemplateContext(formTemplateContext, updatedAt)
              )

        } yield Some(formTemplateContext)
    }
  }
}
