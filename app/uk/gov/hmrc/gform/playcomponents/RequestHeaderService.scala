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
import play.api.cache.caffeine.CaffeineCacheApi
import play.api.mvc.RequestHeader
import uk.gov.hmrc.gform.gform.FormTemplateCacheConfig

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateBehavior, FormTemplateContext, FormTemplateId }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter

final class RequestHeaderService(
  gformConnector: GformConnector,
  formTemplateContextCache: CaffeineCacheApi,
  formTemplateCacheConfig: FormTemplateCacheConfig
)(implicit ec: ExecutionContext) {

  def formTemplateContext(rh: RequestHeader): Future[Option[FormTemplateContext]] =
    RequestFormTemplateId.formTemplateId(rh) match {
      case Some(FormTemplateId(formTemplateId)) =>
        implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequest(rh)
        for {
          formTemplateContext <-
            if (formTemplateCacheConfig.enabled) {
              formTemplateContextCache
                .getOrElseUpdate[FormTemplateContext]("formTemplateContext", formTemplateCacheConfig.expiry) {
                  gformConnector.getFormTemplateContext(FormTemplateId(formTemplateId.toLowerCase))
                }
                .map(Some(_))
            } else {
              gformConnector.getFormTemplateContext(FormTemplateId(formTemplateId.toLowerCase)).map(Some(_))
            }
          formTemplateBehavior <- if (rh.method === "GET")
                                    gformConnector.getFormTemplateBehavior(FormTemplateId(formTemplateId))
                                  else Future.successful(FormTemplateBehavior.empty)
        } yield formTemplateContext.map(
          _.copy(shutter = formTemplateBehavior.shutter, notificationBanner = formTemplateBehavior.notificationBanner)
        )
      case None => Future.successful(None)
    }
}
