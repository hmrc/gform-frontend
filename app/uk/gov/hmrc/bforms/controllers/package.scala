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


import play.api.mvc.{ Action, ActionBuilder, ActionRefiner, Request, Result, Results, WrappedRequest }
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.bforms.models.FormTemplate
import uk.gov.hmrc.bforms.service.RetrieveService
import uk.gov.hmrc.play.http.HeaderCarrier

package object controllers {

  def ActionWithTemplate(
    formTypeId: String,
    version: String
  )(
    implicit ec: ExecutionContext
  ): ActionBuilder[RequestWithTemplate] =
    Action.andThen(WithFormTemplate(formTypeId, version))
}


package controllers {

  case class RequestWithTemplate[B](request: Request[B], formTemplate: FormTemplate) extends WrappedRequest(request)

  class WithFormTemplate private (formTypeId: String, version: String)(implicit ec: ExecutionContext) extends ActionRefiner[Request, RequestWithTemplate] {
    protected def refine[A](request: Request[A]): Future[Either[Result, RequestWithTemplate[A]]] = {

      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, Some(request.session))

      RetrieveService.getFormTemplate(formTypeId, version).map {
        case Right(formTemplate)=> Right(RequestWithTemplate(request, formTemplate))
        case Left(error) => Left(Results.BadRequest(error))
      }
    }
  }

  object WithFormTemplate {
    def apply(formTypeId: String, version: String)(implicit ec: ExecutionContext) = new WithFormTemplate(formTypeId, version)
  }
}
