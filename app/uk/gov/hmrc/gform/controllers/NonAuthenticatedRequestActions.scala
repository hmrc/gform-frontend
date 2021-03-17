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

package uk.gov.hmrc.gform
package controllers

import play.api.i18n.{ Langs, MessagesApi }
import play.api.mvc.{ Action, ActionBuilder, AnyContent, Request, Result }
import scala.concurrent.Future
import scala.language.higherKinds
import uk.gov.hmrc.gform.sharedmodel.LangADT

trait NonAuthenticatedRequestActionsAlgebra[F[_]] {
  def async(f: Request[AnyContent] => LangADT => F[Result]): Action[AnyContent]

  def apply(f: Request[AnyContent] => LangADT => Result): Action[AnyContent]
}

class NonAuthenticatedRequestActions(
  langs: Langs,
  actionBuilder: ActionBuilder[Request, AnyContent]
)(implicit
  messagesApi: MessagesApi
) extends NonAuthenticatedRequestActionsAlgebra[Future] {

  override def async(f: Request[AnyContent] => LangADT => Future[Result]): Action[AnyContent] =
    actionBuilder.async { implicit request =>
      val langADT: LangADT = LangADT.fromRequest(request, langs)
      f(request)(langADT)
    }

  override def apply(f: Request[AnyContent] => LangADT => Result): Action[AnyContent] =
    async(r => l => Future.successful(f(r)(l)))

}
