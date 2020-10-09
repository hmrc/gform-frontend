/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.gform

import cats.instances.future._
import cats.syntax.applicative._
import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents }
import uk.gov.hmrc.gform.auth.models.OperationWithoutForm

import scala.collection.JavaConverters._
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.models.LookupQuery
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, Register }
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

class LookupController(
  auth: AuthenticatedRequestActionsAlgebra[Future],
  lookupRegistry: LookupRegistry,
  messagesControllerComponents: MessagesControllerComponents)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  def lookupAll(formTemplateId: FormTemplateId, register: Register): Action[AnyContent] =
    lookup(formTemplateId, register, LookupQuery.Empty)

  def lookup(formTemplateId: FormTemplateId, register: Register, lookupQuery: LookupQuery): Action[AnyContent] =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.Lookup) { request => implicit l => cache =>
      val filtered: List[LookupLabel] = (lookupRegistry.get(register), lookupQuery) match {
        case (Some(AjaxLookup(options, _, ShowAll.Enabled)), LookupQuery.Empty) =>
          options.process(_.sortLookupByPriorityAndLabel)
        case (Some(AjaxLookup(_, _, ShowAll.Disabled)), LookupQuery.Empty) => List.empty
        case (Some(AjaxLookup(options, autocomplete, showAll)), LookupQuery.Value(query)) =>
          val labels: List[LookupLabel] =
            autocomplete
              .get(l)
              .fold(List.empty[LookupLabel])(
                _.search(query).asScala.toList.sortBy(r => (r.priority, r.value)).map(_.toLookupLabel))

          showAll match {
            case ShowAll.Enabled  => options.process(_.sortLookupByPriorityAndLabel.filter(labels.contains))
            case ShowAll.Disabled => labels
          }
        case _ => List.empty
      }
      val results: List[String] = filtered.map(_.label)
      Ok(Json.toJson(results)).pure[Future]
    }
}
