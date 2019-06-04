/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.mvc.{ Action, AnyContent }
import scala.collection.JavaConverters._
import scala.concurrent.Future
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.lookup.{ AjaxLookup, LookupLabel, LookupRegistry, ShowAll }
import uk.gov.hmrc.gform.models.LookupQuery
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, Register }
import uk.gov.hmrc.play.frontend.controller.FrontendController

class LookupController(
  auth: AuthenticatedRequestActions,
  lookupRegistry: LookupRegistry
) extends FrontendController {

  def lookupAll(formTemplateId: FormTemplateId, register: Register): Action[AnyContent] =
    lookup(formTemplateId, register, LookupQuery.Empty)

  def lookup(formTemplateId: FormTemplateId, register: Register, lookupQuery: LookupQuery): Action[AnyContent] =
    auth.async(formTemplateId, None) { implicit request => l => cache =>
      val filtered: List[LookupLabel] = (lookupRegistry.get(register), lookupQuery) match {
        case (Some(AjaxLookup(options, _, ShowAll.Enabled)), LookupQuery.Empty)  => options.keys.toList
        case (Some(AjaxLookup(options, _, ShowAll.Disabled)), LookupQuery.Empty) => List.empty
        case (Some(AjaxLookup(_, autocomplete, _)), LookupQuery.Value(query)) =>
          autocomplete.search(query).asScala.toList.map(lr => LookupLabel(lr.value))
        case _ => List.empty
      }
      val results: List[String] = filtered.map(_.label).sorted
      Ok(Json.toJson(results)).pure[Future]
    }
}
