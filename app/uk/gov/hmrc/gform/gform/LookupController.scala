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

package uk.gov.hmrc.gform.gform

import cats.syntax.all._
import play.api.i18n.I18nSupport
import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.models.{ LookupQuery, SectionSelectorType }
import uk.gov.hmrc.gform.search.Search
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.lookup.LookupOptions._
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class LookupController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  lookupRegistry: LookupRegistry,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  def lookupWithSelectionCriteria(
    formTemplateId: FormTemplateId,
    formComponentId: FormComponentId,
    register: Register,
    maybeAccessCode: Option[AccessCode],
    lookupQuery: LookupQuery
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => sse => formModelOptics =>
        import i18nSupport._
        val aFormComponents: Seq[FormComponent] = formModelOptics.formModelRenderPageOptics.formModel.allFormComponents
        val withoutCountryAtomFormComponentId = formComponentId.modelComponentId.fold(_.toFormComponentId) {
          case ModelComponentId.Atomic(i, Address.country) => ModelComponentId.pure(i).toFormComponentId
          case otherwise                                   => otherwise.toFormComponentId
        }
        val oFormComponent = aFormComponents.find(_.id.value === withoutCountryAtomFormComponentId.value)

        val sSelectionCriteria: Option[List[SimplifiedSelectionCriteria]] = oFormComponent flatMap {
          case IsText(Text(Lookup(_, sc), _, _, _, _, _, _))         => sc
          case IsOverseasAddress(OverseasAddress(_, _, _, _, _, sc)) => sc
          case _                                                     => None
        } map {
          SimplifiedSelectionCriteria
            .convertToSimplifiedSelectionCriteria(_, lookupRegistry, formModelOptics.formModelVisibilityOptics)
        }

        val priority: Option[Priority] = oFormComponent flatMap {
          case IsText(Text(_, _, _, _, _, _, priority)) => priority
          case _                                        => None
        }

        val results = (lookupRegistry.get(register), lookupQuery, sSelectionCriteria) match {
          case (Some(AjaxLookup(options, _, ShowAll.Enabled)), LookupQuery.Empty, Some(sc)) =>
            options.m
              .get(l)
              .map(r => LookupOptions(filterBySelectionCriteria(sc, r.options)))
              .map(s => LocalisedLookupOptions(Map(l -> s)))
              .map(_.process(_.sortLookupByPriorityAndLabel(priority).map(_.label)))
              .getOrElse(List.empty)

          case (Some(AjaxLookup(options, _, ShowAll.Enabled)), LookupQuery.Empty, None) =>
            options.process(_.sortLookupByPriorityAndLabel(priority)).map(_.label)

          case (Some(AjaxLookup(_, _, ShowAll.Disabled)), LookupQuery.Empty, _) =>
            List.empty

          case (Some(AjaxLookup(options, searcher, _)), LookupQuery.Value(query), Some(sc)) =>
            val labels: List[LookupLabel] = Search.search(searcher, query.toLowerCase)

            options.m
              .get(l)
              .map(r => LookupOptions(filterBySelectionCriteria(sc, r.options)))
              .map(s => LocalisedLookupOptions(Map(l -> s)))
              .map(_.process(_.sortLookupByPriorityAndLabel(priority)))
              .getOrElse(List.empty)
              .filter(labels.contains)
              .map(_.label)

          case (Some(AjaxLookup(options, searcher, showAll)), LookupQuery.Value(query), None) =>
            val labels: List[LookupLabel] = Search.search(searcher, query.toLowerCase)

            showAll match {
              case ShowAll.Enabled =>
                options.process(_.sortLookupByPriorityAndLabel(priority).filter(labels.contains)).map(_.label)
              case ShowAll.Disabled => labels.map(_.label)
            }

          case _ =>
            List.empty
        }

        Ok(Json.toJson(results)).pure[Future]
    }
}
