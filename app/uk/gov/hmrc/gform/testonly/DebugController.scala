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

package uk.gov.hmrc.gform.testonly

import play.api.libs.json.{ JsObject, Json }
import play.api.mvc.MessagesControllerComponents

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.objectStore.ObjectStoreService
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class DebugController(
  auth: AuthenticatedRequestActions,
  objectStoreService: ObjectStoreService,
  messagesControllerComponents: MessagesControllerComponents
)(implicit
  ec: ExecutionContext
) extends FrontendController(messagesControllerComponents) {

  def model(formTemplateId: FormTemplateId) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, None) {
      implicit request => lang => cache => sse => formModelOptics =>
        val totalColumns = formModelOptics.formModelRenderPageOptics.formModel.pages.size

        for {
          envelope <- objectStoreService.getEnvelope(cache.form.envelopeId)
        } yield {
          val page = html.debug.model(formModelOptics, totalColumns, envelope)
          Ok(page)
        }

    }
  def exprs(formTemplateId: FormTemplateId) =
    auth.async[SectionSelectorType.WithAcknowledgement](formTemplateId, None) {
      request => lang => cache => sse => formModelOptics =>
        val graphTopologicalOrder: List[JsObject] = List.empty[JsObject]
        // formModelOptics.formModelVisibilityOptics.graphData.graphTopologicalOrder.toList.map {
        //   case (layerNumber, nodes) =>
        //     Json.obj(
        //       "layerNumber" -> layerNumber.toString,
        //       "nodes"       -> nodes.toString
        //     )
        // }

        val exprs: List[JsObject] = List.empty[JsObject]
        // formModelOptics.formModelVisibilityOptics.evaluationResults.exprMap.toList.sortBy(_._1.toString).map {
        //   case (typedExpr, expressionResult) =>
        //     Json.obj(
        //       "typedExpr"        -> typedExpr.toString,
        //       "expressionResult" -> expressionResult.toString
        //     )
        // }

        val result = Json.obj(
          "expression" -> Json.toJson(exprs),
          "graph"      -> Json.toJson(graphTopologicalOrder)
        )

        Future.successful(Ok(result))
    }
}
