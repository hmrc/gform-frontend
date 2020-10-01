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

package uk.gov.hmrc.gform.models.optics

import uk.gov.hmrc.gform.eval.{ EvaluationResults, ExpressionResult }
import uk.gov.hmrc.gform.graph.{ GraphData, RecData }
import uk.gov.hmrc.gform.models.{ FormModel, PageModel, Visibility }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, SourceOrigin, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Expr
import uk.gov.hmrc.gform.sharedmodel.graph.GraphNode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, IsMultiField, SectionNumber }

case class FormModelVisibilityOptics[D <: DataOrigin](
  formModel: FormModel[Visibility],
  recData: RecData[SourceOrigin.Current],
  evaluationResults: EvaluationResults,
  graphData: GraphData,
  booleanExprCache: BooleanExprCache
) {

  def allFormComponents: List[FormComponent] = formModel.allFormComponents

  def allFormComponentIds: List[FormComponentId] =
    allFormComponents.map(_.id)

  def collect[B](pf: PartialFunction[(ModelComponentId, VariadicValue), B]): Iterable[B] =
    recData.variadicFormData.collect(pf)

  def fcLookup: Map[FormComponentId, FormComponent] = formModel.fcLookup

  def evalO(expr: Expr): Option[ExpressionResult] = {
    val typedExpr = formModel.toTypedExpr(expr)
    evaluationResults.get(typedExpr)
  }

  def eval(expr: Expr): String = evalO(expr).fold("")(_.stringRepresentation)

  object data {
    def all: List[(ModelComponentId, VariadicValue)] =
      allFormComponents.flatMap(_.multiValueId.toModelComponentIds).flatMap { modelComponentId =>
        get(modelComponentId).map(modelComponentId -> _)
      }
    def one(modelComponentId: ModelComponentId): Option[String] =
      if (formModel.isDefinedAt(modelComponentId)) {
        recData.variadicFormData.one(modelComponentId)
      } else None

    def many(modelComponentId: ModelComponentId): Option[Seq[String]] =
      if (formModel.isDefinedAt(modelComponentId)) {
        recData.variadicFormData.many(modelComponentId)
      } else None

    def get(modelComponentId: ModelComponentId): Option[VariadicValue] =
      if (formModel.isDefinedAt(modelComponentId)) {
        recData.variadicFormData.get(modelComponentId)
      } else None
  }

}
