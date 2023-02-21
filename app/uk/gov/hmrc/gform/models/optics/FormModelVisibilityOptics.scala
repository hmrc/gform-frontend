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

package uk.gov.hmrc.gform.models.optics

import uk.gov.hmrc.gform.eval.{ BooleanExprResolver, EvaluationResults, ExpressionResultWithTypeInfo, TypeInfo }
import uk.gov.hmrc.gform.graph.{ GraphData, RecData, RecalculationResult }
import uk.gov.hmrc.gform.models.{ Coordinates, FormModel, FormModelBuilder, Visibility }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormPhase, IncludeIf, SectionNumber }
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, SourceOrigin, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId }
import uk.gov.hmrc.gform.sharedmodel.DataRetrieveResult
import com.softwaremill.quicklens._

case class FormModelVisibilityOptics[D <: DataOrigin](
  formModel: FormModel[Visibility],
  recData: RecData[SourceOrigin.Current],
  recalculationResult: RecalculationResult
) {

  val evaluationResults: EvaluationResults = recalculationResult.evaluationResults
  val graphData: GraphData = recalculationResult.graphData
  val booleanExprCache: BooleanExprCache = recalculationResult.booleanExprCache

  def allFormComponents: List[FormComponent] = formModel.allFormComponents

  def allEditableFormComponentsForCoordinates(coordinates: Coordinates): List[FormComponent] =
    allFormComponentsForCoordinates(coordinates).filter(_.editable)

  def allFormComponentsForCoordinates(coordinates: Coordinates): List[FormComponent] =
    formModel.taskList.allFormComponents(coordinates)

  def allFormComponentIds: List[FormComponentId] =
    allFormComponents.map(_.id)

  def collect[B](pf: PartialFunction[(ModelComponentId, VariadicValue), B]): Iterable[B] =
    recData.variadicFormData.collect(pf)

  def fcLookup: Map[FormComponentId, FormComponent] = formModel.fcLookup

  def evalAndApplyTypeInfoExplicit(expr: Expr, fcId: FormComponentId): ExpressionResultWithTypeInfo = {
    val typeInfo = formModel.explicitTypedExpr(expr, fcId)
    evalAndApplyTypeInfo(typeInfo)
  }

  def evalAndApplyTypeInfoFirst(expr: Expr): ExpressionResultWithTypeInfo = {
    val typeInfo = formModel.toFirstOperandTypeInfo(expr)
    evalAndApplyTypeInfo(typeInfo)
  }

  val booleanExprResolver = BooleanExprResolver(booleanExpr => evalIncludeIfExpr(IncludeIf(booleanExpr), None))

  def evalAndApplyTypeInfo(typeInfo: TypeInfo): ExpressionResultWithTypeInfo =
    ExpressionResultWithTypeInfo(
      recalculationResult.evaluationResults
        .evalExprCurrent(typeInfo, recData, booleanExprResolver, recalculationResult.evaluationContext)
        .applyTypeInfo(typeInfo),
      typeInfo
    )

  def evalIncludeIfExpr(includeIf: IncludeIf, phase: Option[FormPhase]): Boolean =
    FormModelBuilder.evalIncludeIf(includeIf, recalculationResult, recData, formModel, phase)

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

    def forSectionNumber[A](sectionNumber: SectionNumber): Set[VariadicValue] =
      formModel(sectionNumber).allModelComponentIds.flatMap(recData.variadicFormData.get)

    def forCoordinate[A](coordinates: Coordinates): Set[VariadicValue] = {
      val modelComponentIds: List[ModelComponentId] =
        allEditableFormComponentsForCoordinates(coordinates).map(_.multiValueId).flatMap(_.toModelComponentIds)

      modelComponentIds.toSet.flatMap(recData.variadicFormData.get)

    }
  }

  def addDataRetreiveResults(dataRetrieveResults: List[DataRetrieveResult]): FormModelVisibilityOptics[D] =
    this
      .modify(
        _.recalculationResult.evaluationContext.thirdPartyData
      )
      .using(_.updateDataRetrieve(dataRetrieveResults))

}
