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

import uk.gov.hmrc.gform.eval.{ ExprType, ExpressionResultWithTypeInfo, StaticTypeData, TypeInfo }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.FormModel
import uk.gov.hmrc.gform.recalculation.{ EvaluationContext, EvaluationStatus, FreeCalculator }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieveId, DataRetrieveResult, VariadicValue }

final class FormModelVisibilityOptics(
  val formModel: FormModel,
  val freeCalculator: FreeCalculator
) {

  def cleared(modelComponentIds: List[ModelComponentId]): FormModelVisibilityOptics = new FormModelVisibilityOptics(
    formModel,
    freeCalculator.cleared(modelComponentIds)
  )

  def allFormComponents: List[FormComponent] = formModel.allFormComponents

  def allEditableFormComponentsForCoordinates(coordinates: Coordinates): List[FormComponent] =
    allFormComponentsForCoordinates(coordinates).filter(_.editable)

  def allFormComponentsForCoordinates(coordinates: Coordinates): List[FormComponent] =
    formModel.taskList.allFormComponents(coordinates)

  def allFormComponentIds: List[FormComponentId] =
    allFormComponents.map(_.id)

  def collect[B](pf: PartialFunction[(ModelComponentId, VariadicValue), B]): Iterable[B] =
    freeCalculator.variadicFormData.collect(pf)

  def fcLookup: Map[FormComponentId, FormComponent] = formModel.fcLookup

  def toStaticTypeData(formComponentId: FormComponentId): StaticTypeData =
    freeCalculator.metadata.staticTypeInfo
      .get(formComponentId.baseComponentId)
      .getOrElse(StaticTypeData(ExprType.String, None))

  private def explicitTypedExpr(expr: Expr, fcId: FormComponentId): TypeInfo = {
    val staticTypeData = toStaticTypeData(fcId)
    TypeInfo(expr, staticTypeData)
  }

  def evalAndApplyTypeInfoExplicit(expr: Expr, fcId: FormComponentId): ExpressionResultWithTypeInfo = {
    val typeInfo = explicitTypedExpr(expr, fcId)
    evalAndApplyTypeInfo(typeInfo)
  }

  def evalAndApplyTypeInfoFirst(expr: Expr): ExpressionResultWithTypeInfo = {
    val typeInfo = TypeInfo(expr, freeCalculator.metadata.exprStaticType(expr))
    evalAndApplyTypeInfo(typeInfo)
  }

  def evalAndApplyTypeInfo(typeInfo: TypeInfo): ExpressionResultWithTypeInfo =
    ExpressionResultWithTypeInfo(
      freeCalculator
        .evalTypeInfo(typeInfo)
        .applyTypeInfo(typeInfo),
      typeInfo.staticTypeData
    )

  def evalIncludeIfExpr(includeIf: IncludeIf, phase: Option[FormPhase]): Boolean =
    freeCalculator.evalIncludeIf(includeIf)

  def evalRemoveItemIf(removeItemIf: RemoveItemIf): Boolean =
    freeCalculator.evalRemoveItemIf(removeItemIf)

  def textResult(modelComponentId: ModelComponentId): Option[String] =
    // This handles fields like:
    //   "value": "${auth.ctutr}",
    //   "submitMode": "summaryinfoonly"
    freeCalculator.answerMapWithFallback.toStringResultOrOptionResult(modelComponentId) match {
      case EvaluationStatus.StringResult(value) => Some(value)
      case EvaluationStatus.NumberResult(value) => Some(value.toString)
      case _                                    => None
    }

  object data {
    def all: List[(ModelComponentId, VariadicValue)] =
      allFormComponents.flatMap(_.multiValueId.toModelComponentIds).flatMap { modelComponentId =>
        get(modelComponentId).map(modelComponentId -> _)
      }

    def one(modelComponentId: ModelComponentId): Option[String] =
      if (formModel.isDefinedAt(modelComponentId)) {
        freeCalculator.variadicFormData.one(modelComponentId)
      } else None

    def many(modelComponentId: ModelComponentId): Option[Seq[String]] =
      if (formModel.isDefinedAt(modelComponentId)) {
        freeCalculator.variadicFormData.many(modelComponentId)
      } else None

    def get(modelComponentId: ModelComponentId): Option[VariadicValue] =
      if (formModel.isDefinedAt(modelComponentId)) {
        freeCalculator.variadicFormData.get(modelComponentId)
      } else None

    def forCoordinate[A](coordinates: Coordinates): Set[VariadicValue] = {
      val modelComponentIds: List[ModelComponentId] =
        allEditableFormComponentsForCoordinates(coordinates).map(_.multiValueId).flatMap(_.toModelComponentIds)

      modelComponentIds.toSet.flatMap(freeCalculator.variadicFormData.get)

    }
  }

  def addDataRetrieveResults(dataRetrieveResults: List[DataRetrieveResult]): FormModelVisibilityOptics = {
    val evaluationContext: EvaluationContext = freeCalculator.evaluationContext.copy(
      thirdPartyData = freeCalculator.evaluationContext.thirdPartyData.updateDataRetrieve(dataRetrieveResults)
    )

    updateFreeCalculator(evaluationContext)
  }

  def removeDataRetrieveResults(dataRetrieves: List[DataRetrieveId]): FormModelVisibilityOptics = {
    val evaluationContext: EvaluationContext = freeCalculator.evaluationContext.copy(
      thirdPartyData = freeCalculator.evaluationContext.thirdPartyData.removeDataRetrieves(dataRetrieves)
    )

    updateFreeCalculator(evaluationContext)

  }

  private def updateFreeCalculator(evaluationContext: EvaluationContext): FormModelVisibilityOptics =
    new FormModelVisibilityOptics(
      this.formModel,
      freeCalculator.withEvaluationContext(evaluationContext)
    )
}
