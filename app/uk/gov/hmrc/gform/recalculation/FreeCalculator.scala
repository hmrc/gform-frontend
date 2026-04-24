/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.recalculation

import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.{ ExpressionResultWithTypeInfo, StaticTypeData, TypeInfo }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

// This is used to construct VisibilityModel and to evaluate expressions in SmartStrings
//
// This calculator expects runtime form component ids. Like 1_lastName, 2_lastName etc.
// It is caller's responsibility to provide runtime form component ids in booleanExprs and exprs.
final class FreeCalculator(
  val metadata: Metadata,
  val formModelMetadata: FormModelMetadata,
  val answerMapWithFallback: AnswerMapWithFallback,
  val evaluationContext: EvaluationContext
)(implicit messages: Messages) {

  def cleared(modelComponentIds: List[ModelComponentId]): FreeCalculator = new FreeCalculator(
    metadata,
    formModelMetadata,
    answerMapWithFallback.cleared(modelComponentIds),
    evaluationContext
  )

  val calc = RuntimeCalculator(metadata, answerMapWithFallback, evaluationContext, formModelMetadata)

  def withEvaluationContext(evaluationContext: EvaluationContext): FreeCalculator =
    new FreeCalculator(metadata, formModelMetadata, answerMapWithFallback, evaluationContext)

  def withFormModelMetadata(formModelMetadata: FormModelMetadata): FreeCalculator =
    new FreeCalculator(metadata, formModelMetadata, answerMapWithFallback, evaluationContext)

  val answerMap = answerMapWithFallback.answerMap

  val variadicFormData = answerMapWithFallback.mongoUserData.lookup

  def allModelComponentIds(modelComponentId: ModelComponentId): List[(ModelComponentId, EvaluationStatus)] =
    answerMapWithFallback.allModelComponentIds(modelComponentId)

  def evalIncludeIf(includeIf: IncludeIf): Boolean = evalBooleanExpr(includeIf.booleanExpr)

  def evalRemoveItemIf(removeItemIf: RemoveItemIf): Boolean = evalBooleanExpr(removeItemIf.booleanExpr)

  def evalValidIf(validIf: ValidIf): Boolean = evalBooleanExpr(validIf.booleanExpr)

  def evalBooleanExpr(booleanExpr: BooleanExpr): Boolean = calc.evalBooleanExpr(booleanExpr)

  def evalExpr(expr: Expr): ExpressionResultWithTypeInfo = calc.evalExpr(expr)

  def evalExpr(expr: Expr, staticTypeData: StaticTypeData, behaviour: Behaviour): EvaluationStatus =
    calc.evalExpr(expr, staticTypeData, behaviour)

  def evalTypeInfo(typeInfo: TypeInfo): EvaluationStatus =
    evalExpr(typeInfo.expr, typeInfo.staticTypeData, Behaviour.Default)
}
