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

package uk.gov.hmrc.gform.eval

import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.{ FormModel, PageMode }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormComponentId }

sealed trait ExprMetadata extends Product with Serializable {
  def toEvaluationResults[A <: PageMode](
    evResult: EvaluationResults,
    formModel: FormModel[A],
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext
  ): EvaluationResults =
    this match {
      case InferrableExprProjection((InferrableExpr(expr, InferringRule.Explicit(fcId)))) =>
        val typedExpr = formModel.explicitTypedExpr(expr, fcId)
        evResult + (typedExpr, evResult.evalTyped(typedExpr, recData, evaluationContext))
      case InferrableExprProjection((InferrableExpr(expr, InferringRule.FirstOperand))) =>
        val typedExpr = formModel.toTypedExpr(expr)
        evResult + (typedExpr, evResult.evalTyped(typedExpr, recData, evaluationContext))
    }
}

object ExprMetadata {
  // Expression of FormComponent 'cannot' refer to its FormComponent's id.
  case class Plain(inferrableExpr: InferrableExpr) extends ExprMetadata
  // Some expressions of FormComponent 'can' refer to its FormComponent's id.
  // Used by errorMessages, these messages are being show to the user only after
  // value has been provided for the component. So it is ok to allow for them to
  // contains formComponentId on which it is defined.
  case class SelfReferring(inferrableExpr: InferrableExpr, selfReference: FormComponentId) extends ExprMetadata
}

trait IsSelfReferring extends Product with Serializable
object IsSelfReferring {
  case class No(expr: Expr) extends IsSelfReferring
  case class Yes(expr: Expr, selfReference: FormComponentId) extends IsSelfReferring
}

// Projection for ExprMetadata which disregard information about type of expression
object SelfReferenceProjection {
  def unapply(exprMetadata: ExprMetadata): Option[IsSelfReferring] = exprMetadata match {
    case ExprMetadata.Plain(InferrableExpr(expr, _)) => Some(IsSelfReferring.No(expr))
    case ExprMetadata.SelfReferring(InferrableExpr(expr, _), selfReference) =>
      Some(IsSelfReferring.Yes(expr, selfReference))
  }
}

// Projection for ExprMetadata which disregard information about self referring
object InferrableExprProjection {
  def unapply(exprMetadata: ExprMetadata): Option[InferrableExpr] = exprMetadata match {
    case ExprMetadata.Plain(inferrableExpr)            => Some(inferrableExpr)
    case ExprMetadata.SelfReferring(inferrableExpr, _) => Some(inferrableExpr)
  }
}

// Projection for ExprMetadata which extract Expr's only
object ExprOnlyProjection {
  def unapply(exprMetadata: ExprMetadata): Option[Expr] = exprMetadata match {
    case ExprMetadata.Plain(InferrableExpr(expr, _))            => Some(expr)
    case ExprMetadata.SelfReferring(InferrableExpr(expr, _), _) => Some(expr)
  }
}
