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

import cats.Eq
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Expr

case class TypedExpr(expr: Expr, exprType: ExprType) {
  def leafs: List[TypedExpr] = expr.leafs.map(TypedExpr(_, exprType))
}

object TypedExpr {
  implicit val equal: Eq[TypedExpr] = Eq.fromUniversalEquals

  def illegal(expr: Expr): TypedExpr = TypedExpr(expr, ExprType.illegal)
  def string(expr: Expr): TypedExpr = TypedExpr(expr, ExprType.string)
  def sterling(expr: Expr): TypedExpr = TypedExpr(expr, ExprType.sterling)
  def number(expr: Expr): TypedExpr = TypedExpr(expr, ExprType.number)
  def wholeNumber(expr: Expr): TypedExpr = TypedExpr(expr, ExprType.wholeNumber)
  def choiceSelection(expr: Expr): TypedExpr = TypedExpr(expr, ExprType.choiceSelection)

  // format: off
  case object IsSterling {
    def unapply(typedExpr: TypedExpr): Option[Expr] =
      typedExpr.exprType.fold[Option[Expr]](_ => Some(typedExpr.expr))(_ => None)(_ => None)(_ => None)(_ => None)(_ => None)
  }
  case object IsNumber {
    def unapply(typedExpr: TypedExpr): Option[Expr] =
      typedExpr.exprType.fold[Option[Expr]](_ => None)(_ => Some(typedExpr.expr))(_ => None)(_ => None)(_ => None)(_ => None)
  }
  case object IsWholeNumber {
    def unapply(typedExpr: TypedExpr): Option[Expr] =
      typedExpr.exprType.fold[Option[Expr]](_ => None)(_ => None)(_ => Some(typedExpr.expr))(_ => None)(_ => None)(_ => None)
  }
  case object IsString {
    def unapply(typedExpr: TypedExpr): Option[Expr] =
      typedExpr.exprType.fold[Option[Expr]](_ => None)(_ => None)(_ => None)(_ => Some(typedExpr.expr))(_ => None)(_ => None)
  }
  case object IsChoiceSelection {
    def unapply(typedExpr: TypedExpr): Option[Expr] =
      typedExpr.exprType.fold[Option[Expr]](_ => None)(_ => None)(_ => None)(_ => None)(_ => Some(typedExpr.expr))(_ => None)
  }
  case object IsIllegal {
    def unapply(typedExpr: TypedExpr): Option[Expr] =
      typedExpr.exprType.fold[Option[Expr]](_ => None)(_ => None)(_ => None)(_ => None)(_ => None)(_ => Some(typedExpr.expr))
  }
  // format: on
}
