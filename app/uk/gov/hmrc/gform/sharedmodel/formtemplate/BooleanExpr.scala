/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import julienrf.json.derived
import play.api.libs.json._
import scala.language.higherKinds
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.graph.{ Convertible, Evaluator, IsConverted }
import uk.gov.hmrc.gform.sharedmodel.graph.GraphNode
import uk.gov.hmrc.http.HeaderCarrier

sealed trait BooleanExpr
final case class Equals(left: Expr, right: Expr) extends BooleanExpr
final case class NotEquals(left: Expr, right: Expr) extends BooleanExpr
final case class GreaterThan(left: Expr, right: Expr) extends BooleanExpr
final case class GreaterThanOrEquals(left: Expr, right: Expr) extends BooleanExpr
final case class LessThan(left: Expr, right: Expr) extends BooleanExpr
final case class LessThanOrEquals(left: Expr, right: Expr) extends BooleanExpr
final case class Not(e: BooleanExpr) extends BooleanExpr
final case class Or(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case class And(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case object IsTrue extends BooleanExpr
final case object IsFalse extends BooleanExpr

object BooleanExpr {
  implicit val format: OFormat[BooleanExpr] = derived.oformat
}

class BooleanExprEval[F[_]: Monad](
  val evaluator: Evaluator[F]
) {
  def isTrue(
    expr: BooleanExpr,
    data: Map[FormComponentId, Seq[String]],
    retrievals: MaterialisedRetrievals,
    visSet: Set[GraphNode],
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): F[Boolean] = {

    def loop(expr: BooleanExpr): F[Boolean] = isTrue(expr, data, retrievals, visSet, formTemplate)

    def decimalValue(expr: Expr, formTemplate: FormTemplate)(implicit hc: HeaderCarrier): Convertible[F] =
      evaluator
        .eval(visSet, FormComponentId("dummy"), expr, data, retrievals, formTemplate)

    def compare(
      leftField: Expr,
      bigDecimalRelation: (BigDecimal, BigDecimal) => Boolean,
      stringRelation: (String, String) => Boolean,
      rightField: Expr,
      formTemplate: FormTemplate)(implicit hc: HeaderCarrier): F[Boolean] = {

      val left: Convertible[F] = decimalValue(leftField, formTemplate)
      val right: Convertible[F] = decimalValue(rightField, formTemplate)

      def compareByStringRel(l: Convertible[F], r: Convertible[F]): F[Boolean] =
        for {
          convLeft  <- Convertible.asString(l)
          convRight <- Convertible.asString(r)
        } yield stringRelation(convLeft, convRight)

      def compareMaybeBigDecimal(maybeBdA: Option[BigDecimal], maybeBdB: Option[BigDecimal]): F[Boolean] =
        (maybeBdA, maybeBdB) match {
          case (Some(bdA), Some(bdB)) => bigDecimalRelation(bdA, bdB).pure[F]
          case (_, _)                 => compareByStringRel(left, right)
        }

      (left, right) match {
        case (IsConverted(maybeConvLeftF), IsConverted(maybeConvRightF)) =>
          for {
            maybeConvLeft  <- maybeConvLeftF
            maybeConvRight <- maybeConvRightF
            res            <- compareMaybeBigDecimal(maybeConvLeft, maybeConvRight)
          } yield res
        case (l, r) => compareByStringRel(l, r)
      }
    }

    expr match {
      case Equals(field1, field2)              => compare(field1, _ == _, _ == _, field2, formTemplate)
      case NotEquals(field1, field2)           => compare(field1, _ != _, _ != _, field2, formTemplate)
      case GreaterThan(field1, field2)         => compare(field1, _ > _, _ > _, field2, formTemplate)
      case GreaterThanOrEquals(field1, field2) => compare(field1, _ >= _, _ >= _, field2, formTemplate)
      case LessThan(field1, field2)            => compare(field1, _ < _, _ < _, field2, formTemplate)
      case LessThanOrEquals(field1, field2)    => compare(field1, _ <= _, _ <= _, field2, formTemplate)
      case Not(expr)                           => loop(expr).map(!_)
      case Or(expr1, expr2)                    => for { e1 <- loop(expr1); e2 <- loop(expr2) } yield e1 | e2
      case And(expr1, expr2)                   => for { e1 <- loop(expr1); e2 <- loop(expr2) } yield e1 & e2
      case IsTrue                              => true.pure[F]
      case IsFalse                             => false.pure[F]
    }
  }

}
