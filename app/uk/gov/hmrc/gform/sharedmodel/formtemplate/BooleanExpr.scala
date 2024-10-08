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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json._
import scala.util.matching.Regex

sealed trait BooleanExpr {
  def allExpressions: List[Expr] = this match {
    case Equals(left, right)              => left :: right :: Nil
    case GreaterThan(left, right)         => left :: right :: Nil
    case DateAfter(left, right)           => DateCtx(left) :: DateCtx(right) :: Nil
    case GreaterThanOrEquals(left, right) => left :: right :: Nil
    case LessThan(left, right)            => left :: right :: Nil
    case DateBefore(left, right)          => DateCtx(left) :: DateCtx(right) :: Nil
    case LessThanOrEquals(left, right)    => left :: right :: Nil
    case Not(e)                           => e.allExpressions
    case Or(left, right)                  => left.allExpressions ++ right.allExpressions
    case And(left, right)                 => left.allExpressions ++ right.allExpressions
    case IsTrue                           => Nil
    case IsFalse                          => Nil
    case Contains(multiValueField, value) => multiValueField :: value :: Nil
    case In(formCtx, _)                   => formCtx :: Nil
    case HasAnswer(formCtx, atlFormCtx)   => List(formCtx) ++ atlFormCtx.allExpressions.toList
    case MatchRegex(expr, _)              => expr :: Nil
    case FormPhase(_)                     => Nil
    case First(formCtx)                   => formCtx :: Nil
    case IsLogin(_)                       => Nil
    case DuplicateExists(_)               => Nil
  }

  def prettyPrint: String = ExprPrettyPrint.prettyPrintBooleanExpr(this)
}

final case class Equals(left: Expr, right: Expr) extends BooleanExpr
final case class GreaterThan(left: Expr, right: Expr) extends BooleanExpr
final case class GreaterThanOrEquals(left: Expr, right: Expr) extends BooleanExpr
final case class LessThan(left: Expr, right: Expr) extends BooleanExpr
final case class LessThanOrEquals(left: Expr, right: Expr) extends BooleanExpr
final case class Not(e: BooleanExpr) extends BooleanExpr
final case class Or(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case class And(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case object IsTrue extends BooleanExpr
final case object IsFalse extends BooleanExpr
final case class Contains(multiValueField: FormCtx, value: Expr) extends BooleanExpr
final case class In(value: Expr, dataSource: DataSource) extends BooleanExpr
final case class HasAnswer(formCtx: FormCtx, addToListRef: AddToListRef) extends BooleanExpr
final case class MatchRegex(expr: Expr, regex: Regex) extends BooleanExpr

final case class DateBefore(left: DateExpr, right: DateExpr) extends BooleanExpr
final case class DateAfter(left: DateExpr, right: DateExpr) extends BooleanExpr
final case class First(formCtx: FormCtx) extends BooleanExpr
final case class DuplicateExists(fieldList: Seq[FormCtx]) extends BooleanExpr
final case class IsLogin(value: LoginInfo) extends BooleanExpr

final case class FormPhase(value: FormPhaseValue) extends BooleanExpr
sealed trait FormPhaseValue
case object InstructionPDF extends FormPhaseValue

object FormPhaseValue {
  implicit val format: OFormat[FormPhaseValue] = derived.oformat()
}

object BooleanExpr {
  implicit val format: OFormat[BooleanExpr] = derived.oformat()

  implicit val regexFormat: Format[Regex] = {
    val reads: Reads[Regex] = Reads.of[String].map(_.r)
    val writes: Writes[Regex] = Writes.of[String].contramap(_.toString)
    Format(reads, writes)
  }
}
