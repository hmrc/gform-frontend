/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import cats.Monoid
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.models.components._

sealed trait BooleanExpr
final case class Equals(left: Expr, right: Expr) extends BooleanExpr
final case class Or(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case class And(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case object IsTrue extends BooleanExpr

object BooleanExpr {
  implicit val format: OFormat[BooleanExpr] = derived.oformat

  private def isTrue(expr: BooleanExpr, data: Map[FieldId, Seq[String]]): Boolean =
    expr match {
      case Equals(FormCtx(fieldId), Constant(value)) if (data.getOrElse(FieldId(fieldId), Seq("")).head == value) => true
      case Or(expr1, expr2) => isTrue(expr1, data) | isTrue(expr2, data)
      case And(expr1, expr2) => isTrue(expr1, data) & isTrue(expr2, data)
      case IsTrue => true
      case _ => false
    }

  private def evaluate2(data: Map[FieldId, Seq[String]])(expr: BooleanExpr): Boolean =
    expr match {
      case Equals(FormCtx(fieldId), Constant(value)) if (data.getOrElse(FieldId(fieldId), Seq("")).head == value) => true
      case Or(expr1, expr2) => evaluate2(data)(expr1) | evaluate2(data)(expr2)
      case And(expr1, expr2) => evaluate2(data)(expr1) & evaluate2(data)(expr2)
      case IsTrue => true
      case _ => false
    }


  def sectionIdxAfter(idx: Int, formTemplate: FormTemplate, data: Map[FieldId, Seq[String]]): Option[Int] = {

    if (idx >= formTemplate.sections.size-1) None else {
      val nextSection = formTemplate.sections(idx + 1)
      val booleanExpr = nextSection.includeIf.getOrElse(IncludeIf(IsTrue)).expr
      if (isTrue(booleanExpr, data)) Some(idx+1) else {
        sectionIdxAfter(idx+1, formTemplate, data)
      }
    }
  }

  def nextTrueIdxOpt(idx: Int, expressions: List[BooleanExpr], data: Map[FieldId, Seq[String]]): Option[Int] = {

    if (idx >= expressions.size-1) return None
    if (isTrue(expressions(idx + 1), data)) return Some(idx+1)

    nextTrueIdxOpt(idx+1, expressions, data)
  }

}

sealed trait Comparison
final case object Equality extends Comparison

sealed trait BooleanOperation
final case object OrOperation extends BooleanOperation
final case object AndOperation extends BooleanOperation

