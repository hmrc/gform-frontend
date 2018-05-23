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

import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers

import scala.util.{ Failure, Success, Try }

sealed trait BooleanExpr
final case class Equals(left: Expr, right: Expr) extends BooleanExpr
final case class GreaterThan(left: Expr, right: Expr) extends BooleanExpr
final case class GreaterThanOrEquals(left: Expr, right: Expr) extends BooleanExpr
final case class LessThan(left: Expr, right: Expr) extends BooleanExpr
final case class LessThanOrEquals(left: Expr, right: Expr) extends BooleanExpr
final case class Or(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case class And(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case object IsTrue extends BooleanExpr
final case object IsFalse extends BooleanExpr

object BooleanExpr {
  implicit val format: OFormat[BooleanExpr] = derived.oformat

  def isTrue(expr: BooleanExpr, data: Map[FormComponentId, Seq[String]], retrievals: Retrievals): Boolean = {

    def getField(id: String): String = FormDataHelpers.get(data, FormComponentId(id)).headOption.getOrElse("")

    expr match {
      case Equals(field1, field2) => equals(prepopData(field1, retrievals, data), prepopData(field2, retrievals, data))
      case GreaterThan(field1, field2) =>
        greaterThan(prepopData(field1, retrievals, data), prepopData(field2, retrievals, data))
      case GreaterThanOrEquals(field1, field2) =>
        greaterThanOrEquals(prepopData(field1, retrievals, data), prepopData(field2, retrievals, data))
      case LessThan(field1, field2) =>
        lessThan(prepopData(field1, retrievals, data), prepopData(field2, retrievals, data))
      case LessThanOrEquals(field1, field2) =>
        lessThanOrEquals(prepopData(field1, retrievals, data), prepopData(field2, retrievals, data))
      case Or(expr1, expr2)  => isTrue(expr1, data, retrievals) | isTrue(expr2, data, retrievals)
      case And(expr1, expr2) => isTrue(expr1, data, retrievals) & isTrue(expr2, data, retrievals)
      case IsTrue            => true
      case _                 => false
    }
  }

  private def equals(left: String, right: String): Boolean =
    (toMaybeBigDecimal(left), toMaybeBigDecimal(right), left, right) match {
      case (Some(l), Some(r), _, _) => (l == r)
      case (_, _, l, r)             => (l == r)
    }

  private def greaterThan(left: String, right: String): Boolean =
    (toMaybeBigDecimal(left), toMaybeBigDecimal(right), left, right) match {
      case (Some(l), Some(r), _, _) => (l > r)
      case (_, _, l, r)             => (l > r)
    }

  private def greaterThanOrEquals(left: String, right: String): Boolean =
    (toMaybeBigDecimal(left), toMaybeBigDecimal(right), left, right) match {
      case (Some(l), Some(r), _, _) => (l >= r)
      case (_, _, l, r)             => (l >= r)
    }

  private def lessThan(left: String, right: String): Boolean =
    (toMaybeBigDecimal(left), toMaybeBigDecimal(right), left, right) match {
      case (Some(l), Some(r), _, _) => (l < r)
      case (_, _, l, r)             => (l < r)
    }

  private def lessThanOrEquals(left: String, right: String): Boolean =
    (toMaybeBigDecimal(left), toMaybeBigDecimal(right), left, right) match {
      case (Some(l), Some(r), _, _) => (l <= r)
      case (_, _, l, r)             => (l <= r)
    }

  def prepopData(expr: Expr, retrievals: Retrievals, data: Map[FormComponentId, Seq[String]]): String = {

    def toBigDecimal(str: String): BigDecimal =
      Try(BigDecimal(str.replace(",", ""))) match {
        case Success(x) => x
        case Failure(_) => BigDecimal(0)
      }

    expr match {
      case Constant(value) => value
      case UserCtx(_)      => retrievals.affinityGroupName

      case Add(field1, field2) => {
        val y = prepopData(field1, retrievals, data)
        val z = prepopData(field2, retrievals, data)
        (toBigDecimal(y) + toBigDecimal(z)).toString()
      }

      case Subtraction(field1, field2) =>
        val y = prepopData(field1, retrievals, data)
        val z = prepopData(field2, retrievals, data)
        (toBigDecimal(y) - toBigDecimal(z)).toString()

      case Multiply(field1, field2) =>
        val y = prepopData(field1, retrievals, data)
        val z = prepopData(field2, retrievals, data)
        (toBigDecimal(y) * toBigDecimal(z)).toString()

      case id: FormCtx => data.get(id.toFieldId).map(_.head).getOrElse("")
      case _           => ""
    }
  }

  private def toMaybeBigDecimal(str: String): Option[BigDecimal] =
    Try(BigDecimal(str.replace(",", ""))) match {
      case Success(x) => Some(x)
      case Failure(_) => None
    }

}

sealed trait Comparison
final case object Equality extends Comparison

sealed trait BooleanOperation
final case object OrOperation extends BooleanOperation
final case object AndOperation extends BooleanOperation
