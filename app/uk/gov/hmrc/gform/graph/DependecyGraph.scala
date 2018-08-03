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

package uk.gov.hmrc.gform.graph

import cats._
import cats.implicits._
import java.text.{ DecimalFormat, NumberFormat }
import java.util.Locale
import play.api.Logger
import scala.util.{ Failure, Success, Try }
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import uk.gov.hmrc.gform.commons.BigDecimalUtil
import uk.gov.hmrc.gform.models.helpers.{ HasDigits, HasExpr, HasSterling }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

sealed trait GraphException {
  def reportProblem: String = this match {
    case NoTopologicalOrder(fcId, graph) =>
      s"$graph is not possible to sort topologically. Violating node is $fcId. Does graph contains cycle: ${graph.findCycle}"
    case NoFormComponent(fcId, lookup) =>
      s"""No FormComponent found for $fcId. Available FormComponents: ${lookup.keys.mkString(",")}"""
    case NoDataFound(fcId, lookup) =>
      s"""No submitted data found for $fcId. Available data: ${lookup.keys.mkString(",")}"""
  }
}

case class NoTopologicalOrder(fcId: FormComponentId, graph: Graph[FormComponentId, DiEdge]) extends GraphException
case class NoFormComponent(fcId: FormComponentId, lookup: Map[FormComponentId, FormComponent]) extends GraphException
case class NoDataFound(fcId: FormComponentId, lookup: Map[FormComponentId, String]) extends GraphException

object DependencyGraph {

  private val emptyGraph: Graph[FormComponentId, DiEdge] = Graph.empty

  private def toGraph(formTemplate: FormTemplate): Graph[FormComponentId, DiEdge] = {
    val graphs: List[Graph[FormComponentId, DiEdge]] = formTemplate.sections.flatMap(_.fields.map(fromFormComponent))
    graphs.foldLeft(emptyGraph)(_ ++ _)
  }

  private def constructDepencyGraph(
    graph: Graph[FormComponentId, DiEdge]): Either[GraphException, graph.LayeredTopologicalOrder[graph.NodeT]] =
    graph.topologicalSort.map(_.toLayered).leftMap(node => NoTopologicalOrder(node.toOuter, graph))

  private def fromFormComponent(fc: FormComponent): Graph[FormComponentId, DiEdge] = {
    val fcIds: List[FormComponentId] = fc match {
      case HasExpr(expr) => fromExpr(expr)
      case _             => List.empty
    }

    fcIds.map(fc.id ~> _).foldLeft(emptyGraph)(_ + _)
  }

  private def fromExpr(expr: Expr): List[FormComponentId] =
    expr match {
      case FormCtx(fc) => FormComponentId(fc) :: Nil
      // case Sum(FormCtx(fc)) => ??? // TODO JoVl implement once GFC-544 is fixed
      case Add(field1, field2)         => fromExpr(field1) ++ fromExpr(field2)
      case Subtraction(field1, field2) => fromExpr(field1) ++ fromExpr(field2)
      case Multiply(field1, field2)    => fromExpr(field1) ++ fromExpr(field2)
      case otherwise                   => List.empty
    }

  def recalculateFormData(formData: FormData, formTemplate: FormTemplate): Either[GraphException, FormData] = {

    val graph: Graph[FormComponentId, DiEdge] = toGraph(formTemplate)

    val fcLookup: Map[FormComponentId, FormComponent] =
      formTemplate.sections.flatMap(_.fields).map(fc => fc.id -> fc).toMap

    val lookupMap: Map[FormComponentId, String] =
      formData.fields.map { case FormField(id, value) => id -> value }.toMap

    for {
      graphTopologicalOrder <- constructDepencyGraph(graph)
      recalc <- {
        val genesisLookup: Either[GraphException, Map[FormComponentId, String]] = Right(lookupMap)
        graphTopologicalOrder.map(_._2).foldRight(genesisLookup) {
          case (iter, dataLookup) =>
            iter.toList.foldLeft(dataLookup) {
              case (dataLookupE, node) => dataLookupE.flatMap(calculate(node.toOuter, fcLookup, _))
            }
        }
      }
    } yield {
      val fieldsUpd = recalc.map { case (k, v) => FormField(k, v) }.toSeq
      formData.copy(fields = fieldsUpd)
    }
  }

  private def calculate(
    fcId: FormComponentId,
    fcLookup: Map[FormComponentId, FormComponent],
    dataLookup: Map[FormComponentId, String]): Either[GraphException, Map[FormComponentId, String]] =
    for {
      fc   <- Either.fromOption(fcLookup.get(fcId), NoFormComponent(fcId, fcLookup))
      data <- Either.fromOption(dataLookup.get(fcId), NoDataFound(fcId, dataLookup))
    } yield {
      val result = recalculate(fc, dataLookup)
      dataLookup + (fcId -> result)
    }

  private def recalculate(fc: FormComponent, dataLookup: Map[FormComponentId, String]): String =
    fc match {
      case HasExpr(expr) =>
        calculateExpr(fc.id, expr, dataLookup) match {
          case Left(x) => x
          case Right(bigDecimal) =>
            val roundTo = fc.`type` match {
              case HasDigits(digits)   => digits
              case HasSterling(digits) => digits
              case _                   => TextConstraint.defaultFactionalDigits
            }
            defaultFormat(roundTo).format(bigDecimal)
        }
      case _ => ""
    }

  private def defaultFormat(i: Int) = {
    val formatter = NumberFormat.getInstance(Locale.UK)
    formatter.setMaximumFractionDigits(i)
    formatter
  }

  private def calculateExpr(
    fcId: FormComponentId,
    expr: Expr,
    dataLookup: Map[FormComponentId, String]): Either[String, BigDecimal] =
    expr match {
      case Value        => dataLookup.getOrElse(fcId, "").asLeft
      case Constant(fc) => fc.asLeft
      case FormCtx(fc)  => dataLookup.getOrElse(FormComponentId(fc), "").asLeft
      // case Sum(FormCtx(fc)) => ??? // TODO JoVl implement once GFC-544 is fixed
      case Add(field1, field2)         => makeCalc(fcId, dataLookup, _ + _, field1, field2).asRight
      case Subtraction(field1, field2) => makeCalc(fcId, dataLookup, _ - _, field1, field2).asRight
      case Multiply(field1, field2)    => makeCalc(fcId, dataLookup, _ * _, field1, field2).asRight
      case otherwise                   => "".asLeft
    }

  private def makeCalc(
    fcId: FormComponentId,
    dataLookup: Map[FormComponentId, String],
    operator: (BigDecimal, BigDecimal) => BigDecimal,
    xExpr: Expr,
    yExpr: Expr
  ): BigDecimal = {
    def calc(expr: Expr) = calculateExpr(fcId, expr, dataLookup).leftMap(BigDecimalUtil.toBigDecimalDefault).merge
    operator(calc(xExpr), calc(yExpr))
  }
}
