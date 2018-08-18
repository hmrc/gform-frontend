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

import cats.implicits._
import java.text.NumberFormat
import java.util.Locale
import play.api.Logger
import scala.util.{ Failure, Success, Try }
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import uk.gov.hmrc.gform.commons.BigDecimalUtil
import uk.gov.hmrc.gform.models.helpers.{ HasDigits, HasSterling }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.DependencyGraph._
import uk.gov.hmrc.gform.models.helpers.FormComponentHelper.roundTo

sealed trait GraphException {
  def reportProblem: String = this match {
    case NoTopologicalOrder(fcId, graph) =>
      s"$graph is not possible to sort topologically. Violating node is $fcId. Does graph contains cycle: ${graph.findCycle}"
    case NoFormComponent(fcId, lookup) =>
      s"""No FormComponent found for $fcId. Available FormComponents: ${lookup.keys.mkString(",")}"""
  }
}

case class NoTopologicalOrder(fcId: FormComponentId, graph: Graph[FormComponentId, DiEdge]) extends GraphException
case class NoFormComponent(fcId: FormComponentId, lookup: Map[FormComponentId, FormComponent]) extends GraphException

object Recalculation {

  def recalculateFormData(formData: FormData, formTemplate: FormTemplate): Either[GraphException, FormData] = {

    val graph: Graph[FormComponentId, DiEdge] = toGraph(formTemplate)

    val fcLookup: Map[FormComponentId, FormComponent] =
      formTemplate.expandFormTemplate.allFCs.map(fc => fc.id -> fc).toMap

    val lookupMap: Map[FormComponentId, String] =
      formData.fields.map { case FormField(id, value) => id -> value }.toMap

    for {
      graphTopologicalOrder <- constructDepencyGraph(graph).leftMap(node => NoTopologicalOrder(node.toOuter, graph))
      recalc <- {
        val genesisLookup: Either[GraphException, Map[FormComponentId, String]] = Right(lookupMap)
        graphTopologicalOrder.map(_._2).foldRight(genesisLookup) {
          case (iter, dataLookup) =>
            val nodes: List[FormComponentId] = iter.toList.collect {
              case node if lookupMap.get(node.toOuter).isDefined => node.toOuter
            }
            nodes.foldLeft(dataLookup) {
              case (dataLookupE, node) => dataLookupE.flatMap(calculate(node, fcLookup, _))
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
      fc <- Either.fromOption(fcLookup.get(fcId), NoFormComponent(fcId, fcLookup))
    } yield {
      if (fc.editable) dataLookup else dataLookup + (fcId -> recalculate(fc, dataLookup))
    }

  private def recalculate(fc: FormComponent, dataLookup: Map[FormComponentId, String]): String =
    fc match {
      case HasExpr(SingleExpr(expr)) =>
        eval(fc.id, expr, dataLookup) match {
          case Left(x) => x
          case Right(bigDecimal) =>
            defaultFormat(roundTo(fc))
              .format(bigDecimal)
              .replaceAll(",", "") // Format number to have required number of decimal places, but do not keep commas
        }
      case _ => ""
    }

  private def defaultFormat(i: Int) = {
    val formatter = NumberFormat.getInstance(Locale.UK)
    formatter.setMaximumFractionDigits(i)
    formatter
  }

  private def sum(fcId: FormComponentId, fc: String, dataLookup: Map[FormComponentId, String]) = {
    val results = dataLookup.collect { case (key, value) if key.value.endsWith(fc) => Constant(value) }
    val summation = results.foldLeft(Expr.additionIdentity)(Add)
    eval(fcId, summation, dataLookup)
  }

  private def eval(
    fcId: FormComponentId,
    expr: Expr,
    dataLookup: Map[FormComponentId, String]): Either[String, BigDecimal] =
    expr match {
      case Value                       => dataLookup.getOrElse(fcId, "").asLeft
      case Constant(fc)                => fc.asLeft
      case fc @ FormCtx(_)             => dataLookup.getOrElse(fc.toFieldId, "").asLeft
      case Sum(FormCtx(fc))            => sum(fcId, fc, dataLookup)
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
    def calc(expr: Expr) = eval(fcId, expr, dataLookup).leftMap(BigDecimalUtil.toBigDecimalDefault).merge
    operator(calc(xExpr), calc(yExpr))
  }
}
