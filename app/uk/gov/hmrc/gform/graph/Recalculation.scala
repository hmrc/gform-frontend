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

import cats.{ Functor, Monad, MonadError }
import cats.syntax.eq._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.traverse._
import cats.data.EitherT
import java.text.NumberFormat
import java.util.Locale
import scala.math.BigDecimal.RoundingMode
import scala.language.higherKinds
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.commons.{ BigDecimalUtil, NumberFormatUtil }
import uk.gov.hmrc.gform.gform.AuthContextPrepop
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.{ DependencyGraph, GraphNode, IncludeIfGN, SimpleGN }
import uk.gov.hmrc.gform.models.helpers.FormComponentHelper.roundTo
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil._
import uk.gov.hmrc.http.HeaderCarrier

sealed trait GraphException {
  def reportProblem: String = this match {
    case NoTopologicalOrder(fcId, graph) =>
      s"$graph is not possible to sort topologically. Violating node is $fcId. Does graph contains cycle: ${graph.findCycle}"
    case NoFormComponent(fcId, lookup) =>
      s"""No FormComponent found for $fcId. Available FormComponents: ${lookup.keys.mkString(",")}"""
  }
}

case class NoTopologicalOrder(fcId: GraphNode, graph: Graph[GraphNode, DiEdge]) extends GraphException
case class NoFormComponent(fcId: FormComponentId, lookup: Map[FormComponentId, FormComponent]) extends GraphException

class Recalculation[F[_]: Monad, E](
  val booleanExprEval: BooleanExprEval[F],
  val error: GraphException => E
) {

  type Context = (Set[GraphNode], Data)
  type ContextWithNodesToRecalculate = (Set[GraphNode], List[FormComponentId])

  def recalculateFormData(data: Data, formTemplate: FormTemplate, retrievals: MaterialisedRetrievals)(
    implicit hc: HeaderCarrier,
    me: MonadError[F, E]): F[FormDataRecalculated] =
    recalculateFormData_(data, formTemplate, retrievals).value.flatMap {
      case Left(graphException) => me.raiseError(error(graphException))
      case Right(fd)            => fd.pure[F]
    }

  private def recalculateFormData_(data: Data, formTemplate: FormTemplate, retrievals: MaterialisedRetrievals)(
    implicit hc: HeaderCarrier): EitherT[F, GraphException, FormDataRecalculated] = {

    val graph: Graph[GraphNode, DiEdge] = DependencyGraph.toGraph(formTemplate)

    val fcLookup: Map[FormComponentId, FormComponent] = formTemplate.expandFormTemplate.fcsLookup

    val orderedGraph: Either[GraphException, graph.LayeredTopologicalOrder[graph.NodeT]] = DependencyGraph
      .constructDepencyGraph(graph)
      .leftMap(node => NoTopologicalOrder(node.toOuter, graph))

    val orderedGraphT: EitherT[F, GraphException, graph.LayeredTopologicalOrder[graph.NodeT]] = EitherT(
      orderedGraph.pure[F])

    /**
      * Adds invisible nodes to invisibility set and returns nodes to recalculate for current Graph layer
      */
    def markInvisible(
      acc: ContextWithNodesToRecalculate,
      dataLookup: Data,
      gn: GraphNode): F[ContextWithNodesToRecalculate] = {
      val (visibilitySet, nodesToRecalculate) = acc
      gn match {
        case s @ SimpleGN(fcId) =>
          if (booleanExprEval.evaluator.isHidden(fcId, visibilitySet)) {
            (visibilitySet ++ Set(s), fcId :: nodesToRecalculate).pure[F]
          } else
            (visibilitySet, fcId :: nodesToRecalculate).pure[F]
        case n @ IncludeIfGN(_, includeIf) =>
          val isSectionVisible: F[Boolean] =
            booleanExprEval
              .isTrue(includeIf.expr, dataLookup, retrievals, visibilitySet, formTemplate)
          for {
            sectionVisible <- isSectionVisible
          } yield {
            if (sectionVisible) {
              (visibilitySet, nodesToRecalculate)
            } else {
              val predecessors: Set[graph.NodeT] = graph.find(n).map(_.diPredecessors).toSet.flatten
              val hidden: Set[GraphNode] = predecessors.map(_.toOuter)
              (visibilitySet ++ hidden + n, nodesToRecalculate)
            }
          }
      }
    }

    def recalculateGraphLayer(graphLayer: Iterable[graph.NodeT], ctx: Context): EitherT[F, GraphException, Context] = {
      val (visibilitySet, dataLookup) = ctx

      val extendVisibility: EitherT[F, GraphException, ContextWithNodesToRecalculate] = {
        val visNodes: F[ContextWithNodesToRecalculate] =
          graphLayer.toList.map(_.toOuter).foldLeft((visibilitySet, List.empty[FormComponentId]).pure[F]) {
            case (accF, graphNode) =>
              for {
                acc <- accF
                res <- markInvisible(acc, dataLookup, graphNode)
              } yield res
          }

        EitherT(visNodes.map(Right.apply))
      }

      def recalculateLayerNodes(
        visSet: Set[GraphNode],
        nodes: List[FormComponentId]): EitherT[F, GraphException, Data] = {

        val genesis: EitherT[F, GraphException, Data] = EitherT(dataLookup.pure[F].map(Right.apply))

        nodes.foldLeft(genesis) {
          case (dataLookupF, node) =>
            for {
              dataLookupUpd <- dataLookupF
              recalculated  <- EitherT(calculate(visSet, node, fcLookup, dataLookupUpd, retrievals, formTemplate))
            } yield recalculated
        }
      }

      for {
        visAndNodes <- extendVisibility
        (visSet, nodesToRecalculate) = visAndNodes
        recalculatedData <- recalculateLayerNodes(visSet, nodesToRecalculate)
      } yield (visSet, recalculatedData)
    }

    val contextE: Either[GraphException, Context] =
      Right((Set.empty, data))

    val genesisContext: EitherT[F, GraphException, Context] =
      EitherT(contextE.pure[F])

    for {
      graphTopologicalOrder <- orderedGraphT
      recalc <- graphTopologicalOrder.foldRight(genesisContext) {
                 case ((_, graphLayer), contextF) =>
                   for {
                     context    <- contextF
                     updatedCtx <- recalculateGraphLayer(graphLayer, context)
                   } yield updatedCtx
               }
    } yield {
      val (allInvisible, recalculatedData) = recalc
      FormDataRecalculated(allInvisible, recalculatedData)
    }
  }

  private def hasData(fc: FormComponent, dataLookup: Data): Boolean =
    dataLookup.get(fc.id).fold(false)(_.filter(_.isEmpty).isEmpty)

  private def calculate(
    visSet: Set[GraphNode],
    fcId: FormComponentId,
    fcLookup: Map[FormComponentId, FormComponent],
    dataLookup: Data,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): F[Either[GraphException, Data]] =
    Either.fromOption(fcLookup.get(fcId), NoFormComponent(fcId, fcLookup)).traverse { fc =>
      if (fc.editable && hasData(fc, dataLookup)) dataLookup.pure[F]
      else
        fc match {
          case IsText(_) | IsTextArea(_) =>
            recalculate(fc, visSet, dataLookup, retrievals, formTemplate).map(a => dataLookup + (fcId -> Seq(a)))
          case _ => dataLookup.pure[F] // Nothing to recompute on non-text components
        }
    }

  private def recalculate(
    fc: FormComponent,
    visSet: Set[GraphNode],
    dataLookup: Data,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): F[String] =
    fc match {
      case HasExpr(SingleExpr(expr)) =>
        booleanExprEval.evaluator.eval(visSet, fc.id, expr, dataLookup, retrievals, formTemplate) match {
          case NonConvertible(x)   => x
          case MaybeConvertible(x) => x
          case Converted(bigDecimalF) =>
            bigDecimalF.map {
              case NonComputable => ""
              case Computed(bigDecimal) =>
                defaultFormat(roundTo(fc)) // TODO JoVl use Convertible.round
                  .format(bigDecimal)
                  .replaceAll(",", "") // Format number to have required number of decimal places, but do not keep commas
            }
        }
      case _ => "".pure[F]
    }

  def defaultFormat(i: Int) = {
    val formatter = NumberFormat.getInstance(Locale.UK)
    formatter.setMaximumFractionDigits(i)
    formatter
  }
}

class Evaluator[F[_]: Monad](
  eeittPrepop: (Eeitt, MaterialisedRetrievals, FormTemplate, HeaderCarrier) => F[String]
) {

  val default = "0"
  val defaultF = default.pure[F]
  val nothingF = "".pure[F]

  def eval(
    visSet: Set[GraphNode],
    fcId: FormComponentId,
    expr: Expr,
    dataLookup: Data,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): Convertible[F] =
    expr match {
      case Value           => MaybeConvertible(dataLookup.get(fcId).flatMap(_.headOption).getOrElse("").pure[F])
      case UserCtx(_)      => NonConvertible(affinityGroupNameO(retrievals.affinityGroup).pure[F])
      case AuthCtx(value)  => NonConvertible(AuthContextPrepop.values(value, retrievals).pure[F])
      case EeittCtx(eeitt) => NonConvertible(eeittPrepop(eeitt, retrievals, formTemplate, hc))
      case Constant(fc)    => MaybeConvertible(fc.pure[F])
      case fc @ FormCtx(_) =>
        if (isHidden(fc.toFieldId, visSet)) MaybeConvertible(defaultF)
        else MaybeConvertible(dataLookup.get(fc.toFieldId).flatMap(_.headOption).getOrElse(default).pure[F])
      case Sum(FormCtx(fc)) =>
        if (isHidden(fcId, visSet)) MaybeConvertible(defaultF)
        else sum(visSet, fcId, fc, dataLookup, retrievals, formTemplate)
      case Sum(_)              => NonConvertible(nothingF)
      case Add(f1, f2)         => Converted(makeCalc(visSet, fcId, dataLookup, _ + _, f1, f2, retrievals, formTemplate))
      case Subtraction(f1, f2) => Converted(makeCalc(visSet, fcId, dataLookup, _ - _, f1, f2, retrievals, formTemplate))
      case Multiply(f1, f2)    => Converted(makeCalc(visSet, fcId, dataLookup, _ * _, f1, f2, retrievals, formTemplate))
    }

  private def sum(
    visSet: Set[GraphNode],
    fcId: FormComponentId,
    fc: String,
    dataLookup: Data,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier) = {
    val results = dataLookup.collect {
      case (key, value) if key.value.endsWith(fc) => Constant(value.headOption.getOrElse(""))
    }
    val summation = results.foldLeft(Expr.additionIdentityExpr)(Add)
    eval(visSet, fcId, summation, dataLookup, retrievals, formTemplate)
  }

  def isHidden(fcId: FormComponentId, visSet: Set[GraphNode]): Boolean =
    visSet.exists {
      case SimpleGN(fcId_)       => fcId === fcId_
      case IncludeIfGN(fcId_, _) => fcId === fcId_
    }

  private def makeCalc(
    visSet: Set[GraphNode],
    fcId: FormComponentId,
    dataLookup: Data,
    operator: (BigDecimal, BigDecimal) => BigDecimal,
    xExpr: Expr,
    yExpr: Expr,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate
  )(implicit hc: HeaderCarrier): F[Computable] = {
    def calc(expr: Expr): Convertible[F] =
      eval(visSet, fcId, expr, dataLookup, retrievals, formTemplate)
    (calc(xExpr), calc(yExpr)) match {
      case (IsConverted(maybeConvLeftF), IsConverted(maybeConvRightF)) =>
        for {
          maybeConvLeft  <- maybeConvLeftF
          maybeConvRight <- maybeConvRightF
        } yield {
          (maybeConvLeft, maybeConvRight) match {
            case (Some(l), Some(r)) => Computed(operator(l, r))
            case (_, _)             => NonComputable
          }
        }

      case (_, _) => (NonComputable: Computable).pure[F]
    }
  }
}

sealed trait Computable
case class Computed(x: BigDecimal) extends Computable
case object NonComputable extends Computable

sealed trait Convertible[F[_]]

object Convertible {
  def asString[F[_]: Functor](convertible: Convertible[F]): F[String] = convertible match {
    case Converted(bigDecimal) => bigDecimal.map(NumberFormatUtil.defaultFormat.format)
    case MaybeConvertible(str) => str
    case NonConvertible(str)   => str
  }

  def round[F[_]: Monad](convertible: Convertible[F], scale: Option[Int]): F[String] = convertible match {
    case IsConverted(eff) =>
      eff.flatMap {
        case None => Convertible.asString(convertible)
        case Some(bigDecimal) =>
          scale
            .fold(bigDecimal)(s => bigDecimal.setScale(s, RoundingMode.FLOOR))
            .pure[F]
            .map(NumberFormatUtil.defaultFormat.format)
      }
    case _ => Convertible.asString(convertible)
  }
}

case class NonConvertible[F[_]](str: F[String]) extends Convertible[F]
case class MaybeConvertible[F[_]](str: F[String]) extends Convertible[F]
case class Converted[F[_]](computable: F[Computable]) extends Convertible[F]

object IsConverted {
  def unapply[F[_]: Functor](convertible: Convertible[F]): Option[F[Option[BigDecimal]]] =
    convertible match {
      case Converted(bigDecimal) => Some(bigDecimal.map { case NonComputable => None; case Computed(bd) => Some(bd) })
      case MaybeConvertible(str) => Some(str.map(BigDecimalUtil.toBigDecimalSafe))
      case NonConvertible(_)     => None
    }
}
