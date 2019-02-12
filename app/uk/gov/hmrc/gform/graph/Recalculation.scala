/*
 * Copyright 2019 HM Revenue & Customs
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

import cats.{ Applicative, Monad, MonadError }
import cats.syntax.eq._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.traverse._
import cats.data.EitherT
import java.text.NumberFormat
import java.util.Locale

import scala.language.higherKinds
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, AuthenticatedRetrievals, MaterialisedRetrievals }
import uk.gov.hmrc.gform.commons.{ BigDecimalUtil, NumberFormatUtil }
import uk.gov.hmrc.gform.gform.AuthContextPrepop
import uk.gov.hmrc.gform.models.ExpandUtils
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormDataRecalculated }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.{ DependencyGraph, GraphNode, IncludeIfGN, SimpleGN }
import uk.gov.hmrc.gform.models.helpers.FormComponentHelper.extractMaxFractionalDigits
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil._
import uk.gov.hmrc.gform.sharedmodel.SubmissionReferenceUtil
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

  def recalculateFormData(
    data: Data,
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier, me: MonadError[F, E]): F[FormDataRecalculated] =
    recalculateFormData_(data, formTemplate, retrievals, envelopeId).value.flatMap {
      case Left(graphException) => me.raiseError(error(graphException))
      case Right(fd)            => fd.pure[F]
    }

  private def recalculateFormData_(
    data: Data,
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): EitherT[F, GraphException, FormDataRecalculated] = {

    val graph: Graph[GraphNode, DiEdge] = DependencyGraph.toGraph(formTemplate, data)

    val fcLookup: Map[FormComponentId, FormComponent] = formTemplate.expandFormTemplate(data).fcsLookup(data)

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
              recalculated <- EitherT(
                               calculate(visSet, node, fcLookup, dataLookupUpd, retrievals, formTemplate, envelopeId))
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
    formTemplate: FormTemplate,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): F[Either[GraphException, Data]] =
    Either.fromOption(fcLookup.get(fcId), NoFormComponent(fcId, fcLookup)).traverse { fc =>
      if ((fc.editable || fc.derived) && hasData(fc, dataLookup)) dataLookup.pure[F]
      else
        fc match {
          case IsText(_) | IsTextArea(_) =>
            recalculate(fc, visSet, dataLookup, retrievals, formTemplate, envelopeId).map { recalculatedValue =>
              if (recalculatedValue.isEmpty) dataLookup
              else dataLookup + (fcId -> Seq(recalculatedValue))
            }
          case _ => dataLookup.pure[F] // Nothing to recompute on non-text components
        }
    }

  private def recalculate(
    fc: FormComponent,
    visSet: Set[GraphNode],
    dataLookup: Data,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): F[String] =
    fc match {
      case HasExpr(SingleExpr(expr)) =>
        val conv: Convertible[F] =
          booleanExprEval.evaluator.eval(visSet, fc.id, expr, dataLookup, retrievals, formTemplate, envelopeId)
        val maxFractionDigitsAndRoundingMode = extractMaxFractionalDigits(fc)

        Convertible.round(
          conv,
          maxFractionDigitsAndRoundingMode.maxDigits,
          maxFractionDigitsAndRoundingMode.roundingMode,
          formTemplate)
      case _ => "".pure[F]
    }
}

class Evaluator[F[_]: Monad](
  val eeittPrepop: (Eeitt, MaterialisedRetrievals, FormTemplate, HeaderCarrier) => F[String]
) {

  val defaultF = "0".pure[F]
  val nothingF = "".pure[F]

  def eval(
    visSet: Set[GraphNode],
    fcId: FormComponentId,
    expr: Expr,
    dataLookup: Data,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Convertible[F] =
    expr match {
      case Value => getSubmissionData(dataLookup, fcId)
      case UserCtx(Enrolment(ServiceName(sn), IdentifierName(in))) =>
        NonConvertible {
          retrievals match {
            case AnonymousRetrievals(_) => "".pure[F]
            case AuthenticatedRetrievals(_, enrolments, _, _, _, _, _, _) =>
              enrolments.getEnrolment(sn).flatMap(_.getIdentifier(in)).map(_.value).getOrElse("").pure[F]
          }
        }
      case UserCtx(_)          => NonConvertible(affinityGroupNameO(AffinityGroupUtil.fromRetrievals(retrievals)).pure[F])
      case AuthCtx(value)      => NonConvertible(AuthContextPrepop.values(value, retrievals).pure[F])
      case EeittCtx(eeitt)     => NonConvertible(eeittPrepop(eeitt, retrievals, formTemplate, hc))
      case SubmissionReference => NonConvertible(SubmissionReferenceUtil.getSubmissionReference(envelopeId).pure[F])
      case Constant(fc)        => MaybeConvertible(fc.pure[F])
      case fc @ FormCtx(_) =>
        if (isHidden(fc.toFieldId, visSet)) MaybeConvertibleHidden(defaultF, fc.toFieldId)
        else getSubmissionData(dataLookup, fc.toFieldId)
      case Sum(fc @ FormCtx(value)) =>
        if (isHidden(fc.toFieldId, visSet)) MaybeConvertibleHidden(defaultF, fc.toFieldId)
        else sum(visSet, fcId, value, dataLookup, retrievals, formTemplate, envelopeId)
      case Sum(_) => NonConvertible(nothingF)
      case Add(f1, f2) =>
        Converted(
          makeCalc(
            visSet,
            fcId,
            dataLookup,
            f1,
            f2,
            retrievals,
            formTemplate,
            (_, _) => doComputation(_ + _),
            envelopeId))
      case Subtraction(f1, f2) =>
        Converted(
          makeCalc(
            visSet,
            fcId,
            dataLookup,
            f1,
            f2,
            retrievals,
            formTemplate,
            (_, _) => doComputation(_ - _),
            envelopeId))
      case Multiply(f1, f2) =>
        Converted(
          makeCalc(
            visSet,
            fcId,
            dataLookup,
            f1,
            f2,
            retrievals,
            formTemplate,
            (_, _) => doComputation(_ * _),
            envelopeId))
    }

  private def getSubmissionData(dataLookup: Data, fcId: FormComponentId): Convertible[F] =
    dataLookup.get(fcId).flatMap(_.headOption) match {
      case None        => Converted((NonComputable: Computable).pure[F])
      case Some(value) => MaybeConvertible(value.pure[F])
    }

  private def sum(
    visSet: Set[GraphNode],
    fcId: FormComponentId,
    fc: String,
    dataLookup: Data,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier) = {
    val results = dataLookup.collect {
      case (key, value) if key.value.endsWith(fc) => Constant(value.headOption.getOrElse(""))
    }
    val summation = results.foldLeft(Expr.additionIdentityExpr)(Add)
    eval(visSet, fcId, summation, dataLookup, retrievals, formTemplate, envelopeId)
  }

  def isHidden(fcId: FormComponentId, visSet: Set[GraphNode]): Boolean =
    visSet.exists {
      case SimpleGN(fcId_)       => fcId === fcId_
      case IncludeIfGN(fcId_, _) => fcId === fcId_
    }

  private def doComputation(operator: (BigDecimal, BigDecimal) => BigDecimal)(
    maybeBigDecimalA: Option[BigDecimal],
    maybeBigDecimalB: Option[BigDecimal]): F[Computable] =
    ((maybeBigDecimalA, maybeBigDecimalB) match {
      case (Some(bdA), Some(bdB)) => Computed(operator(bdA, bdB))
      case (_, _)                 => NonComputable
    }).pure[F]

  def makeCalc[A](
    visSet: Set[GraphNode],
    fcId: FormComponentId,
    dataLookup: Data,
    xExpr: Expr,
    yExpr: Expr,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    f: (Convertible[F], Convertible[F]) => (Option[BigDecimal], Option[BigDecimal]) => F[A],
    envelopeId: EnvelopeId
  )(implicit hc: HeaderCarrier): F[A] = {
    def calc(expr: Expr): Convertible[F] =
      eval(visSet, fcId, expr, dataLookup, retrievals, formTemplate, envelopeId)

    val x = calc(xExpr)
    val y = calc(yExpr)

    for {
      maybeBigDecimalA <- Convertible.convert(x, formTemplate)
      maybeBigDecimalB <- Convertible.convert(y, formTemplate)
      a                <- f(x, y)(maybeBigDecimalA, maybeBigDecimalB)
    } yield a

  }
}

sealed trait Computable extends Product with Serializable
case class Computed(x: BigDecimal) extends Computable
case object NonComputable extends Computable

sealed trait Convertible[F[_]]

object Convertible {
  def asString[F[_]: Applicative](convertible: Convertible[F], formTemplate: FormTemplate): F[Option[String]] =
    convertible match {
      case Converted(computable) =>
        computable.map {
          case NonComputable => Some("")
          case Computed(bd)  => Some(NumberFormatUtil.defaultFormat.format(bd))
        }
      case MaybeConvertible(str)            => str.map(Some.apply)
      case m @ MaybeConvertibleHidden(_, _) => m.visible(formTemplate, Some.apply)
      case NonConvertible(str)              => str.map(Some.apply)
    }

  def convert[F[_]: Applicative](convertible: Convertible[F], formTemplate: FormTemplate): F[Option[BigDecimal]] =
    convertible match {
      case Converted(computable) =>
        computable.map { case NonComputable => None; case Computed(bd) => Some(bd) }
      case MaybeConvertible(str)            => str.map(BigDecimalUtil.toBigDecimalSafe)
      case m @ MaybeConvertibleHidden(_, _) => m.visible(formTemplate, BigDecimalUtil.toBigDecimalSafe)
      case NonConvertible(_)                => Option.empty.pure[F]
    }

  def round[F[_]: Monad](
    convertible: Convertible[F],
    scale: Int,
    roundingMode: RoundingMode,
    formTemplate: FormTemplate): F[String] =
    convert(convertible, formTemplate).flatMap {
      case Some(bd) =>
        NumberFormatUtil.roundAndFormat(bd, scale, roundingMode).pure[F]
      case None => Convertible.asString(convertible, formTemplate).map(_.getOrElse(""))
    }
}

case class Converted[F[_]](computable: F[Computable]) extends Convertible[F]
case class NonConvertible[F[_]](str: F[String]) extends Convertible[F]
case class MaybeConvertible[F[_]](str: F[String]) extends Convertible[F]
case class MaybeConvertibleHidden[F[_]: Applicative](str: F[String], fcId: FormComponentId) extends Convertible[F] {
  def visible[A](formTemplate: FormTemplate, f: String => Option[A]): F[Option[A]] = {
    val lookup = formTemplate.expandFormTemplateFull.fcsLookupFull
    val maybeFc: Option[FormComponent] = lookup.get(fcId).filter {
      case IsChoice(_) => false
      case _           => true
    }
    val fNone: F[Option[A]] = (None: Option[A]).pure[F]
    maybeFc.fold(fNone)(Function.const(str.map(f)))
  }
}
