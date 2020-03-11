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

package uk.gov.hmrc.gform.graph

import cats.{ Applicative, Id, Monad, MonadError }
import cats.syntax.eq._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.instances.string._
import cats.data.EitherT

import scala.language.higherKinds
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.commons.{ BigDecimalUtil, NumberFormatUtil }
import uk.gov.hmrc.gform.eval.BooleanExprEval
import uk.gov.hmrc.gform.gform.AuthContextPrepop
import uk.gov.hmrc.gform.graph.processor.UserCtxEvaluatorProcessor
import uk.gov.hmrc.gform.sharedmodel.{ IdNumberValue, RecalculatedTaxPeriodKey, SubmissionRef, VariadicFormData, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormDataRecalculated, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.{ DependencyGraph, GraphNode, IncludeIfGN, SimpleGN }
import uk.gov.hmrc.gform.models.helpers.FormComponentHelper.extractMaxFractionalDigits
import uk.gov.hmrc.http.HeaderCarrier

sealed trait RecalculationOp extends Product with Serializable

object RecalculationOp {
  val setEmpty: RecalculationOp = SetEmpty
  val noChange: RecalculationOp = NoChange
  def newValue(value: String): RecalculationOp = NewValue(value)
}

case object SetEmpty extends RecalculationOp
case object NoChange extends RecalculationOp
case class NewValue(value: String) extends RecalculationOp

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

  type Context = (Set[GraphNode], RecData)
  type ContextWithNodesToRecalculate = (Set[GraphNode], List[FormComponentId])

  def recalculateFormData(
    data: VariadicFormData,
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier, me: MonadError[F, E]): F[FormDataRecalculated] =
    recalculateFormDataWithLookup(
      data,
      formTemplate,
      retrievals,
      thirdPartyData,
      envelopeId,
      Map.empty
    )

  def recalculateFormDataWithLookup(
    data: VariadicFormData,
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    envelopeId: EnvelopeId,
    additionalFcLookup: Map[FormComponentId, FormComponent])(
    implicit hc: HeaderCarrier,
    me: MonadError[F, E]): F[FormDataRecalculated] =
    recalculateFormData_(data, formTemplate, retrievals, thirdPartyData, envelopeId, additionalFcLookup).value.flatMap {
      case Left(graphException) => me.raiseError(error(graphException))
      case Right(fd)            => fd.pure[F]
    }

  private def recalculateFormData_(
    data: VariadicFormData,
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    envelopeId: EnvelopeId,
    additionalFcLookup: Map[FormComponentId, FormComponent])(
    implicit hc: HeaderCarrier): EitherT[F, GraphException, FormDataRecalculated] = {

    val graph: Graph[GraphNode, DiEdge] = DependencyGraph.toGraph(formTemplate, data)

    val fcLookup = formTemplate.expandFormTemplate(data).formComponentsLookup(data) ++ additionalFcLookup

    val revealingChoices: List[(FormComponent, RevealingChoice)] = fcLookup.values.toList.collect {
      case fc @ IsRevealingChoice(rc) => (fc, rc)
    }

    val reverseRevealingChoiceLookup: Map[FormComponentId, (FormComponent, RevealingChoice)] = revealingChoices
      .map {
        case (fc, rc) =>
          rc.options.toList.flatMap(_.revealingFields).map(rcFc => (rcFc.id, (fc, rc))).toMap
      }
      .foldLeft(Map.empty[FormComponentId, (FormComponent, RevealingChoice)])(_ ++ _)

    val orderedGraph: Either[GraphException, Traversable[(Int, List[GraphNode])]] = DependencyGraph
      .constructDependencyGraph(graph)
      .leftMap(node => NoTopologicalOrder(node.toOuter, graph))

    val orderedGraphT: EitherT[F, GraphException, Traversable[(Int, List[GraphNode])]] = EitherT(orderedGraph.pure[F])

    def isHiddenOnRevealingChoice(fcId: FormComponentId): Boolean =
      fcLookup.get(fcId).fold(false) { fc =>
        reverseRevealingChoiceLookup.get(fc.id).fold(false) {
          case (fcRc, rc) =>
            !RevealingChoice.slice(fcRc.id)(data)(rc).map(_.id).contains(fcId)
        }
      }

    /**
      * Adds invisible nodes to invisibility set and returns nodes to recalculate for current Graph layer
      */
    def markInvisible(
      acc: ContextWithNodesToRecalculate,
      dataLookup: RecData,
      gn: GraphNode): F[ContextWithNodesToRecalculate] = {
      val (visibilitySet, nodesToRecalculate) = acc
      gn match {
        case s @ SimpleGN(fcId) =>
          if (booleanExprEval.evaluator.isHidden(fcId, visibilitySet) || isHiddenOnRevealingChoice(fcId)) {
            (visibilitySet ++ Set(s), fcId :: nodesToRecalculate).pure[F]
          } else
            (visibilitySet, fcId :: nodesToRecalculate).pure[F]
        case n @ IncludeIfGN(_, includeIf) =>
          val isSectionVisible: F[Boolean] =
            booleanExprEval
              .isTrue(
                includeIf.expr,
                dataLookup.data,
                retrievals,
                visibilitySet,
                thirdPartyData,
                envelopeId,
                formTemplate)
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

    def recalculateGraphLayer(graphLayer: List[GraphNode], ctx: Context): EitherT[F, GraphException, Context] = {
      val (visibilitySet, dataLookup) = ctx

      val extendVisibility: EitherT[F, GraphException, ContextWithNodesToRecalculate] = {
        val visNodes: F[ContextWithNodesToRecalculate] =
          graphLayer.foldLeft((visibilitySet, List.empty[FormComponentId]).pure[F]) {
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
        nodes: List[FormComponentId]): EitherT[F, GraphException, RecData] = {

        val genesis: EitherT[F, GraphException, RecData] = EitherT(dataLookup.pure[F].map(Right.apply))

        nodes.foldLeft(genesis) {
          case (dataLookupF, node) =>
            for {
              dataLookupUpd <- dataLookupF
              recalculated <- EitherT(
                               calculate(
                                 visSet,
                                 node,
                                 fcLookup,
                                 dataLookupUpd,
                                 retrievals,
                                 formTemplate,
                                 thirdPartyData,
                                 envelopeId))
            } yield recalculated
        }
      }

      for {
        visAndNodes <- extendVisibility
        (visSet, nodesToRecalculate) = visAndNodes
        recalculatedData <- recalculateLayerNodes(visSet, nodesToRecalculate)
      } yield (visSet, recalculatedData)
    }

    val data2 = data.mapValues {
      case (fcId, value) =>
        fcLookup.get(fcId) match {
          case Some(fc) =>
            fc match {
              case IsCapitalised() => value.map(_.toUpperCase())
              case _               => value
            }
          case _ => value
        }
    }

    val contextE: Either[GraphException, Context] =
      Right((Set.empty, RecData.fromData(data2)))

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

  private def hasData(fc: FormComponent, dataLookup: VariadicFormData): Boolean =
    dataLookup.get(fc.id).fold(false)(!_.exists(_.isEmpty))

  private def isHmrcTaxPeriodComponent(fc: FormComponent): Boolean = fc match {
    case IsHmrcTaxPeriod(_) => true
    case _                  => false
  }

  private def calculate(
    visSet: Set[GraphNode],
    fcId: FormComponentId,
    fcLookup: Map[FormComponentId, FormComponent],
    dataLookup: RecData,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    thirdPartyData: ThirdPartyData,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): F[Either[GraphException, RecData]] =
    Either.fromOption(fcLookup.get(fcId), NoFormComponent(fcId, fcLookup)).traverse { fc =>
      if (fc.editable && (hasData(fc, dataLookup.data) && !isHmrcTaxPeriodComponent(fc)))
        dataLookup.pure[F]
      else
        fc match {
          case IsHmrcTaxPeriod(taxPeriod) =>
            recalculate(fc, visSet, dataLookup, retrievals, formTemplate, thirdPartyData, envelopeId).map {
              case SetEmpty => dataLookup
              case NoChange => dataLookup
              case NewValue(value) =>
                dataLookup.copy(recalculatedTaxPeriod = dataLookup.recalculatedTaxPeriod + (RecalculatedTaxPeriodKey(
                  fc.id,
                  taxPeriod) -> IdNumberValue(value)))
            }
          case IsText(_) | IsTextArea(_) =>
            recalculate(fc, visSet, dataLookup, retrievals, formTemplate, thirdPartyData, envelopeId).map {
              case SetEmpty        => dataLookup.copy(data = dataLookup.data addOne (fcId -> ""))
              case NoChange        => dataLookup
              case NewValue(value) => dataLookup.copy(data = dataLookup.data addOne (fcId -> value))
            }
          case _ => dataLookup.pure[F] // Nothing to recompute on non-text components
        }
    }

  private def recalculate(
    fc: FormComponent,
    visSet: Set[GraphNode],
    dataLookup: RecData,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    thirdPartyData: ThirdPartyData,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): F[RecalculationOp] =
    fc match {
      case HasExpr(SingleExpr(expr)) =>
        val conv: Convertible[F] =
          booleanExprEval.evaluator
            .eval(visSet, fc.id, expr, dataLookup.data, retrievals, formTemplate, thirdPartyData, envelopeId)
        val maxFractionDigitsAndRoundingMode = extractMaxFractionalDigits(fc)

        Convertible.round(
          conv,
          maxFractionDigitsAndRoundingMode.maxDigits,
          maxFractionDigitsAndRoundingMode.roundingMode,
          formTemplate)
      case _ => RecalculationOp.noChange.pure[F]
    }
}

class Evaluator[F[_]: Monad](
  val eeittPrepop: (Eeitt, MaterialisedRetrievals, FormTemplate, HeaderCarrier) => F[String]
) {

  def defaultF[T[_]: Applicative]: T[String] = "0".pure[T]

  private def evalRosm(thirdPartyData: ThirdPartyData, rosmProp: RosmProp): RecalculationOp = {
    val f = thirdPartyData.desRegistrationResponse.fold(RecalculationOp.setEmpty) _
    rosmProp match {
      case RosmSafeId           => f(a => NewValue(a.safeId))
      case RosmOrganisationName => f(a => NewValue(a.orgOrInd.getOrganisationName))
      case RosmOrganisationType => f(a => NewValue(a.orgOrInd.getOrganisationType))
      case RosmIsAGroup         => f(a => NewValue(a.orgOrInd.getIsAGroup))
    }
  }

  def evalFormCtx(visSet: Set[GraphNode], fc: FormCtx, dataLookup: VariadicFormData): Convertible[F] =
    if (isHidden(fc.toFieldId, visSet)) MaybeConvertibleHidden(defaultF, fc.toFieldId)
    else getSubmissionData(dataLookup, fc.toFieldId)

  def evalVariadicFormCtx(visSet: Set[GraphNode], fc: FormCtx, dataLookup: VariadicFormData): Option[VariadicValue] =
    if (isHidden(fc.toFieldId, visSet)) None
    else dataLookup.get(fc.toFieldId)

  def eval(
    visSet: Set[GraphNode],
    fcId: FormComponentId,
    expr: Expr,
    dataLookup: VariadicFormData,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    thirdPartyData: ThirdPartyData,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Convertible[F] =
    expr match {
      case Value                               => getSubmissionData(dataLookup, fcId)
      case HmrcRosmRegistrationCheck(rosmProp) => NonConvertible(evalRosm(thirdPartyData, rosmProp).pure[F])
      case ctx @ UserCtx(_) =>
        new UserCtxEvaluatorProcessor[F].processEvaluation(retrievals, ctx, formTemplate.authConfig)
      case AuthCtx(value) =>
        NonConvertible(RecalculationOp.newValue(AuthContextPrepop.values(value, retrievals)).pure[F])
      case EeittCtx(eeitt) =>
        NonConvertible(eeittPrepop(eeitt, retrievals, formTemplate, hc).map(RecalculationOp.newValue))
      case FormTemplateCtx(FormTemplateProp.SubmissionReference) =>
        NonConvertible(RecalculationOp.newValue(SubmissionRef(envelopeId).toString).pure[F])
      case FormTemplateCtx(FormTemplateProp.Id) =>
        NonConvertible(RecalculationOp.newValue(formTemplate._id.value).pure[F])
      case Constant(fc) => MaybeConvertible(fc.pure[F])
      case fc: FormCtx  => evalFormCtx(visSet, fc, dataLookup)
      case Sum(fc: FormCtx) =>
        if (isHidden(fc.toFieldId, visSet)) MaybeConvertibleHidden(defaultF, fc.toFieldId)
        else sum(visSet, fcId, fc.toFieldId, dataLookup, retrievals, formTemplate, thirdPartyData, envelopeId)
      case Sum(_) => NonConvertible(RecalculationOp.noChange.pure[F])
      case Else(f1, f2) =>
        val f: Expr => Convertible[F] =
          eval(visSet, fcId, _, dataLookup, retrievals, formTemplate, thirdPartyData, envelopeId)
        Convertible.orElse(f(f1), f(f2))

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
            thirdPartyData,
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
            thirdPartyData,
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
            thirdPartyData,
            envelopeId))
    }

  def evalAsString(
    recalculated: FormDataRecalculated,
    fcId: FormComponentId,
    expr: Expr,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    thirdPartyData: ThirdPartyData,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): F[Option[String]] =
    Convertible
      .asString(
        eval(
          recalculated.invisible,
          fcId,
          expr,
          recalculated.recData.data,
          retrievals,
          formTemplate,
          thirdPartyData,
          envelopeId),
        formTemplate
      )
      .map(_.flatMap(_.cast[NewValue]).map(_.value))

  private def getSubmissionData(dataLookup: VariadicFormData, fcId: FormComponentId): Convertible[F] =
    dataLookup.get(fcId).toList.flatMap(_.toSeq).headOption match {
      case None        => Converted((NonComputable: Computable).pure[F])
      case Some(value) => MaybeConvertible(value.pure[F])
    }

  private def sum(
    visSet: Set[GraphNode],
    componentContainingSum: FormComponentId,
    componentToBeSummed: FormComponentId,
    dataLookup: VariadicFormData,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    thirdPartyData: ThirdPartyData,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier) = {
    val results = dataLookup.collect {
      case (key, value: VariadicValue.One) if key.reduceToTemplateFieldId === componentToBeSummed =>
        Constant(value.value)
    }
    val summation = results.foldLeft(Expr.additionIdentityExpr)(Add)
    eval(visSet, componentContainingSum, summation, dataLookup, retrievals, formTemplate, thirdPartyData, envelopeId)
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
    dataLookup: VariadicFormData,
    xExpr: Expr,
    yExpr: Expr,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    f: (Convertible[F], Convertible[F]) => (Option[BigDecimal], Option[BigDecimal]) => F[A],
    thirdPartyData: ThirdPartyData,
    envelopeId: EnvelopeId
  )(implicit hc: HeaderCarrier): F[A] = {
    def calc(expr: Expr): Convertible[F] =
      eval(visSet, fcId, expr, dataLookup, retrievals, formTemplate, thirdPartyData, envelopeId)

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

sealed trait Convertible[F[_]] extends Product with Serializable

case class Nested[F[_]](convertible: F[Convertible[F]]) extends Convertible[F]
case class Converted[F[_]](computable: F[Computable]) extends Convertible[F]
case class NonConvertible[F[_]](str: F[RecalculationOp]) extends Convertible[F]
case class MaybeConvertible[F[_]](str: F[String]) extends Convertible[F]
case class MaybeConvertibleHidden[F[_]: Applicative](str: F[String], fcId: FormComponentId) extends Convertible[F] {
  def visible[A](formTemplate: FormTemplate, f: String => Option[A]): F[Option[A]] = {
    val lookup = formTemplate.expandFormTemplateFull.formComponentsLookupFull
    val maybeFc: Option[FormComponent] = lookup.get(fcId).filter {
      case IsChoice(_) => false
      case _           => true
    }
    val fNone: F[Option[A]] = Option.empty[A].pure[F]
    maybeFc.fold(fNone)(Function.const(str.map(f)))
  }
}

object Convertible {
  def asString[F[_]: Monad](convertible: Convertible[F], formTemplate: FormTemplate): F[Option[RecalculationOp]] =
    convertible match {
      case Converted(computable) =>
        computable.map {
          case NonComputable => Some(RecalculationOp.noChange)
          case Computed(bd)  => Some(NewValue(NumberFormatUtil.defaultFormat.format(bd)))
        }
      case MaybeConvertible(str)            => str.map(a => Some(RecalculationOp.newValue(a)))
      case m @ MaybeConvertibleHidden(_, _) => m.visible(formTemplate, a => Some(RecalculationOp.newValue(a)))
      case NonConvertible(str)              => str.map(Some.apply)
      case Nested(convertible)              => convertible.flatMap(a => asString(a, formTemplate))
    }

  def orElse[F[_]: Monad](leftConvertible: Convertible[F], rightConvertible: Convertible[F]): Convertible[F] =
    leftConvertible match {
      case Converted(computable) =>
        Nested(computable.map {
          case NonComputable => rightConvertible
          case Computed(bd)  => leftConvertible
        })
      case MaybeConvertible(str) =>
        Nested(str.map { s =>
          if (s.isEmpty) rightConvertible else leftConvertible
        })
      case MaybeConvertibleHidden(str, _) =>
        Nested(str.map { s =>
          if (s.isEmpty || s === "0") rightConvertible else leftConvertible // 0 is special case in MaybeConvertibleHidden denoting field is hidden
        })
      case NonConvertible(str) =>
        Nested(str.map {
          case NewValue("") => rightConvertible
          case NoChange     => rightConvertible
          case _            => leftConvertible
        })
      case n @ Nested(convertible) => n
    }

  def convert[F[_]: Monad](convertible: Convertible[F], formTemplate: FormTemplate): F[Option[BigDecimal]] =
    convertible match {
      case Converted(computable) =>
        computable.map { case NonComputable => None; case Computed(bd) => Some(bd) }
      case MaybeConvertible(str)            => str.map(BigDecimalUtil.toBigDecimalSafe)
      case m @ MaybeConvertibleHidden(_, _) => m.visible(formTemplate, BigDecimalUtil.toBigDecimalSafe)
      case NonConvertible(_)                => Option.empty[BigDecimal].pure[F]
      case Nested(convertible)              => convertible.flatMap(a => convert(a, formTemplate))
    }

  def round[F[_]: Monad](
    convertible: Convertible[F],
    scale: Int,
    roundingMode: RoundingMode,
    formTemplate: FormTemplate): F[RecalculationOp] =
    convert(convertible, formTemplate).flatMap {
      case Some(bd) =>
        RecalculationOp.newValue(NumberFormatUtil.roundAndFormat(bd, scale, roundingMode)).pure[F]
      case None => Convertible.asString(convertible, formTemplate).map(_.getOrElse(RecalculationOp.noChange))
    }
}
