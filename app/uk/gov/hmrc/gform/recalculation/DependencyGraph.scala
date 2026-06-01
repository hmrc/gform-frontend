/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.recalculation

import cats.data.NonEmptyList
import cats.syntax.all._
import play.api.i18n.Messages
import scala.collection.mutable
import scalax.collection.OneOrMore
import scalax.collection.immutable.Graph
import scalax.collection.hyperedges.multilabeled.LDiHyperEdge
import uk.gov.hmrc.gform.commons.BigDecimalUtil
import uk.gov.hmrc.gform.controllers.AuthCacheWithoutForm
import uk.gov.hmrc.gform.eval.{ ExprType, StaticTypeData }
import uk.gov.hmrc.gform.gform.ExprUpdater
import uk.gov.hmrc.gform.models.ExpandUtils
import uk.gov.hmrc.gform.models.ids.{ IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData
import uk.gov.hmrc.gform.sharedmodel.{ VariadicFormData, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.form.VisitIndex
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

sealed trait RelationKind

object RelationKind {
  case class Condition(includeIf: IncludeIf) extends RelationKind
  case class IncludeOptionIf(includeIf: IncludeIf, expr: Expr) extends RelationKind
  case class Value(expr: Expr, section: Section) extends RelationKind
  case class Repeats(expr: Expr) extends RelationKind
  case class LinkOnly() extends RelationKind
}

case class Relation(
  formComponentIds: OneOrMore[FormComponentId],
  dependencies: OneOrMore[FormComponentId],
  label: RelationKind
) extends LDiHyperEdge[FormComponentId, RelationKind](
      sources = formComponentIds,
      targets = dependencies
    ) //with MultiEdge

class DependencyGraph(val graph: Graph[FormComponentId, Relation]) {
  def nodes: graph.NodeSetT = graph.nodes
  def edges: graph.EdgeSetT = graph.edges
  def find(formComponentId: FormComponentId): Option[graph.NodeT] = graph.find(formComponentId)

  def pretty(): String = {
    val nodes = graph.nodes.toList.map(_.toString).sorted
    val edges = graph.edges.toList.map(_.toString).sorted

    List(
      List(s"graph : $graph"),
      List(s"nodes (${nodes.size}):"),
      nodes.map("  " + _),
      List(s"edges (${edges.size}):"),
      edges.map("  " + _)
    ).flatten.mkString("\n")
  }
}

// Read Only! This represent user data.
class MongoUserData(
  val lookup: VariadicFormData
) {

  def cleared(modelComponentIds: List[ModelComponentId]): Unit = {
    lookup -- modelComponentIds
    ()
  }

  def getEvalStatus(fcId: ModelComponentId, metadata: Metadata): EvaluationStatus =
    lookup
      .get(fcId)
      .map { variadicValue =>
        val baseComponentId = fcId.baseComponentId

        val fc: StaticTypeData = metadata.staticTypeInfo(baseComponentId)

        fc.exprType match {
          case ExprType.Number =>
            val v = variadicValue.toOne.value.trim
            if (v.isEmpty) {
              EvaluationStatus.Empty
            } else {
              BigDecimalUtil.toBigDecimalOrString(v)
            }
          case ExprType.String          => EvaluationStatus.StringResult(variadicValue.toOne.value)
          case ExprType.ChoiceSelection => EvaluationStatus.OptionResult(variadicValue.toMany.value)
          case ExprType.Date =>
            if (variadicValue.isEmpty) {
              EvaluationStatus.Empty
            } else {
              EvaluationStatus.NumberResult(BigDecimal(variadicValue.toOne.value.toInt))
            }
          case _ => EvaluationStatus.StringResult(variadicValue.toOne.value)
        }
      }
      .getOrElse(EvaluationStatus.Empty)
}

object MongoUserData {

  // For tests only
  def apply(answers: (String, VariadicValue)*): MongoUserData =
    new MongoUserData(
      VariadicFormData(
        mutable.Map(answers: _*).map { case (key, value) =>
          val runtimeKey = ExpandUtils.toModelComponentId(key)
          runtimeKey -> value
        }
      )
    )
}

class Lifter(
  val modelComponentId: ModelComponentId, // Needed to get correct DataRetrieve in ATL
  runtime: Runtime,
  iterationNumber: Option[Int]
) {
  def toRuntimeValue(
    metadata: Metadata,
    formComponentId: FormComponentId
  ): Either[ModelComponentId, NonEmptyList[ModelComponentId]] =
    iterationNumber match {
      case Some(iterationNumber) => toRuntimeValueRepeated(metadata, formComponentId, iterationNumber)
      case _                     => toRuntimeValueStandard(metadata, formComponentId)
    }

  private def toRuntimeValueStandard(
    metadata: Metadata,
    formComponentId: FormComponentId
  ): Either[ModelComponentId, NonEmptyList[ModelComponentId]] = {
    val baseComponentId = formComponentId.baseComponentId
    metadata.refInfo(baseComponentId) match {
      case RefInfo.NonRepeatingPage(_) =>
        Left(ModelComponentId.Pure(IndexedComponentId.Pure(baseComponentId)))
      case RefInfo.RepeatingPage(_) | RefInfo.AddToListPage(_) | RefInfo.TaskListRepeatingPage(_, _) |
          RefInfo.TaskListAddToListPage(_, _) =>
        val indices: List[Int] = runtime.cache.get(formComponentId).toList.flatten.flatMap(_.maybeIndex).distinct.sorted

        Right(
          NonEmptyList
            .fromList(indices.map(index => ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, index))))
            .getOrElse(
              NonEmptyList.of(
                ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, 1))
              )
            )
        )

      case RefInfo.TaskListNonRepeatingPage(_, _) =>
        Left(ModelComponentId.Pure(IndexedComponentId.Pure(baseComponentId)))
    }
  }

  private def toRuntimeValueRepeated(
    metadata: Metadata,
    formComponentId: FormComponentId,
    iterationNumber: Int
  ): Either[ModelComponentId, NonEmptyList[ModelComponentId]] = {
    val baseComponentId = formComponentId.baseComponentId
    val modelId = metadata.refInfo(baseComponentId) match {
      case RefInfo.NonRepeatingPage(_) => ModelComponentId.Pure(IndexedComponentId.Pure(baseComponentId))
      case RefInfo.RepeatingPage(_) =>
        ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, iterationNumber))
      case RefInfo.AddToListPage(_) =>
        ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, iterationNumber))
      case RefInfo.TaskListNonRepeatingPage(_, _) => ModelComponentId.Pure(IndexedComponentId.Pure(baseComponentId))
      case RefInfo.TaskListRepeatingPage(_, _) =>
        ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, iterationNumber))
      case RefInfo.TaskListAddToListPage(_, _) =>
        ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, iterationNumber))
    }

    Left(modelId)
  }
}

object Lifter {
  def getLifter(modelComponentId: ModelComponentId, runtime: Runtime): Lifter =
    new Lifter(modelComponentId, runtime, modelComponentId.indexedComponentId.maybeIndex)
}

object Runtime {
  def apply(visitIndex: VisitIndex, mongoUserData: MongoUserData, metadata: Metadata): Runtime =
    new Runtime(visitIndex, mongoUserData, metadata, mutable.Map[FormComponentId, Set[ModelComponentId]]())
}

class Runtime(
  visitIndex: VisitIndex,
  mongoUserData: MongoUserData,
  metadata: Metadata,
  val cache: mutable.Map[FormComponentId, Set[ModelComponentId]]
) {

  private def compute(formComponentId: FormComponentId): Set[ModelComponentId] =
    if (formComponentId == FormComponentId.root || formComponentId == FormComponentId.leaf) {
      Set.empty[ModelComponentId]
    } else if (metadata.isGroupComponent(formComponentId) || metadata.isGroup(formComponentId.baseComponentId)) {
      // Group components need special handling, since they are repeated fields on non-repeated page

      val fcIdAtomic = metadata.isAtomic(formComponentId.baseComponentId)

      mongoUserData.lookup.collect {
        case (key, _)
            if key.baseComponentId == formComponentId.baseComponentId &&
              (
                // TODO it would be good to not store atomic field without atom into mongo, then we wouldn't need this.
                // Note - comment out this and run the tests to see why is this needed.
                (fcIdAtomic && key.isAtomic()) || (!fcIdAtomic && !key.isAtomic())
              ) =>
          key
      }.toSet
    } else {
      val baseComponentId = formComponentId.baseComponentId

      val refInfo: RefInfo = metadata.refInfo(baseComponentId)

      val classics: Set[SectionNumber.Classic] = refInfo.getVisitIndex(visitIndex)
      val modelComponentIds: Set[ModelComponentId] = if (classics.isEmpty) {

        // At the start of the journey there are no visited pages, but
        // we want to recalculate expressions anyway.
        val rfcId = refInfo match {
          case RefInfo.NonRepeatingPage(_)            => ModelComponentId.Pure(IndexedComponentId.Pure(baseComponentId))
          case RefInfo.RepeatingPage(_)               => ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, 1))
          case RefInfo.AddToListPage(_)               => ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, 1))
          case RefInfo.TaskListNonRepeatingPage(_, _) => ModelComponentId.Pure(IndexedComponentId.Pure(baseComponentId))
          case RefInfo.TaskListRepeatingPage(_, _) =>
            ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, 1))
          case RefInfo.TaskListAddToListPage(_, _) =>
            ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, 1))
        }
        Set(rfcId)
      } else {
        // classics.toList.sorted.collect <- do not try to sort this, sort it elsewhere, we need to eliminate duplicates here!!!
        classics.collect {
          case SectionNumber.Classic.NormalPage(_) => ModelComponentId.Pure(IndexedComponentId.Pure(baseComponentId))
          case SectionNumber.Classic.RepeatedPage(_, pageNumber) =>
            ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, pageNumber + 1))
          case SectionNumber.Classic.AddToListPage.Page(_, iterationNumber, _) =>
            ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, iterationNumber))
          case SectionNumber.Classic.AddToListPage.TerminalPage(_, iterationNumber, _) =>
            ModelComponentId.Pure(IndexedComponentId.Indexed(baseComponentId, iterationNumber))
        }
      }

      if (metadata.isAtomic(baseComponentId)) {
        modelComponentIds.toList.flatMap { modelComponentId =>
          metadata.atomicsLookup(formComponentId)(modelComponentId.indexedComponentId).toList
        }.toSet
      } else {
        modelComponentIds
      }
    }

  def toIndexedFormComponentIds(formComponentId: FormComponentId): Set[ModelComponentId] =
    cache.getOrElseUpdate(formComponentId, compute(formComponentId))
}

class Recalculator(
  graph: DependencyGraph,
  mongoUserData: MongoUserData,
  val metadata: Metadata,
  runtime: Runtime,
  val answerMap: AnswerMap,
  var evaluationContext: EvaluationContext,
  val cacheBuster: CacheBuster
)(implicit messages: Messages) {

  def updateThirdPartyData(thirdPartyData: ThirdPartyData): Unit =
    evaluationContext = evaluationContext.copy(
      thirdPartyData = thirdPartyData
    )

  def markForRecalculation(modelComponentIds: List[ModelComponentId]): Unit =
    answerMap.markForRecalculation(modelComponentIds)

  def recalculate(): Unit =
    graph.nodes.foreach { node =>
      evalNode(node)
    }

  // FormModel doesn't exists at this moment, so FormModelMetadata are not available.
  // This is ok because links in general should not contribute to visibility calculation.
  // FormModelMetadata of FreeCalculator will be made available by calling withFormModelMetadata method
  val formModelMetadata = FormModelMetadata.notAvailable

  def mkFreeCalculator(): FreeCalculator = {
    recalculate()
    new FreeCalculator(
      this,
      formModelMetadata
    )
  }

  val answerMapWithFallback = AnswerMapWithFallback(
    answerMap,
    mongoUserData,
    metadata.staticTypeInfo
  )

  private def needsToBeEvaluated(modelComponentId: ModelComponentId): Boolean =
    answerMap(modelComponentId) == EvaluationStatus.Dirty

  private def evalNode(node: graph.graph.NodeT)(implicit messages: Messages): Unit = {
    val formComponentId: FormComponentId = node.outer

    val repeatedFcIds = runtime.toIndexedFormComponentIds(formComponentId)

    repeatedFcIds.foreach { modelComponentId =>
      if (needsToBeEvaluated(modelComponentId)) {
        if (node.incoming.isEmpty) {
          // Node is not depending on anything
          val answer = mongoUserData.getEvalStatus(modelComponentId, metadata)
          answerMap.addOne(modelComponentId -> answer)
        } else {
          val relations: Set[Relation] = node.incoming.map(_.outer)

          // Before evaluation this node, we evaluate everything this nodes depends on
          relations.flatMap(_.formComponentIds.toList).foreach { fcId =>
            // Eval only when a dependent component is not a current component
            if (fcId.modelComponentId.baseComponentId =!= modelComponentId.baseComponentId) {
              val node = graph.nodes.get(fcId)
              evalNode(node)
            }
          }

          val calculator = BootstrapCalculator(
            runtime,
            answerMap,
            metadata,
            evaluationContext,
            formModelMetadata,
            Lifter.getLifter(modelComponentId, runtime),
            cacheBuster
          )

          val labels: Set[RelationKind] = relations.map(_.label)

          val isVisibleByRepeatExpr: Boolean =
            labels.collect { case RelationKind.Repeats(expr) => expr }.forall { expr =>
              calculator
                .evalExpr(expr)
                .evaluationStatus match { // I expected that I will need visitIndex to detect visible repeated pages, but result of expr seems enough!
                case EvaluationStatus.StringResult(value) =>
                  modelComponentId.indexedComponentId match {
                    case IndexedComponentId.Pure(_)           => throw new Exception("Expected repeated modelComponentId")
                    case IndexedComponentId.Indexed(_, index) => index <= value.toInt
                  }
                case EvaluationStatus.NumberResult(value) =>
                  modelComponentId.indexedComponentId match {
                    case IndexedComponentId.Pure(_)           => throw new Exception("Expected repeated modelComponentId")
                    case IndexedComponentId.Indexed(_, index) => index <= value
                  }
                case _ => true
              }
            }

          val includeIfs: Set[IncludeIf] = labels
            .collect { case RelationKind.Condition(includeIf) =>
              includeIf
            }

          val isVisible: Boolean = includeIfs.forall { includeIf =>
            calculator.evalBooleanExpr(includeIf.booleanExpr)
          }

          if (isVisible && isVisibleByRepeatExpr) {

            val computedValueExpr: Option[Expr] = labels
              .collectFirst { case RelationKind.Value(expr, section) =>
                modelComponentId.maybeIndex.fold(expr) { index =>
                  val baseIds = section.maybeAddToList.fold(List.empty[FormComponentId])(_.allIds)
                  ExprUpdater(expr, index, baseIds)
                }
              }

            val answer = computedValueExpr match {
              case Some(expr) =>
                calculator.evalExpr(expr).evaluationStatus
              case None =>
                mongoUserData.getEvalStatus(modelComponentId, metadata) match {
                  case EvaluationStatus.OptionResult(values) =>
                    val includeOptionsIf: Set[RelationKind.IncludeOptionIf] = labels
                      .collect { case r: RelationKind.IncludeOptionIf => r }

                    val selectedButInvisible: Set[String] = includeOptionsIf.flatMap { includeOptionIf =>
                      if (calculator.evalBooleanExpr(includeOptionIf.includeIf.booleanExpr)) {
                        None
                      } else {
                        val evalRes = calculator.evalExpr(includeOptionIf.expr)

                        Some(evalRes.stringRepresentation)
                      }
                    }
                    val onlySelected = values.filterNot(selectedButInvisible)
                    if (onlySelected.isEmpty) {
                      EvaluationStatus.Empty
                    } else
                      EvaluationStatus.OptionResult(onlySelected)
                  case other => other
                }
            }

            answerMap.addOne(modelComponentId -> answer)

          } else {
            answerMap.addOne(modelComponentId -> EvaluationStatus.Hidden)
          }
        }
      }
    }
  }
}

sealed trait RefInfo {

  def isRepeatedField() = this match {
    case RefInfo.NonRepeatingPage(_)            => false
    case RefInfo.RepeatingPage(_)               => true
    case RefInfo.AddToListPage(_)               => true
    case RefInfo.TaskListNonRepeatingPage(_, _) => false
    case RefInfo.TaskListRepeatingPage(_, _)    => true
    case RefInfo.TaskListAddToListPage(_, _)    => true
  }

  def getVisitIndex(visitIndex: VisitIndex): Set[SectionNumber.Classic] = this match {
    case RefInfo.NonRepeatingPage(index) =>
      visitIndex match {
        case VisitIndex.Classic(visitIndex) =>
          visitIndex.filter { classic =>
            classic.sectionIndex === index
          }
        case VisitIndex.TaskList(_) => Set.empty
      }
    case RefInfo.RepeatingPage(index) =>
      visitIndex match {
        case VisitIndex.Classic(visitIndex) =>
          visitIndex.filter { classic =>
            classic.sectionIndex === index
          }
        case VisitIndex.TaskList(_) => Set.empty
      }
    case RefInfo.AddToListPage(index) =>
      visitIndex match {
        case VisitIndex.Classic(visitIndex) =>
          visitIndex.filter { classic =>
            classic match {
              case SectionNumber.Classic.AddToListPage.Page(sectionIndex, _, _) => sectionIndex === index
              case _                                                            => false
            }
          }
        case VisitIndex.TaskList(_) => Set.empty
      }

    case RefInfo.TaskListNonRepeatingPage(coordinates1, index) =>
      visitIndex match {
        case VisitIndex.Classic(_) => Set.empty
        case VisitIndex.TaskList(visitIndex) =>
          visitIndex.flatMap { case (coordinates2, values) =>
            values.filter { classic =>
              classic.sectionIndex === index && coordinates1 == coordinates2
            }
          }.toSet
      }
    case RefInfo.TaskListRepeatingPage(coordinates1, index) =>
      visitIndex match {
        case VisitIndex.Classic(_) => Set.empty
        case VisitIndex.TaskList(visitIndex) =>
          visitIndex.flatMap { case (coordinates2, values) =>
            values.filter { classic =>
              classic match {
                case SectionNumber.Classic.RepeatedPage(sectionIndex, _) =>
                  sectionIndex === index && coordinates1 == coordinates2
                case _ => false
              }
            }
          }.toSet
      }
    case RefInfo.TaskListAddToListPage(coordinates1, index) =>
      visitIndex match {
        case VisitIndex.Classic(_) => Set.empty
        case VisitIndex.TaskList(visitIndex) =>
          visitIndex.flatMap { case (coordinates2, values) =>
            values.filter { classic =>
              classic match {
                case SectionNumber.Classic.AddToListPage.Page(sectionIndex, _, _) =>
                  sectionIndex === index && coordinates1 == coordinates2
                case _ => false
              }
            }
          }.toSet
      }
  }
}

object RefInfo {

  final case class NonRepeatingPage(index: TemplateSectionIndex) extends RefInfo
  final case class RepeatingPage(index: TemplateSectionIndex) extends RefInfo
  final case class AddToListPage(index: TemplateSectionIndex) extends RefInfo
  final case class TaskListNonRepeatingPage(coordinate: Coordinates, index: TemplateSectionIndex) extends RefInfo
  final case class TaskListRepeatingPage(coordinates: Coordinates, index: TemplateSectionIndex) extends RefInfo
  final case class TaskListAddToListPage(coordinates: Coordinates, index: TemplateSectionIndex) extends RefInfo

}

object Recalculator {
  def from(
    formTemplate: FormTemplate,
    metadata: Metadata,
    mongoUserData: MongoUserData,
    visitIndex: VisitIndex,
    evaluationContext: EvaluationContext,
    cacheBuster: CacheBuster
  )(implicit messages: Messages): Recalculator = {
    val graph = DependencyGraph.toGraph(formTemplate, metadata)

    val dependencyGraph = new DependencyGraph(graph)
    val runtime = Runtime(visitIndex, mongoUserData, metadata)
    val answerMap = AnswerMap.initialAnwerMap(graph, runtime)
    new Recalculator(
      dependencyGraph,
      mongoUserData,
      metadata,
      runtime,
      answerMap,
      evaluationContext,
      cacheBuster
    )
  }

  def fromEnrolmentSection(enrolmentSection: EnrolmentSection, cache: AuthCacheWithoutForm, cacheBuster: CacheBuster)(
    implicit messages: Messages
  ): Recalculator = {
    val graph = Graph.empty[FormComponentId, Relation]
    val dependencyGraph = new DependencyGraph(graph)
    val metadata = Metadata.fromEnrolmentSection(enrolmentSection)
    val mongoUserData = new MongoUserData(VariadicFormData.empty)
    val visitIndex = VisitIndex.empty
    val runtime = Runtime(visitIndex, mongoUserData, metadata)
    val answerMap = AnswerMap.initialAnwerMap(graph, runtime)
    val evaluationContext = EvaluationContext.from(cache)
    new Recalculator(
      dependencyGraph,
      mongoUserData,
      metadata,
      runtime,
      answerMap,
      evaluationContext,
      cacheBuster
    )
  }
}

object DependencyGraph {

  private def toRelation(
    parentsList: List[FormComponentId],
    childrenList: List[FormComponentId],
    relationKind: RelationKind
  ): Option[Relation] =
    OneOrMore.from(childrenList).map { children =>
      val parents = OneOrMore.from(parentsList).getOrElse(OneOrMore.one(FormComponentId.root))

      Relation(parents, children, relationKind) // Parents control visibility of children

    }

  def toGraph(formTemplate: FormTemplate, metadata: Metadata): Graph[FormComponentId, Relation] = {
    val sections: List[Section] = formTemplate.formKind.allSections.sections.map(_.section)

    val addToLists: List[Section.AddToList] = sections.flatMap { section =>
      section.maybeAddToList
    }

    val addToListRelations: List[Relation] = addToLists.flatMap { addToList =>
      addToList.includeIf.fold(List.empty[Relation]) { includeIf =>
        val includeIfRefs: List[FormComponentId] = includeIfReferences(includeIf)

        val formComponentIds: List[FormComponentId] =
          addToList.pages.toList.flatMap(_.allEnterableFormComponents.map(_.id))

        List(
          toRelation(includeIfRefs, List(addToList.addAnotherQuestion.id), RelationKind.Condition(includeIf)),
          toRelation(
            List(addToList.addAnotherQuestion.id),
            formComponentIds,
            RelationKind.Condition(includeIf)
          )
        ).flatten
      }
    }

    val repeatingSectionEdges: List[Relation] = formTemplate.formKind.allSections.sections.flatMap { section =>
      section.section.fold[Option[Relation]](_ => Option.empty[Relation]) { repeatingPage =>
        val repeatsRefs: List[FormComponentId] = exprReferences(repeatingPage.repeats)

        val fieldIds: List[FormComponentId] = repeatingPage.page.fields.filter(_.isEnterableFormComponent).map(_.id)

        toRelation(repeatsRefs, fieldIds, RelationKind.Repeats(repeatingPage.repeats))

      }(_ => Option.empty[Relation])
    }

    val taskEdges: List[Relation] = formTemplate.formKind.fold(classic => List.empty[Relation]) { taskList =>
      taskList.sections.toList.flatMap(_.tasks.toList).flatMap { task =>
        task.includeIf
          .fold(Option.empty[Relation]) { includeIf =>
            val includeIfRefs: List[FormComponentId] = includeIfReferences(includeIf)

            val formComponentIds: List[FormComponentId] =
              task.sections.toList.flatMap(_.allPages).flatMap(_.allEnterableFormComponents.map(_.id))

            toRelation(includeIfRefs, formComponentIds, RelationKind.Condition(includeIf))
          }
          .toList
      }
    }

    graphFromPages(sections, repeatingSectionEdges, taskEdges, addToListRelations, metadata)
  }

  private def toPageRelations(page: Page): Option[Relation] =
    page.includeIf.flatMap { includeIf =>
      val includeIfRefs: List[FormComponentId] = includeIfReferences(includeIf)

      val enterableFieldIds: List[FormComponentId] = page.allEnterableFormComponents.map(_.id)

      val fieldIds =
        if (enterableFieldIds.isEmpty) {
          List(FormComponentId.leaf)
        } else {
          enterableFieldIds
        }
      toRelation(includeIfRefs, fieldIds, RelationKind.Condition(includeIf))

    }

  private def toValueRefs(formComponent: FormComponent, section: Section, metadata: Metadata): Option[Relation] =
    formComponent match {
      case IsText(text) =>
        text.value match {
          case Value => None
          case expr =>
            val parents = OneOrMore.from(exprReferences(text.value)).getOrElse(OneOrMore.one(FormComponentId.root))

            val maybeExplicitExprType: Option[ExplicitExprType] =
              metadata.staticTypeInfo.get(formComponent.baseComponentId).map { staticTypData =>
                staticTypData.textConstraint match {
                  case Some(WholeSterling(_, roundingMode)) => ExplicitExprType.WholeSterling(roundingMode)
                  case Some(Sterling(roundingMode, _))      => ExplicitExprType.Sterling(roundingMode)
                  case Some(IsPositiveNumberOrNumber(maxFractionalDigits, roundingMode, _)) =>
                    ExplicitExprType.Number(maxFractionalDigits, roundingMode)
                  case _ => ExplicitExprType.Text
                }
              }

            val expr2 = maybeExplicitExprType.fold(expr)(explicitExprType => Typed(expr, explicitExprType))

            Some(
              Relation(parents, OneOrMore.one(formComponent.id), RelationKind.Value(expr2, section))
            )
        }
      // Prepopulation of address and overseas address is handled on page rendering not here
      case _ => None
    }

  private def toFieldRelations(field: FormComponent): Option[Relation] =
    field.includeIf.map { includeIf =>
      val includeIfRefs: List[FormComponentId] = includeIfReferences(includeIf)

      val incIfRefs = OneOrMore.from(includeIfRefs).getOrElse(OneOrMore.one(FormComponentId.root))

      Relation(incIfRefs, OneOrMore.one(field.id), RelationKind.Condition(includeIf))
    }

  private def revealingFieldRelation(
    fieldId: FormComponentId,
    expr: Expr,
    fcIds: List[FormComponentId]
  ): Option[Relation] =
    OneOrMore.from(fcIds).map { revealingFieldsIds =>
      Relation(
        OneOrMore.one(fieldId),
        revealingFieldsIds,
        RelationKind.Condition(IncludeIf(Contains(FormCtx(fieldId), expr)))
      )
    }

  private def optionDataIncludeIfRelation(
    fieldId: FormComponentId,
    choice: OptionData
  ): Option[Relation] =
    choice.includeIf.flatMap { includeIf =>
      val includeIfRefs: List[FormComponentId] = includeIfReferences(includeIf)
      OneOrMore
        .from(includeIfRefs)
        .map { refs =>
          Relation(
            refs,
            OneOrMore.one(fieldId),
            RelationKind.LinkOnly()
          )
        }
    }

  private def revealingFieldOptionRelation(
    fieldId: FormComponentId,
    choice: OptionData,
    expr: Expr
  ): Option[Relation] =
    choice.includeIf.flatMap { includeIf =>
      val includeIfRefs: List[FormComponentId] = includeIfReferences(includeIf)
      OneOrMore
        .from(includeIfRefs)
        .map { refs =>
          Relation(
            refs,
            OneOrMore.one(fieldId),
            RelationKind.IncludeOptionIf(includeIf, expr)
          )
        }
    }

  private def toRevealingChoiceRelations(field: FormComponent): List[Relation] = field match {
    case IsChoice(choice) =>
      choice.options.flatMap { optionData =>
        optionData.dynamic.toList.flatMap { dynamic =>
          dynamic match {
            case Dynamic.ATLBased(formComponentId) =>
              List(
                Relation(
                  OneOrMore.one(formComponentId),
                  OneOrMore.one(field.id),
                  RelationKind.LinkOnly()
                )
              )
            case Dynamic.DataRetrieveBased(_) => List.empty[Relation]
          }
        } ++
          optionDataIncludeIfRelation(field.id, optionData)
      }

    case IsRevealingChoice(revealingChoice) =>
      revealingChoice.options.zipWithIndex.toList.flatMap { case (revealingChoiceElement, index) =>
        val fcIds: List[FormComponentId] = revealingChoiceElement.allEnterableFormComponents.map(_.id)

        val contains: List[Relation] = revealingChoiceElement.choice match {
          case ib: OptionData.IndexBased =>
            val expr = Constant(index.toString)
            revealingFieldOptionRelation(field.id, revealingChoiceElement.choice, expr).toList ++
              revealingFieldRelation(field.id, expr, fcIds).toList
          case vb: OptionData.ValueBased =>
            vb.value match {
              case OptionDataValue.StringBased(value) =>
                val expr = Constant(value)
                revealingFieldOptionRelation(
                  field.id,
                  revealingChoiceElement.choice,
                  expr
                ).toList ++ revealingFieldRelation(field.id, expr, fcIds).toList

              case OptionDataValue.ExprBased(expr) =>
                val valueRefs = exprReferences(expr)
                revealingFieldOptionRelation(
                  field.id,
                  revealingChoiceElement.choice,
                  expr
                ).toList ++
                  revealingFieldRelation(field.id, expr, fcIds).toList ++
                  OneOrMore
                    .from(valueRefs)
                    .map { refs =>
                      Relation(
                        refs,
                        OneOrMore.one(field.id),
                        RelationKind.LinkOnly()
                      )
                    }
                    .toList
            }
        }
        contains
      }
    case _ => List.empty[Relation]
  }

  def graphFromPages(
    sections: List[Section],
    repeatingSectionEdges: List[Relation],
    taskEdges: List[Relation],
    addToListRelations: List[Relation],
    metadata: Metadata
  ): Graph[FormComponentId, Relation] = {
    val edges: List[Relation] = sections.flatMap { section =>
      section.allPages.flatMap { page =>
        val fieldsRelations: List[Relation] = page.allEnterableFormComponents.flatMap { field =>
          val fieldRelation: Option[Relation] = toFieldRelations(field)
          val valueRelation: Option[Relation] = toValueRefs(field, section, metadata)
          val revealingChoiceRelation: List[Relation] = toRevealingChoiceRelations(field)
          fieldRelation ++ valueRelation ++ revealingChoiceRelation
        }
        val pageRelations: Option[Relation] = toPageRelations(page)

        fieldsRelations ++ pageRelations ++ repeatingSectionEdges ++ taskEdges ++ addToListRelations
      }
    }

    Graph.from(nodes = Nil, edges = edges)

  }

  def includeIfReferences(includeIf: IncludeIf): List[FormComponentId] = booleanExprReferences(includeIf.booleanExpr)

  def booleanExprReferences(booleanExpr: BooleanExpr): List[FormComponentId] =
    booleanExpr.allExpressions.flatMap(exprReferences)

  def exprReferences(expr: Expr): List[FormComponentId] =
    expr.allFormComponentIds()

}
