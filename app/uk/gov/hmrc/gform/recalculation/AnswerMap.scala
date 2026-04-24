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

import cats.syntax.all._
import scala.collection.mutable
import scalax.collection.immutable.Graph
import uk.gov.hmrc.gform.models.ExpandUtils
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

// Warning: AnswerMap contains only ModelComponentId which are dependent on other ModelComponentId.
class AnswerMap(val answerMap: mutable.Map[ModelComponentId, EvaluationStatus]) {

  def cleared(modelComponentIds: List[ModelComponentId]): Unit =
    modelComponentIds.foreach(modelComponentId => answerMap.remove(modelComponentId))

  def apply(formComponentId: ModelComponentId): EvaluationStatus =
    answerMap
      .get(formComponentId)
      .getOrElse(
        EvaluationStatus.Empty
      ) // This getOrElse is here to handle form changes on the fly. Like adding new field which some of the old fields depends on.

  def get(formComponentId: ModelComponentId): Option[EvaluationStatus] = answerMap.get(formComponentId)

  def addOne(tuple: (ModelComponentId, EvaluationStatus)): Unit =
    answerMap.addOne(tuple)
  //addOneTraced(tuple)

  def isManaging(modelComponentId: ModelComponentId): Boolean =
    modelComponentId.fold { pure =>
      pure.indexedComponentId.fold { pure =>
        answerMap.keys.exists(_.baseComponentId === pure.baseComponentId)
      } { indexed =>
        answerMap.isDefinedAt(modelComponentId)
      }
    } { atomic =>
      atomic.indexedComponentId.fold { pure =>
        answerMap.keys.exists(_.baseComponentId === pure.baseComponentId)
      } { indexed =>
        answerMap.isDefinedAt(modelComponentId)
      }
    }

  @scala.annotation.nowarn
  private def addOneTraced(tuple: (ModelComponentId, EvaluationStatus)): Unit = {
    val before = pretty()
    answerMap.addOne(tuple)
    val after = pretty()
    println("Updating answerMap " + tuple)
    println("before:")
    println("  " + before.replace("\n", "\n  "))
    println("after:")
    println("  " + after.replace("\n", "\n  "))
  }

  def pretty(): String =
    answerMap.toList.sortBy { case (k, v) => k.toMongoIdentifier }.mkString("\n")

  def prettyPrint(): Unit = println(pretty())

  override def toString(): String =
    answerMap.toString

  def forBaseComponentIdLessThanEqual(
    modelComponentId: ModelComponentId
  ): List[(ModelComponentId, EvaluationStatus)] =
    modelComponentId.indexedComponentId.fold(pure => forBaseComponentId(pure.baseComponentId)) { indexed =>
      answerMap.collect {
        case (mcId, value)
            if mcId.baseComponentId === indexed.baseComponentId &&
              mcId.maybeIndex.exists(_ <= indexed.index) =>
          mcId -> value
      }.toList
    }

  def forBaseComponentIdLessThan(
    modelComponentId: ModelComponentId
  ): List[(ModelComponentId, EvaluationStatus)] =
    modelComponentId.indexedComponentId.fold(pure => forBaseComponentId(pure.baseComponentId)) { indexed =>
      answerMap.collect {
        case (mcId, value)
            if mcId.baseComponentId === indexed.baseComponentId &&
              mcId.maybeIndex.exists(_ < indexed.index) =>
          mcId -> value
      }.toList
    }

  def forBaseComponentId(baseComponentId: BaseComponentId): List[(ModelComponentId, EvaluationStatus)] =
    answerMap
      .collect {
        case (modelComponentId, value) if modelComponentId.baseComponentId === baseComponentId =>
          modelComponentId -> value
      }
      .toList
      .sortBy(_._1.maybeIndex)
}

object AnswerMap {

  def initialAnwerMap(
    graph: Graph[FormComponentId, Relation],
    runtime: Runtime
  ): AnswerMap = {
    val answerMap = mutable.Map[ModelComponentId, EvaluationStatus]()
    graph.nodes.foreach { formComponentId =>
      if (formComponentId.outer != FormComponentId.root) {
        val expandedFcIds = runtime.toIndexedFormComponentIds(formComponentId)
        expandedFcIds.foreach { formComponentId =>
          answerMap.addOne(formComponentId -> EvaluationStatus.Dirty)
        }
      }
    }
    new AnswerMap(answerMap)
  }

  // For tests only
  def apply(exprs: (String, EvaluationStatus)*): AnswerMap = {
    val answerMap = mutable.Map[ModelComponentId, EvaluationStatus]()
    answerMap.addAll(exprs.map { case (key, value) =>
      val runtimeKey = ExpandUtils.toModelComponentId(key)
      runtimeKey -> value
    })
    new AnswerMap(answerMap)
  }
}
