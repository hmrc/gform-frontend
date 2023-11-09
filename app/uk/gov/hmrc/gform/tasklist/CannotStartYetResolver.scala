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

package uk.gov.hmrc.gform.tasklist

import cats.implicits._
import cats.data.NonEmptyList
import uk.gov.hmrc.gform.models.{ BracketsWithSectionNumber, DataExpanded, FormModel }
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, Expr, FormCtx }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AddressLens

final class CannotStartYetResolver(
  // Return FormComponentIds which are defined in given task
  formComponentIdsLookup: Map[Coordinates, Set[BaseComponentId]],
  // Return FormComponentIds which are referred to in given task
  dependingFcIdLookup: Map[Coordinates, Set[BaseComponentId]]
) {
  // What task given BaseComponentId belongs to.
  private val fcIdToTaskMapping: Map[BaseComponentId, Coordinates] = formComponentIdsLookup.flatMap {
    case (coordinate, formComponentIds) =>
      formComponentIds.map { formComponentId =>
        formComponentId -> coordinate
      }.toMap
  }

  private val dependingTaskLookup: Map[Coordinates, Set[Coordinates]] = dependingFcIdLookup.map {
    case (coordinates, fcIds) =>
      val dependentCoordinates: Set[Coordinates] = fcIds.map { fcId =>
        fcIdToTaskMapping.getOrElse(fcId, throw new Exception(s"Not found task where formComponent $fcId is defined"))
      }
      coordinates -> dependentCoordinates
  }

  def resolveCannotStartYet(
    initialStatuses: NonEmptyList[(Coordinates, TaskStatus)]
  ): NonEmptyList[(Coordinates, TaskStatus)] = {
    val statusLookup: Map[Coordinates, TaskStatus] = initialStatuses.toList.toMap

    val finalStatuses: NonEmptyList[(Coordinates, TaskStatus)] = initialStatuses.map { case (coordinates, status) =>
      val canStart = dependingTaskLookup
        .getOrElse(coordinates, Set.empty[Coordinates])
        .forall { coordinates =>
          val initialStatus = statusLookup.getOrElse(
            coordinates,
            throw new Exception(s"Not found taskStatus for Coordinates $coordinates")
          )
          initialStatus === TaskStatus.Completed || initialStatus === TaskStatus.NotRequired
        }
      val statusUpd = if (canStart) status else TaskStatus.CannotStartYet
      coordinates -> statusUpd
    }

    if (initialStatuses === finalStatuses)
      initialStatuses
    else
      resolveCannotStartYet(finalStatuses) // Task can form a chain of dependencies, so we need to go link by link

  }

}

object CannotStartYetResolver {
  def create(formModel: FormModel[DataExpanded]): CannotStartYetResolver = {
    val taskList: BracketsWithSectionNumber.TaskList[DataExpanded] = formModel.brackets.unsafeToTaskList
    val formComponentIdsLookup: Map[Coordinates, Set[BaseComponentId]] = taskList.brackets
      .map { case (coordinates, brackets) =>
        coordinates -> brackets.toBracketsList.flatMap { bracket =>
          bracket.toPageModel.toList.flatMap(pageModel => pageModel.allFormComponents.map(_.id.baseComponentId))

        }.toSet
      }
      .toList
      .toMap

    val dependingFcIdLookup: Map[Coordinates, Set[BaseComponentId]] = taskList.brackets.toList.map {
      case (coordinates, brackets) =>
        val allExprs: List[Expr] =
          brackets.toBracketsList.flatMap(_.toPlainBracket.allExprs(formModel))

        val baseComponentIds: List[BaseComponentId] =
          allExprs.flatMap(_.leafs(formModel)).collect {
            case FormCtx(fcId)        => fcId.baseComponentId
            case AddressLens(fcId, _) => fcId.baseComponentId
          }

        val ownBaseComponentIds = formComponentIdsLookup.getOrElse(coordinates, Set.empty[BaseComponentId])

        val foreignBaseComponentIds = baseComponentIds.filterNot(ownBaseComponentIds)

        coordinates -> foreignBaseComponentIds.toSet
    }.toMap

    new CannotStartYetResolver(
      formComponentIdsLookup,
      dependingFcIdLookup
    )
  }
}
