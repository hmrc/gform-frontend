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

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, IncludeIf, Task }

final class NotRequiredResolver[D <: DataOrigin](
  formModelVisibilityOptics: FormModelVisibilityOptics[D],
  notRequiredIfEvalLookup: Set[Coordinates]
) {
  def resolveNotRequired(
    initialStatuses: NonEmptyList[(Coordinates, TaskStatus)]
  ): NonEmptyList[(Coordinates, TaskStatus)] =
    initialStatuses.map { case (coordinates, taskStatus) =>
      val finalStatus =
        taskStatus match {
          case TaskStatus.CannotStartYet => TaskStatus.CannotStartYet
          case otherwise =>
            def allEditableFormComponentsForCoordinates = {
              val fm = formModelVisibilityOptics.formModel
              val availableFormComponents = fm.taskList
                .availablePages(coordinates)
                .filter { page =>
                  page.getIncludeIf.forall { includeIf =>
                    NotRequiredResolver.onDemandEvalIncludeIf(formModelVisibilityOptics, includeIf)
                  }
                }
                .flatMap(_.allFormComponents)

              !availableFormComponents.exists(_.editable)
            }

            if (notRequiredIfEvalLookup(coordinates) || allEditableFormComponentsForCoordinates) {
              TaskStatus.NotRequired
            } else otherwise
        }
      coordinates -> finalStatus
    }

}

object NotRequiredResolver {
  def onDemandEvalIncludeIf(formModelVisibilityOptics: FormModelVisibilityOptics[_], includeIf: IncludeIf): Boolean = {
    val fm = formModelVisibilityOptics.formModel
    fm.onDemandIncludeIf.forall(f => f(includeIf))
  }
  def create[D <: DataOrigin](
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    taskCoordinatesLookup: Map[Task, Coordinates]
  ): NotRequiredResolver[D] = {

    val notRequiredIfEvalLookup: Set[Coordinates] = taskCoordinatesLookup.collect {
      case (task, coordinates) if task.notRequiredIf.fold(false)(onDemandEvalIncludeIf(formModelVisibilityOptics, _)) =>
        coordinates
    }.toSet
    new NotRequiredResolver(formModelVisibilityOptics, notRequiredIfEvalLookup)
  }
}
