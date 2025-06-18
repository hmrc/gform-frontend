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
            if (
              notRequiredIfEvalLookup(coordinates) || formModelVisibilityOptics
                .allEditableFormComponentsForCoordinates(coordinates)
                .isEmpty
            ) {

              TaskStatus.NotRequired
            } else otherwise
        }
      coordinates -> finalStatus
    }

}

object NotRequiredResolver {
  def create[D <: DataOrigin](
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    taskCoordinatesLookup: Map[Task, Coordinates]
  ): NotRequiredResolver[D] = {
    def onDemandEvalIncludeIf(includeIf: IncludeIf) = {
      val fm = formModelVisibilityOptics.formModel
      fm.onDemandIncludeIf.forall(f => f(includeIf))
    }
    val notRequiredIfEvalLookup: Set[Coordinates] = taskCoordinatesLookup.collect {
      case (task, coordinates) if task.notRequiredIf.fold(false)(onDemandEvalIncludeIf) =>
        coordinates
    }.toSet
    new NotRequiredResolver(formModelVisibilityOptics, notRequiredIfEvalLookup)
  }
}
