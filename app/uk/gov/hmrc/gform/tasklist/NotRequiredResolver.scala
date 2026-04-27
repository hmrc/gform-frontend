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
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, Task }

final class NotRequiredResolver(
  formModelVisibilityOptics: FormModelVisibilityOptics,
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
  def create(
    formModelVisibilityOptics: FormModelVisibilityOptics,
    taskCoordinatesLookup: Map[Task, Coordinates]
  ): NotRequiredResolver = {
    val notRequiredIfEvalLookup: Set[Coordinates] = taskCoordinatesLookup.collect {
      case (task, coordinates)
          if task.notRequiredIf.fold(false)(formModelVisibilityOptics.evalIncludeIfExpr(_, None)) =>
        coordinates
    }.toSet
    new NotRequiredResolver(formModelVisibilityOptics, notRequiredIfEvalLookup)
  }
}
