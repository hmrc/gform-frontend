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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.gform.models.{ AllSections, Coordinates }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils._
import uk.gov.hmrc.gform.sharedmodel.SmartString

sealed trait FormKind extends Product with Serializable {
  def fold[B](f: FormKind.Classic => B)(g: FormKind.TaskList => B): B =
    this match {
      case n: FormKind.Classic  => f(n)
      case r: FormKind.TaskList => g(r)
    }

  def foldNested[B](f: NonEmptyList[TaskSection] => B)(g: List[Section] => B): B =
    fold(classic => g(classic.sections))(taskList => f(taskList.sections))

  val allSections: AllSections = fold[AllSections] { classic =>
    AllSections.Classic(classic.sections)
  } { taskList =>
    AllSections.TaskList {
      taskList.sections.zipWithIndex.flatMap { case (taskSection, taskSectionIndex) =>
        taskSection.tasks.zipWithIndex.map { case (task, taskIndex) =>
          Coordinates(TaskSectionNumber(taskSectionIndex), TaskNumber(taskIndex)) -> task.sections.toList
        }
      }
    }
  }

}

object FormKind {

  final case class Classic(sections: List[Section]) extends FormKind

  object Classic {
    implicit val format: OFormat[Classic] = derived.oformat()
  }

  final case class TaskList(sections: NonEmptyList[TaskSection]) extends FormKind

  object TaskList {
    implicit val format: OFormat[TaskList] = derived.oformat()
  }

  implicit val format: OFormat[FormKind] = derived.oformat()

}

final case class TaskSection(
  title: SmartString,
  tasks: NonEmptyList[Task]
) {
  val sections: NonEmptyList[Section] = tasks.flatMap(_.sections)
}
object TaskSection {
  implicit val format: OFormat[TaskSection] = derived.oformat()
}

final case class Task(
  title: SmartString,
  sections: NonEmptyList[Section],
  summarySection: Option[SummarySection]
)

object Task {
  implicit val format: OFormat[Task] = derived.oformat()
}
