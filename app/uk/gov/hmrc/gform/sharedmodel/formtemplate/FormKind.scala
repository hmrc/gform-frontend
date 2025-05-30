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
import uk.gov.hmrc.gform.models.{ AllSections, IndexedSection }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils._
import uk.gov.hmrc.gform.sharedmodel.SmartString
import com.softwaremill.quicklens._

sealed trait FormKind extends Product with Serializable {
  def fold[B](f: FormKind.Classic => B)(g: FormKind.TaskList => B): B =
    this match {
      case n: FormKind.Classic  => f(n)
      case r: FormKind.TaskList => g(r)
    }

  def foldNested[B](f: NonEmptyList[TaskSection] => B)(g: List[Section] => B): B =
    fold(classic => g(classic.sections))(taskList => f(taskList.sections))

  def allCustomExprs: List[Expr] =
    fold(_ => List.empty[Expr]) { taskList =>
      taskList.sections.toList.flatMap { taskSection =>
        taskSection.title.allInterpolations ++ taskSection.tasks.toList.flatMap { task =>
          task.title.allInterpolations ++
            task.includeIf.toList.flatMap(_.booleanExpr.allExpressions) ++
            task.startIf.toList.flatMap(_.booleanExpr.allExpressions) ++
            task.notRequiredIf.toList.flatMap(_.booleanExpr.allExpressions)
        }
      }
    }

  val allSections: AllSections = fold[AllSections] { classic =>
    AllSections.Classic(classic.sections.zipWithIndex.map { case (s, i) =>
      IndexedSection.SectionIndex(s, TemplateSectionIndex(i))
    })
  } { taskList =>
    AllSections.TaskList {
      taskList.sections.zipWithIndex.flatMap { case (taskSection, taskSectionIndex) =>
        taskSection.tasks.zipWithIndex.map { case (task, taskIndex) =>
          Coordinates(TaskSectionNumber(taskSectionIndex), TaskNumber(taskIndex)) -> updateIncludeIf(
            task.sections.toList,
            task.includeIf,
            task.notRequiredIf
          ).zipWithIndex.map { case (s, i) => IndexedSection.SectionIndex(s, TemplateSectionIndex(i)) }
        }
      }
    }
  }
  private val atlMap = allSections.sections.flatMap {
    case IndexedSection.SectionIndex(s: Section.AddToList, _) => s.atlMap
    case _                                                    => Map.empty
  }.toMap
  private val repeatingMap =
    allSections.sections
      .collect { case IndexedSection.SectionIndex(Section.RepeatingPage(page, _), _) => page.allIds.map(i => (i, i)) }
      .flatten
      .toMap
  private val groupMap = allSections.sections
    .collect { case IndexedSection.SectionIndex(Section.NonRepeatingPage(page), _) => page.allFieldsNested }
    .flatten
    .collect { case fc @ IsGroup(_) => fc.childrenFormComponents.map(c => (c.id, fc.id)) }
    .flatten
    .toMap[FormComponentId, FormComponentId]
  val repeatedComponentsDetails = RepeatedComponentsDetails(
    atlMap ++ groupMap ++ repeatingMap
  )

  private def updateIncludeIf(
    sections: List[Section],
    includeIf: Option[IncludeIf],
    notRequiredIf: Option[IncludeIf]
  ): List[Section] =
    sections.map {
      case s: Section.NonRepeatingPage =>
        s.modify(_.page.includeIf)
          .using(andIncludeIfs(_, includeIf))
          .modify(_.page.notRequiredIf)
          .using(andIncludeIfs(_, notRequiredIf))
      case s: Section.RepeatingPage =>
        s.modify(_.page.includeIf)
          .using(andIncludeIfs(_, includeIf))
          .modify(_.page.notRequiredIf)
          .using(andIncludeIfs(_, notRequiredIf))
      case s: Section.AddToList =>
        s.modify(_.includeIf)
          .using(andIncludeIfs(_, includeIf))
          .modify(_.notRequiredIf)
          .using(andIncludeIfs(_, notRequiredIf))
    }

  private def andIncludeIfs(includeIf1: Option[IncludeIf], includeIf2: Option[IncludeIf]): Option[IncludeIf] =
    (includeIf1, includeIf2) match {
      case (Some(IncludeIf(expr1)), Some(IncludeIf(expr2))) => Some(IncludeIf(And(expr1, expr2)))
      case (Some(IncludeIf(expr1)), None)                   => Some(IncludeIf(expr1))
      case (None, Some(IncludeIf(expr2)))                   => Some(IncludeIf(expr2))
      case (None, None)                                     => None
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
  id: Option[TaskId],
  title: SmartString,
  sections: NonEmptyList[Section],
  summarySection: Option[SummarySection],
  declarationSection: Option[DeclarationSection],
  includeIf: Option[IncludeIf],
  caption: Option[SmartString],
  startIf: Option[IncludeIf],
  notRequiredIf: Option[IncludeIf]
)

object Task {
  implicit val format: OFormat[Task] = derived.oformat()
}
