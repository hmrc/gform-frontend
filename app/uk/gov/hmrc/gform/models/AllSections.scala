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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, Section, TaskNumber, TaskSectionNumber, TemplateSectionIndex }

sealed trait IndexedSection {
  def section: Section // Section which knows its position in json template
}

object IndexedSection {
  case class SectionIndex(section: Section, index: TemplateSectionIndex) extends IndexedSection
  case class SectionNoIndex(section: Section.NonRepeatingPage) extends IndexedSection
}

sealed trait AllSections extends Product with Serializable {

  def sections: List[IndexedSection]

  def fold[B](f: AllSections.Classic => B)(g: AllSections.TaskList => B): B =
    this match {
      case n: AllSections.Classic  => f(n)
      case r: AllSections.TaskList => g(r)
    }

  def mapSection[A <: PageMode](
    f: Option[Coordinates] => IndexedSection => Option[Bracket[A]]
  ): BracketPlainCoordinated[A] =
    fold[BracketPlainCoordinated[A]] { classic =>
      val xs: List[Bracket[A]] = classic.sections.map(f(None)).collect { case Some(bracket) =>
        bracket
      }
      NonEmptyList
        .fromList(xs)
        .fold(throw new IllegalArgumentException("Form must have at least one (visible) page")) {
          BracketPlainCoordinated.Classic(_)
        }
    } { taskList =>
      val xs: NonEmptyList[(Coordinates, TaskModel[A])] = taskList.coordSections.map { case (coordinates, sections) =>
        val xs: List[Bracket[A]] = sections.map(f(Some(coordinates))).collect { case Some(bracket) =>
          bracket
        }

        val taskModelCoordinated: TaskModel[A] = NonEmptyList
          .fromList(xs)
          .fold(TaskModel.allHidden[A])(brackets => TaskModel.editable[A](brackets))

        coordinates -> taskModelCoordinated

      }
      BracketPlainCoordinated.TaskList(xs)
    }

  def +(others: List[Section.NonRepeatingPage]) = fold[AllSections](_.copy(others = others))(_.copy(others = others))
}

object AllSections {
  case class Classic(sections0: List[IndexedSection], others: List[Section.NonRepeatingPage] = Nil)
      extends AllSections {
    val sections = sections0 ++ others.map(section => IndexedSection.SectionNoIndex(section))
  }
  case class TaskList(
    sections0: NonEmptyList[(Coordinates, List[IndexedSection])],
    others: List[Section.NonRepeatingPage] = Nil
  ) extends AllSections {
    val coordSections: NonEmptyList[(Coordinates, List[IndexedSection])] = {
      if (others.isEmpty) {
        sections0
      } else
        sections0.append(
          (
            Coordinates(TaskSectionNumber(999999), TaskNumber(999999)),
            others.map(section => IndexedSection.SectionNoIndex(section))
          )
        )
    }
    val sections = sections0.toList.flatMap(_._2) ++ others.map(section => IndexedSection.SectionNoIndex(section))
  }
}
