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

import cats.Eq
import cats.implicits._
import cats.data.NonEmptyList
import scala.util.Try
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Section, TaskNumber, TaskSectionNumber }

final case class Coordinates(
  taskSectionNumber: TaskSectionNumber,
  taskNumber: TaskNumber
) {
  val value: String = List(taskSectionNumber.value, taskNumber.value).mkString(",")
  val numberValue: Int = taskSectionNumber.value * 10000000 + taskNumber.value * 10000
}

object Coordinates {
  implicit val equal: Eq[Coordinates] = Eq.fromUniversalEquals

  def parse(value: String): Try[Coordinates] =
    value.split(",").toList.traverse(n => Try(n.toInt)).collect { case taskSectionNumber :: taskNumber :: Nil =>
      Coordinates(TaskSectionNumber(taskSectionNumber), TaskNumber(taskNumber))
    }

}

sealed trait AllSections extends Product with Serializable {

  def sections: List[Section]

  def fold[B](f: AllSections.Classic => B)(g: AllSections.TaskList => B): B =
    this match {
      case n: AllSections.Classic  => f(n)
      case r: AllSections.TaskList => g(r)
    }

  def mapSection[A <: PageMode](f: Section => Option[BracketPlain[A]]): BracketPlainCoordinated[A] =
    fold[BracketPlainCoordinated[A]] { classic =>
      val xs: List[BracketPlain[A]] = classic.sections.map(f).collect { case Some(bracket) =>
        bracket
      }
      NonEmptyList
        .fromList(xs)
        .fold(throw new IllegalArgumentException("Form must have at least one (visible) page")) {
          BracketPlainCoordinated.Classic(_)
        }
    } { taskList =>
      val xs: NonEmptyList[(Coordinates, TaskModelCoordinated[A])] = taskList.coordSections.map {
        case (coordinates, sections) =>
          val xs: List[BracketPlain[A]] = sections.map(f).collect { case Some(bracket) =>
            bracket
          }

          val taskModelCoordinated: TaskModelCoordinated[A] = NonEmptyList
            .fromList(xs)
            .fold(TaskModelCoordinated.allHidden[A])(brackets => TaskModelCoordinated.editable[A](brackets))

          coordinates -> taskModelCoordinated

      }
      BracketPlainCoordinated.TaskList(xs)
    }

  def +(others: List[Section]) = fold[AllSections](_.copy(others = others))(_.copy(others = others))
}

object AllSections {
  case class Classic(sections0: List[Section], others: List[Section] = Nil) extends AllSections {
    val sections = sections0 ++ others
  }
  case class TaskList(sections0: NonEmptyList[(Coordinates, List[Section])], others: List[Section] = Nil)
      extends AllSections {
    val coordSections: NonEmptyList[(Coordinates, List[Section])] = {
      if (others.isEmpty) {
        sections0
      } else
        sections0.append((Coordinates(TaskSectionNumber(999999), TaskNumber(999999)), others))
    }
    val sections = sections0.toList.flatMap(_._2) ++ others
  }
}

sealed trait BracketPlainCoordinated[A <: PageMode] extends Product with Serializable {
  def fold[B](f: BracketPlainCoordinated.Classic[A] => B)(g: BracketPlainCoordinated.TaskList[A] => B): B =
    this match {
      case n: BracketPlainCoordinated.Classic[_]  => f(n)
      case r: BracketPlainCoordinated.TaskList[_] => g(r)
    }
}

object BracketPlainCoordinated {
  case class Classic[A <: PageMode](bracketPlains: NonEmptyList[BracketPlain[A]]) extends BracketPlainCoordinated[A]
  case class TaskList[A <: PageMode](bracketPlains: NonEmptyList[(Coordinates, TaskModelCoordinated[A])])
      extends BracketPlainCoordinated[A]
}
