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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, Coordinates, SectionNumber }

sealed trait Brackets extends Product with Serializable {

  def fold[B](f: Brackets.Classic => B)(g: Brackets.TaskList => B): B =
    this match {
      case n: Brackets.Classic  => f(n)
      case r: Brackets.TaskList => g(r)
    }

  def unsafeToTaskList: Brackets.TaskList =
    fold(classic => throw new Exception("Brackets.unsafeToTaskList invoked on Classic type: " + classic))(
      identity
    )

  def map(
    e: Singleton => Singleton
  )(
    f: CheckYourAnswers => CheckYourAnswers
  )(
    g: Repeater => Repeater
  ) = this match {
    case Brackets.Classic(brackets) =>
      Brackets.Classic(
        brackets.map(_.map(e, f, g))
      )
    case Brackets.TaskList(brackets) =>
      Brackets.TaskList(brackets.map { case (coor, taskModel) =>
        coor -> taskModel.mapBracket(_.map(e, f, g))
      })
  }
  def addToListById(addToListId: AddToListId, idx: Int): Bracket.AddToListIteration =
    addToListBracket(addToListId).iterations.toList(idx)

  def addToListBracket(addToListId: AddToListId): Bracket.AddToList =
    fold(_.brackets)(_.allBrackets).toList
      .collectFirst {
        case b: Bracket.AddToList if b.isToListById(addToListId) => b
      }
      .getOrElse(throw new IllegalArgumentException(s"Invalid addToListId $addToListId"))

  def addToListBrackets: List[Bracket.AddToList] =
    fold(_.brackets)(_.allBrackets).collect { case x: Bracket.AddToList =>
      x
    }
  def repeatingPageBrackets: List[Bracket.RepeatingPage] =
    fold(_.brackets)(_.allBrackets).collect { case x: Bracket.RepeatingPage =>
      x
    }
  def nonRepeatingPageBrackets: List[Bracket.NonRepeatingPage] =
    fold(_.brackets)(_.allBrackets).collect { case x: Bracket.NonRepeatingPage =>
      x
    }

  def withSectionNumber(sectionNumber: SectionNumber): Bracket = {
    def bracketForSectionNumber(brackets: NonEmptyList[Bracket]): Bracket =
      brackets
        .find(_.hasSectionNumber(sectionNumber))
        .getOrElse {
          brackets
            .collect {
              case atl: Bracket.AddToList if atl.iterations.exists(_.repeater.sectionNumber < sectionNumber) => atl
            }
            .lastOption
            .getOrElse(throw new IllegalArgumentException(s"Wrong sectionNumber $sectionNumber"))
        }
    fold { classic =>
      bracketForSectionNumber(classic.brackets)
    } { taskList =>
      val coordinates: Coordinates = sectionNumber.unsafeToTaskList.coordinates
      taskList.lookup
        .get(coordinates)
        .flatMap(_.toBracketsNel)
        .map(bracketForSectionNumber)
        .getOrElse(throw new Exception(s"Bracket not found for coordinates $coordinates"))
    }
  }

  def toBrackets: NonEmptyList[Bracket] = fold(_.brackets)(_.allBrackets)

  def toPageModelWithNumber: NonEmptyList[(PageModel, SectionNumber)] = toBrackets.flatMap(_.toPageModelWithNumber)
}

object Brackets {
  case class Classic(brackets: NonEmptyList[Bracket]) extends Brackets

  case class TaskList(brackets: NonEmptyList[(Coordinates, TaskModel)]) extends Brackets {
    val lookup: Map[Coordinates, TaskModel] = brackets.toList.toMap

    val allBrackets: NonEmptyList[Bracket] = NonEmptyList
      .fromList(brackets.toList.flatMap(_._2.toBracketsList))
      .getOrElse(throw new Exception("Task list has to have at least one task with one visible page"))

    def bracketsFor(coordinates: Coordinates): TaskModel = lookup.getOrElse(
      coordinates,
      throw new Exception(
        s"""Cannot find Brackets for coordinates $coordinates. Known coordinates: ${lookup.keys.mkString(",")}"""
      )
    )
  }
}
