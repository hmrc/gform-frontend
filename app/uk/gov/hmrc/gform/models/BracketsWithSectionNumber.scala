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

sealed trait BracketsWithSectionNumber[A <: PageMode] extends Product with Serializable {

  def fold[B](f: BracketsWithSectionNumber.Classic[A] => B)(g: BracketsWithSectionNumber.TaskList[A] => B): B =
    this match {
      case n: BracketsWithSectionNumber.Classic[_]  => f(n)
      case r: BracketsWithSectionNumber.TaskList[_] => g(r)
    }

  def unsafeToTaskList: BracketsWithSectionNumber.TaskList[A] =
    fold(classic =>
      throw new Exception("BracketsWithSectionNumber.unsafeToTaskList invoked on Classic type: " + classic)
    )(identity)

  def toBracketPlainCoordinated: BracketPlainCoordinated[A] = fold[BracketPlainCoordinated[A]] { classic =>
    BracketPlainCoordinated.Classic(classic.brackets)
  } { taskList =>
    BracketPlainCoordinated.TaskList(taskList.brackets)
  }

  def map[B <: PageMode](
    e: Singleton[A] => Singleton[B]
  )(
    f: CheckYourAnswers[A] => CheckYourAnswers[B]
  )(
    g: Repeater[A] => Repeater[B]
  )(
    h: Singleton[A] => Singleton[B]
  ) = this match {
    case BracketsWithSectionNumber.Classic(brackets) =>
      BracketsWithSectionNumber.Classic(
        brackets.map(_.map(e, f, g, h))
      )
    case BracketsWithSectionNumber.TaskList(brackets) =>
      BracketsWithSectionNumber.TaskList(brackets.map { case (coor, taskModel) =>
        coor -> taskModel.mapBracket(_.map(e, f, g, h))
      })
  }
  def addToListById(addToListId: AddToListId, idx: Int): Bracket.AddToListIteration[A] =
    addToListBracket(addToListId).iterations.toList(idx)

  def addToListBracket(addToListId: AddToListId): Bracket.AddToList[A] =
    fold(_.brackets)(_.allBrackets).toList
      .collectFirst {
        case b: Bracket.AddToList[A] if b.isToListById(addToListId) => b
      }
      .getOrElse(throw new IllegalArgumentException(s"Invalid addToListId $addToListId"))

  def addToListBrackets: List[Bracket.AddToList[A]] =
    fold(_.brackets)(_.allBrackets).collect { case x: Bracket.AddToList[A] =>
      x
    }
  def repeatingPageBrackets: List[Bracket.RepeatingPage[A]] =
    fold(_.brackets)(_.allBrackets).collect { case x: Bracket.RepeatingPage[A] =>
      x
    }
  def nonRepeatingPageBrackets: List[Bracket.NonRepeatingPage[A]] =
    fold(_.brackets)(_.allBrackets).collect { case x: Bracket.NonRepeatingPage[A] =>
      x
    }

  def withSectionNumber(sectionNumber: SectionNumber): Bracket[A] = {
    def bracketForSectionNumber(brackets: NonEmptyList[Bracket[A]]): Bracket[A] =
      brackets
        .find(_.hasSectionNumber(sectionNumber))
        .getOrElse {
          brackets
            .collect {
              case atl: Bracket.AddToList[A] if atl.iterations.exists(_.repeater.sectionNumber < sectionNumber) => atl
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

  def toBrackets: NonEmptyList[Bracket[A]] =
    fold(_.brackets)(_.allBrackets)

  def toPageModelWithNumber: NonEmptyList[(PageModel[A], SectionNumber)] =
    fold { classic =>
      classic.brackets.flatMap(_.toPageModelWithNumber)
    } { taskList =>
      taskList.allBrackets.flatMap(_.toPageModelWithNumber)
    }
}

object BracketsWithSectionNumber {
  case class Classic[A <: PageMode](brackets: NonEmptyList[Bracket[A]]) extends BracketsWithSectionNumber[A]

  case class TaskList[A <: PageMode](brackets: NonEmptyList[(Coordinates, TaskModel[A])])
      extends BracketsWithSectionNumber[A] {
    val lookup: Map[Coordinates, TaskModel[A]] = brackets.toList.toMap

    val allBrackets: NonEmptyList[Bracket[A]] = NonEmptyList
      .fromList(brackets.toList.flatMap(_._2.toBracketsList))
      .getOrElse(throw new Exception("Task list has to have at least one task with one visible page"))

    def bracketsFor(coordinates: Coordinates): TaskModel[A] = lookup.getOrElse(
      coordinates,
      throw new Exception(
        s"""Cannot find Brackets for coordinates $coordinates. Known coordinates: ${lookup.keys.mkString(",")}"""
      )
    )
  }

  def fromBracketCoordinated[A <: PageMode](bracketPlains: BracketPlainCoordinated[A]): BracketsWithSectionNumber[A] =
    bracketPlains match {
      case BracketPlainCoordinated.Classic(brackets) => Classic(brackets)
      case BracketPlainCoordinated.TaskList(coordinatedBrackets) =>
        val res: NonEmptyList[(Coordinates, TaskModel[A])] =
          coordinatedBrackets.map { case (coordinated, taskModelCoordinated) =>
            (coordinated, taskModelCoordinated.toTaskModel())
          }
        TaskList(res)
    }
}
