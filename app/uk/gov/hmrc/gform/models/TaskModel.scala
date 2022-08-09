/*
 * Copyright 2022 HM Revenue & Customs
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

sealed trait TaskModel[A <: PageMode] extends Product with Serializable {

  def fold[B](f: TaskModel.AllHidden[A] => B)(g: TaskModel.Editable[A] => B): B =
    this match {
      case n: TaskModel.AllHidden[A] => f(n)
      case r: TaskModel.Editable[A]  => g(r)
    }

  val toBracketsList: List[Bracket[A]] = fold(_ => List.empty[Bracket[A]])(_.brackets.toList)

  val toBracketsNel: Option[NonEmptyList[Bracket[A]]] =
    fold(_ => Option.empty[NonEmptyList[Bracket[A]]])(editable => Some(editable.brackets))

  def mapBracket[B <: PageMode](f: Bracket[A] => Bracket[B]): TaskModel[B] =
    fold(_ => TaskModel.allHidden[B])(editable => TaskModel.editable(editable.brackets.map(f)))

  val toTaskModelCoordinated: TaskModelCoordinated[A] =
    fold[TaskModelCoordinated[A]](_ => TaskModelCoordinated.allHidden)(editable =>
      TaskModelCoordinated.editable(editable.brackets.map(_.toPlainBracket))
    )

}

object TaskModel {

  def apply[A <: PageMode](xs: List[Bracket[A]]): TaskModel[A] =
    NonEmptyList
      .fromList(xs)
      .fold[TaskModel[A]](TaskModel.allHidden)(nel => TaskModel.Editable(nel))

  def editable[A <: PageMode](bs: NonEmptyList[Bracket[A]]): TaskModel[A] = TaskModel.Editable(bs)
  def allHidden[A <: PageMode]: TaskModel[A] = AllHidden[A]

  // This task has at least one page visible
  case class Editable[A <: PageMode](brackets: NonEmptyList[Bracket[A]]) extends TaskModel[A]
  // This task has all pages hidden by include if
  case class AllHidden[A <: PageMode]() extends TaskModel[A]
}
