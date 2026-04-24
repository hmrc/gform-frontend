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

sealed trait TaskModel extends Product with Serializable {

  def fold[B](f: TaskModel.AllHidden => B)(g: TaskModel.Editable => B): B =
    this match {
      case n: TaskModel.AllHidden => f(n)
      case r: TaskModel.Editable  => g(r)
    }

  val toBracketsList: List[Bracket] = fold(_ => List.empty[Bracket])(_.brackets.toList)

  val toBracketsNel: Option[NonEmptyList[Bracket]] =
    fold(_ => Option.empty[NonEmptyList[Bracket]])(editable => Some(editable.brackets))

  def mapBracket(f: Bracket => Bracket): TaskModel =
    fold(_ => TaskModel.allHidden)(editable => TaskModel.editable(editable.brackets.map(f)))

  def toTaskModel(): TaskModel =
    fold(_ => TaskModel.allHidden)(editable => TaskModel.editable(editable.brackets))

  def modifyBrackets(f: NonEmptyList[Bracket] => NonEmptyList[Bracket]): TaskModel =
    fold(_ => TaskModel.allHidden)(editable => TaskModel.editable(f(editable.brackets)))
}

object TaskModel {

  def apply(xs: List[Bracket]): TaskModel =
    NonEmptyList
      .fromList(xs)
      .fold[TaskModel](TaskModel.allHidden)(nel => TaskModel.Editable(nel))

  def editable(bs: NonEmptyList[Bracket]): TaskModel = TaskModel.Editable(bs)
  def allHidden: TaskModel = AllHidden()

  // This task has at least one page visible
  case class Editable(brackets: NonEmptyList[Bracket]) extends TaskModel
  // This task has all pages hidden by include if
  case class AllHidden() extends TaskModel
}
