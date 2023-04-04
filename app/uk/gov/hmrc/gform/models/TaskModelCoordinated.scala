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

sealed trait TaskModelCoordinated[A <: PageMode] extends Product with Serializable {

  def fold[B](f: TaskModelCoordinated.AllHidden[A] => B)(g: TaskModelCoordinated.Editable[A] => B): B =
    this match {
      case n: TaskModelCoordinated.AllHidden[A] => f(n)
      case r: TaskModelCoordinated.Editable[A]  => g(r)
    }

  def toTaskModel(f: NonEmptyList[BracketPlain[A]] => NonEmptyList[Bracket[A]]): TaskModel[A] =
    fold(_ => TaskModel.allHidden[A])(editable => TaskModel.editable[A](f(editable.brackets)))

  def modifyBrackets(f: NonEmptyList[BracketPlain[A]] => NonEmptyList[BracketPlain[A]]): TaskModelCoordinated[A] =
    fold(_ => TaskModelCoordinated.allHidden[A])(editable => TaskModelCoordinated.editable[A](f(editable.brackets)))
}

object TaskModelCoordinated {

  def editable[A <: PageMode](bs: NonEmptyList[BracketPlain[A]]): TaskModelCoordinated[A] =
    TaskModelCoordinated.Editable[A](bs)
  def allHidden[A <: PageMode]: TaskModelCoordinated[A] = AllHidden()

  case class Editable[A <: PageMode](brackets: NonEmptyList[BracketPlain[A]]) extends TaskModelCoordinated[A]
  case class AllHidden[A <: PageMode]() extends TaskModelCoordinated[A]
}
