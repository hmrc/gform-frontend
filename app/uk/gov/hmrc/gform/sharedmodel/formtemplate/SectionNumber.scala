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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.Eq
import cats.implicits._
import play.api.libs.json._
import scala.util.{ Failure, Success, Try }
import uk.gov.hmrc.gform.models.Coordinates

sealed trait SectionNumber extends Ordered[SectionNumber] with Product with Serializable {

  def fold[B](f: SectionNumber.Classic => B)(g: SectionNumber.TaskList => B): B =
    this match {
      case n: SectionNumber.Classic  => f(n)
      case r: SectionNumber.TaskList => g(r)
    }

  def isTaskList: Boolean = fold(_ => false)(_ => true)

  override def compare(that: SectionNumber): Int = (this, that) match {
    case (SectionNumber.Classic(sn0), SectionNumber.Classic(sn1)) => sn0.compare(sn1)
    case (SectionNumber.TaskList(Coordinates(tsn0, tn0), sn0), SectionNumber.TaskList(Coordinates(tsn1, tn1), sn1)) =>
      if (tsn0 === tsn1 && tn0 === tn1) sn0.compare(sn1)
      else if (tsn0 === tsn1) tn0.compare(tn1)
      else tsn0.compare(tsn1)

    case (l, r) => throw new Exception(s"Cannot compare SectionNumber: $l with $r")
  }

  def toCoordinates: Option[Coordinates] =
    fold[Option[Coordinates]](_ => None)(taskList => Some(taskList.coordinates))

  def toCoordinatesUnsafe: Coordinates =
    fold[Coordinates](_ => throw new Exception(s"Cannot convert $this to coordinates"))(_.coordinates)

  def contains(coordinates: Coordinates): Boolean = toCoordinates.fold(false)(_ === coordinates)

  def increment: SectionNumber = fold[SectionNumber] { classic =>
    SectionNumber.Classic(classic.sectionNumber + 1)
  } { case SectionNumber.TaskList(Coordinates(taskSectionNumber, taskNumber), sectionNumber) =>
    SectionNumber.TaskList(Coordinates(taskSectionNumber, taskNumber), sectionNumber + 1)
  }

  def +(i: Int): SectionNumber = fold[SectionNumber] { classic =>
    SectionNumber.Classic(classic.sectionNumber + i)
  } { case SectionNumber.TaskList(Coordinates(taskSectionNumber, taskNumber), sectionNumber) =>
    SectionNumber.TaskList(Coordinates(taskSectionNumber, taskNumber), sectionNumber + i)
  }

  def unsafeToClassic: SectionNumber.Classic =
    fold(identity)(taskList => throw new Exception("unsafeToClassic invoked on TaskList type: " + taskList))
  def unsafeToTaskList: SectionNumber.TaskList =
    fold(classic => throw new Exception("unsafeToTaskList invoked on Classic type: " + classic))(identity)

  def value: String = this match {
    case SectionNumber.Classic(value) => value.toString
    case SectionNumber.TaskList(Coordinates(taskSectionNumber, taskNumber), sectionNumber) =>
      List(taskSectionNumber.value, taskNumber.value, sectionNumber).mkString(",")
  }

  def numberValue: Int = this match {
    case SectionNumber.Classic(value)                       => value
    case SectionNumber.TaskList(coordinates, sectionNumber) => coordinates.numberValue + sectionNumber
  }
}

object SectionNumber {
  final case class Classic(sectionNumber: Int) extends SectionNumber
  final case class TaskList(
    coordinates: Coordinates,
    sectionNumber: Int
  ) extends SectionNumber

  val classicZero = Classic(0)
  val taskListZero = TaskList(Coordinates(TaskSectionNumber(0), TaskNumber(0)), 0)

  implicit val equal: Eq[SectionNumber] = Eq.fromUniversalEquals
  implicit val format: Format[SectionNumber] = Format[SectionNumber](
    Reads[SectionNumber] {
      case JsString(value) =>
        parse(value) match {
          case Success(sectionNumber) => JsSuccess(sectionNumber)
          case Failure(error)         => JsError(s"Invalid section number: $value. Error: $error")
        }
      case unknown => JsError(s"JsString value expected, got: $unknown")
    },
    Writes[SectionNumber](a => JsString(a.value))
  )

  def parse(value: String): Try[SectionNumber] =
    value.split(",").toList.traverse(n => Try(n.toInt)).collect {
      case sectionNumber :: Nil => SectionNumber.Classic(sectionNumber)
      case taskSectionNumber :: taskNumber :: sectionNumber :: Nil =>
        SectionNumber.TaskList(Coordinates(TaskSectionNumber(taskSectionNumber), TaskNumber(taskNumber)), sectionNumber)
    }
}
