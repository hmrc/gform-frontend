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

package uk.gov.hmrc.gform.sharedmodel.form

import cats.implicits._
import play.api.libs.json.{ JsArray, JsError, JsObject, JsSuccess, JsValue, Json, OFormat }
import scala.util.Try
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, SectionNumber, TaskNumber, TaskSectionNumber, TemplateSectionIndex }

sealed trait VisitIndex extends Product with Serializable {

  def fold[B](f: VisitIndex.Classic => B)(g: VisitIndex.TaskList => B): B =
    this match {
      case n: VisitIndex.Classic  => f(n)
      case r: VisitIndex.TaskList => g(r)
    }

  private def visitClassic(sectionNumber: SectionNumber.Classic): VisitIndex =
    fold[VisitIndex] { classic =>
      val updatedVisits = VisitIndex.Classic(classic.visitsIndex + sectionNumber)
      updatedVisits
    }(identity)

  private def visitTaskList(sectionNumber: SectionNumber.TaskList): VisitIndex =
    fold[VisitIndex](identity) { taskList =>
      val update =
        taskList.visitsIndex.get(sectionNumber.coordinates).fold(Set(sectionNumber.sectionNumber)) { alreadyVisited =>
          alreadyVisited + sectionNumber.sectionNumber
        }

      val visitsIndexUpd = taskList.visitsIndex ++ Map(sectionNumber.coordinates -> update)

      VisitIndex.TaskList(visitsIndexUpd)
    }

  def visit(sectionNumber: SectionNumber): VisitIndex = sectionNumber.fold(visitClassic)(visitTaskList)
  def unvisit(sectionNumber: SectionNumber): VisitIndex = sectionNumber.fold(unvisitClassic)(unvisitTaskList)

  def removeIteration(
    sectionIndexToRemove: TemplateSectionIndex,
    iterationIndexToRemove: Int,
    isLastIteration: Boolean, // This indicates that we are removing last iteration
    maybeCoordinates: Option[Coordinates]
  ): VisitIndex = {
    import SectionNumber.Classic.AddToListPage

    def removeIterationClassic(classics: Set[SectionNumber.Classic]): Set[SectionNumber.Classic] =
      // not matched cases are removed
      classics.collect {
        case a @ AddToListPage.Page(sectionIndex, iterationIndex, pageNumber)
            if iterationIndex =!= iterationIndexToRemove && sectionIndexToRemove === sectionIndex =>
          if (iterationIndex > iterationIndexToRemove) {
            AddToListPage.Page(sectionIndex, iterationIndex - 1, pageNumber)
          } else {
            a
          }
        case a @ AddToListPage.TerminalPage(sectionIndex, iterationIndex, terminalPage)
            if iterationIndex =!= iterationIndexToRemove && sectionIndexToRemove === sectionIndex =>
          if (iterationIndex > iterationIndexToRemove) {
            AddToListPage.TerminalPage(sectionIndex, iterationIndex - 1, terminalPage)
          } else {
            a
          }
        case p @ AddToListPage.DefaultPage(sectionIndex) if !isLastIteration || sectionIndexToRemove =!= sectionIndex =>
          p
        case p @ AddToListPage.Page(sectionIndex, _, _) if sectionIndexToRemove =!= sectionIndex         => p
        case p @ AddToListPage.TerminalPage(sectionIndex, _, _) if sectionIndexToRemove =!= sectionIndex => p
        case np @ SectionNumber.Classic.NormalPage(_)                                                    => np
        case rp @ SectionNumber.Classic.RepeatedPage(_, _)                                               => rp
      }

    fold[VisitIndex] { classic =>
      val updatedVisitsIndex = removeIterationClassic(classic.visitsIndex)
      classic.copy(visitsIndex = updatedVisitsIndex)
    } { taskList =>
      maybeCoordinates.fold(taskList) { coordinatesToModify =>
        VisitIndex.TaskList(taskList.visitsIndex.map { case (coordinates, sectionNumbers) =>
          if (coordinates === coordinatesToModify) {
            (coordinates, removeIterationClassic(sectionNumbers))
          } else {
            (coordinates, sectionNumbers)
          }
        })
      }
    }
  }

  private def unvisitClassic(sectionNumber: SectionNumber.Classic): VisitIndex =
    fold[VisitIndex] { classic =>
      val updatedVisits = VisitIndex.Classic(classic.visitsIndex - sectionNumber)
      updatedVisits
    }(identity)

  private def unvisitTaskList(sectionNumber: SectionNumber.TaskList): VisitIndex =
    fold[VisitIndex](identity) { taskList =>
      val update =
        taskList.visitsIndex.get(sectionNumber.coordinates).fold(Set(sectionNumber.sectionNumber)) { alreadyVisited =>
          alreadyVisited - sectionNumber.sectionNumber
        }

      val visitsIndexUpd = taskList.visitsIndex ++ Map(sectionNumber.coordinates -> update)

      VisitIndex.TaskList(visitsIndexUpd)
    }

  private def containsClassic(sectionNumber: SectionNumber.Classic): Boolean =
    fold[Boolean](classic => classic.visitsIndex.contains(sectionNumber))(_ => false)

  private def containsTaskList(sectionNumber: SectionNumber.TaskList): Boolean =
    fold[Boolean](_ => false)(taskList =>
      taskList.visitsIndex.get(sectionNumber.coordinates).fold(false)(_.contains(sectionNumber.sectionNumber))
    )

  def contains(sectionNumber: SectionNumber): Boolean = sectionNumber.fold(containsClassic)(containsTaskList)
}

object VisitIndex {

  final case class Classic(visitsIndex: Set[SectionNumber.Classic]) extends VisitIndex
  final case class TaskList(visitsIndex: Map[Coordinates, Set[SectionNumber.Classic]]) extends VisitIndex

  val key: String = "visitsIndex"

  val empty: VisitIndex = Classic(Set.empty)

  implicit val format: OFormat[VisitIndex] = OFormat(
    (jsValue: JsValue) =>
      (jsValue \ key).toOption match {
        case None => JsError(s"Missing '$key' field. Failed to decode VisitIndex from: $jsValue")
        case Some(a: JsArray) =>
          a.validate[Set[SectionNumber.Classic]] match {
            case JsSuccess(visits, _) => JsSuccess(Classic(visits))
            case JsError(errors)      => JsSuccess(Classic(Set.empty[SectionNumber.Classic]))
          }
        case Some(o: JsObject) =>
          o.value.toList
            .traverse { case (k, v) =>
              Try(k.split(",").toList).collect { case taskSectionNumber :: taskNumber :: Nil =>
                val key = Coordinates(TaskSectionNumber(taskSectionNumber.toInt), TaskNumber(taskNumber.toInt))
                val visits = v.validate[Set[SectionNumber.Classic]].getOrElse(Set.empty[SectionNumber.Classic])
                key -> visits
              }
            }
            .fold(
              error => JsError("Failed to decode VisitIndex for TaskList from json: " + jsValue),
              xs => JsSuccess(TaskList(xs.toMap))
            )
        case Some(unexpected) => JsError("Unknown type. Failed to decode VisitIndex from json: " + unexpected)
      },
    (visitIndex: VisitIndex) =>
      visitIndex match {
        case Classic(visitsIndex) => Json.obj(key -> Json.toJson(visitsIndex))
        case TaskList(visitsIndex) =>
          val s: Map[String, JsValue] =
            visitsIndex.toList.map { case (Coordinates(TaskSectionNumber(tsc), TaskNumber(tn)), indexes) =>
              List(tsc, tn).mkString(",") -> {
                Json.toJson(indexes)
              }
            }.toMap
          Json.obj(key -> Json.toJson(s))
      }
  )
}
