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

package uk.gov.hmrc.gform.sharedmodel.form

import cats.implicits._
import play.api.libs.json.{ JsArray, JsError, JsObject, JsSuccess, JsValue, Json, OFormat }
import scala.util.Try
import uk.gov.hmrc.gform.models.{ Coordinates, DataExpanded, FormModel, PageModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ SectionNumber, TaskNumber, TaskSectionNumber }

sealed trait VisitIndex extends Product with Serializable {

  def fold[B](f: VisitIndex.Classic => B)(g: VisitIndex.TaskList => B): B =
    this match {
      case n: VisitIndex.Classic  => f(n)
      case r: VisitIndex.TaskList => g(r)
    }

  private def visitClassic(sectionNumber: SectionNumber.Classic): VisitIndex =
    fold[VisitIndex](classic => VisitIndex.Classic(classic.visitsIndex + sectionNumber.sectionNumber))(identity)

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

  private def unvisitClassic(sectionNumber: SectionNumber.Classic): VisitIndex =
    fold[VisitIndex](classic => VisitIndex.Classic(classic.visitsIndex - sectionNumber.sectionNumber))(identity)

  private def unvisitTaskList(sectionNumber: SectionNumber.TaskList): VisitIndex =
    fold[VisitIndex](identity)(identity)

  private def containsClassic(sectionNumber: SectionNumber.Classic): Boolean =
    fold[Boolean](classic => classic.visitsIndex.contains(sectionNumber.sectionNumber))(_ => false)

  private def containsTaskList(sectionNumber: SectionNumber.TaskList): Boolean =
    fold[Boolean](_ => false)(taskList =>
      taskList.visitsIndex.get(sectionNumber.coordinates).fold(false)(_.contains(sectionNumber.sectionNumber))
    )

  def contains(sectionNumber: SectionNumber): Boolean = sectionNumber.fold(containsClassic)(containsTaskList)
}

object VisitIndex {

  final case class Classic(visitsIndex: Set[Int]) extends VisitIndex
  final case class TaskList(visitsIndex: Map[Coordinates, Set[Int]]) extends VisitIndex

  val key: String = "visitsIndex"

  val empty: VisitIndex = Classic(Set.empty)

  implicit val format: OFormat[VisitIndex] = OFormat(
    (jsValue: JsValue) =>
      (jsValue \ key).toOption match {
        case None             => JsError(s"Missing '$key' field. Failed to decode VisitIndex from: $jsValue")
        case Some(a: JsArray) => JsSuccess(Classic(a.value.map(_.as[Int]).toSet))
        case Some(o: JsObject) =>
          val res: Try[List[(Coordinates, Set[Int])]] =
            o.value.toList.traverse { case (k, v) =>
              Try(k.split(",").toList.map(_.toInt)).collect { case taskSectionNumber :: taskNumber :: Nil =>
                (Coordinates(TaskSectionNumber(taskSectionNumber), TaskNumber(taskNumber))) -> v.as[Set[Int]]
              }
            }
          res.fold(
            error => JsError("Failed to decode VisitIndex for TaskList from json: " + jsValue),
            xs => JsSuccess(TaskList(xs.toMap))
          )
        case Some(unexpected) => JsError("Unknown type. Failed to decode VisitIndex from json: " + unexpected)
      },
    (visitIndex: VisitIndex) =>
      visitIndex match {
        case Classic(visitsIndex) => Json.obj(key -> Json.toJson(visitsIndex))
        case TaskList(visitsIndex) =>
          val s: Map[String, JsValue] = visitsIndex.toList.map {
            case (Coordinates(TaskSectionNumber(tsc), TaskNumber(tn)), indexes) =>
              List(tsc, tn).mkString(",") -> Json.toJson(indexes)
          }.toMap
          Json.obj(key -> Json.toJson(s))
      }
  )

  def updateSectionVisits(
    formModel: FormModel[DataExpanded],
    mongoFormModel: FormModel[DataExpanded],
    visitsIndex: VisitIndex
  ): VisitIndex = {
    def update(
      xs: Set[Int],
      f: Int => SectionNumber,
      pages: FormModel[DataExpanded] => List[PageModel[DataExpanded]]
    ): Set[Int] =
      xs.map { index =>
        Try(mongoFormModel(f(index))).toOption.fold(-1) { page =>
          page.allFormComponents.headOption.fold(-1) { mongoHead =>
            val firstComponentId = mongoHead.id
            pages(formModel).indexWhere { pageModel =>
              pageModel.allFormComponents.headOption.fold(false)(_.id === firstComponentId)
            }

          }
        }
      }.filterNot(_ === -1)

    visitsIndex.fold[VisitIndex] { classic =>
      VisitIndex.Classic(update(classic.visitsIndex, SectionNumber.Classic(_), _.pages.toList))
    } { taskList =>
      VisitIndex.TaskList {
        taskList.visitsIndex.map { case (coordinates, indexes) =>
          coordinates ->
            update(
              indexes,
              SectionNumber.TaskList(coordinates, _),
              _.taskList.availablePages(coordinates).toList
            )
        }
      }
    }
  }
}
