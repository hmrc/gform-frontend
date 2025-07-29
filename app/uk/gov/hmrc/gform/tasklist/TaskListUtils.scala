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

package uk.gov.hmrc.gform.tasklist

import cats.data.NonEmptyList
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, CacheData }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.models.{ BracketsWithSectionNumber, Visibility }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.sharedmodel.form.{ FormModelOptics, TaskIdTaskStatusMapping }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ TaskStatus => TaskStatusExpr }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, Expr, FormKind, FormTemplate, Task, TaskNumber, TaskSection, TaskSectionNumber }
import uk.gov.hmrc.gform.tasklist.TaskStatus.NotStarted
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

object TaskListUtils {

  def withTask[A](formTemplate: FormTemplate, taskSectionNumber: TaskSectionNumber, taskNumber: TaskNumber)(
    f: Task => A
  ): A =
    withTaskList(formTemplate) { taskList =>
      taskList.sections.toList
        .lift(taskSectionNumber.value)
        .fold(invalidTaskSectionNumber(formTemplate, taskSectionNumber)) { taskSection =>
          taskSection.tasks.toList
            .lift(taskNumber.value)
            .fold(invalidTaskNumber(formTemplate, taskSectionNumber, taskNumber))(f)
        }
    }

  def withTaskSection[A](formTemplate: FormTemplate, taskSectionNumber: TaskSectionNumber)(
    f: TaskSection => A
  ): A =
    withTaskList(formTemplate) { taskList =>
      taskList.sections.toList
        .lift(taskSectionNumber.value)
        .fold(invalidTaskSectionNumber(formTemplate, taskSectionNumber))(f)
    }

  def withTaskList[A](formTemplate: FormTemplate)(f: FormKind.TaskList => A): A =
    formTemplate.formKind
      .fold(_ => throw new Exception(s"Form template: ${formTemplate._id.value} is not task list"))(f)

  def toTaskCoordinatesMap[A](formTemplate: FormTemplate): Map[Task, Coordinates] =
    withTaskList(formTemplate) { taskList =>
      taskList.sections.toList.zipWithIndex.flatMap { case (taskSection, taskSectionIndex) =>
        taskSection.tasks.toList.zipWithIndex.collect { case (task, taskIndex) =>
          val coordinates = evalCoordinates(taskSectionIndex, taskIndex)
          task -> coordinates
        }
      }
    }.toMap

  private def invalidTaskSectionNumber(formTemplate: FormTemplate, taskSectionNumber: TaskSectionNumber) =
    throw new Exception(s"Task List: ${formTemplate._id.value} does not contains task section $taskSectionNumber")

  private def invalidTaskNumber(
    formTemplate: FormTemplate,
    taskSectionNumber: TaskSectionNumber,
    taskNumber: TaskNumber
  ) =
    throw new Exception(
      s"Task List: ${formTemplate._id.value} task section $taskSectionNumber does not contains task number $taskNumber"
    )

  def evalStatusLookup(
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    validationService: ValidationService,
    taskCoordinatesMap: Map[Task, Coordinates]
  )(implicit
    hc: HeaderCarrier,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator,
    ex: ExecutionContext
  ): Future[NonEmptyList[(Coordinates, TaskStatus)]] = {
    val formModel = formModelOptics.formModelVisibilityOptics.formModel
    val taskList: BracketsWithSectionNumber.TaskList[Visibility] = formModel.brackets.unsafeToTaskList
    val coordinates: NonEmptyList[Coordinates] = taskList.brackets.map(_._1)
    if (cache.formTemplate.isSpecimen) {
      Future.successful(coordinates.map(coordinates => coordinates -> TaskStatus.NotStarted))
    } else {
      val formModelVisibilityOptics = formModelOptics.formModelVisibilityOptics

      val cannotStartYetResolver = CannotStartYetResolver.create(formModelOptics, taskCoordinatesMap)
      val notRequiredResolver = NotRequiredResolver.create(formModelVisibilityOptics, taskCoordinatesMap)
      for {
        statusesLookup <- coordinates
                            .traverse { coordinate =>
                              val dataForCoordinate: Set[VariadicValue] =
                                formModelVisibilityOptics.data
                                  .forCoordinate(coordinate)
                              val hasTerminationPage = formModel.taskList
                                .availablePages(coordinate)
                                .exists(
                                  _.isTerminationPage(formModelVisibilityOptics.booleanExprResolver)
                                )

                              for {
                                formHandlerResult <-
                                  validationService.validateFormModel(
                                    cache,
                                    envelope,
                                    formModelVisibilityOptics,
                                    Some(coordinate)
                                  )
                                validatedATLs =
                                  validationService.validateATLs(
                                    formModel.taskList.availablePages(
                                      coordinate
                                    ),
                                    formModelVisibilityOptics
                                  )
                              } yield {
                                val taskStatus =
                                  if (dataForCoordinate.isEmpty) {
                                    TaskStatus.NotStarted
                                  } else if (
                                    formHandlerResult.isFormValid && !hasTerminationPage && validatedATLs.isValid
                                  ) {
                                    TaskStatus.Completed
                                  } else {
                                    TaskStatus.InProgress
                                  }
                                coordinate -> taskStatus
                              }
                            }
                            .map(notRequiredResolver.resolveNotRequired)
                            .map(
                              cannotStartYetResolver.resolveCannotStartYet
                            )

      } yield statusesLookup
    }
  }

  def evalCoordinates(taskSectionIndex: Int, taskIndex: Int): Coordinates =
    Coordinates(TaskSectionNumber(taskSectionIndex), TaskNumber(taskIndex))

  def evalTaskIdTaskStatusMapping(
    taskCoordinatesMap: Map[Task, Coordinates],
    statusesLookup: NonEmptyList[(Coordinates, TaskStatus)]
  ): TaskIdTaskStatusMapping = {
    val statusesMap = statusesLookup.toList.toMap

    val mapping = taskCoordinatesMap.collect {
      case (task, coordinates) if task.id.isDefined =>
        task.id.get -> statusesMap.getOrElse(coordinates, NotStarted)
    }

    TaskIdTaskStatusMapping(mapping)
  }

  def hasTaskStatusExpr(cache: AuthCacheWithForm, formModelOptics: FormModelOptics[DataOrigin.Mongo]) = {
    val formModel = formModelOptics.formModelRenderPageOptics.formModel
    val allBracketExprs = formModel.brackets.toBrackets.toList.flatMap(_.allExprs(formModel))
    val allCustomExprs = cache.formTemplateContext.formTemplate.formKind.allCustomExprs
    val expressionsOutExprs =
      cache.formTemplateContext.formTemplate.expressionsOutput.fold(List.empty[Expr])(_.lookup.values.toList)
    val allExprs = allBracketExprs ++ allCustomExprs ++ expressionsOutExprs
    val leafs: List[Expr] = allExprs.flatMap(_.leafs())
    leafs.exists { case TaskStatusExpr(_) => true; case _ => false }
  }
}
