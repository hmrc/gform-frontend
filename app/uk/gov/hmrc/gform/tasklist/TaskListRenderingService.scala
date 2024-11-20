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
import cats.syntax.eq._
import play.api.i18n.Messages
import play.api.mvc.Request
import play.twirl.api.Html
import scala.concurrent.{ ExecutionContext, Future }

import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluationSyntax, SmartStringEvaluator }
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.{ BracketsWithSectionNumber, Visibility }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.VariadicValue
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, FormTemplate, TaskNumber, TaskSectionNumber }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.govukfrontend.views.Aliases.{ TaskList, TaskListItemTitle }
import uk.gov.hmrc.govukfrontend.views.viewmodels.tag.Tag
import uk.gov.hmrc.govukfrontend.views.viewmodels.tasklist.{ TaskListItem, TaskListItemStatus }
import uk.gov.hmrc.http.HeaderCarrier

class TaskListRenderingService(
  frontendAppConfig: FrontendAppConfig,
  validationService: ValidationService
)(implicit ec: ExecutionContext) {
  def renderTaskList(
    formTemplate: FormTemplate,
    maybeAccessCode: Option[AccessCode],
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[_],
    hc: HeaderCarrier,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[Html] =
    for {
      statusesLookup <- statuses(cache, envelope, formModelOptics)
    } yield TaskListUtils.withTaskList(formTemplate) { taskList =>
      val visibleTaskCoordinates: List[Coordinates] = taskList.sections.toList.zipWithIndex.flatMap {
        case (taskSection, taskSectionIndex) =>
          taskSection.tasks.toList.zipWithIndex.collect {
            case (task, taskIndex)
                if task.includeIf.forall(formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(_, None)) =>
              evalCoordinates(taskSectionIndex, taskIndex)
          }
      }
      val visibleTaskStatusesLookup = statusesLookup.filter { case (coord, _) =>
        visibleTaskCoordinates.contains(coord)
      }
      val completedTasks = completedTasksCount(visibleTaskStatusesLookup)

      def taskUrl(coordinates: Coordinates, taskStatus: TaskStatus) =
        taskStatus match {
          case TaskStatus.CannotStartYet | TaskStatus.NotRequired => None
          case _ =>
            Some(
              routes.TaskListController
                .newTask(
                  formTemplate._id,
                  maybeAccessCode,
                  coordinates.taskSectionNumber,
                  coordinates.taskNumber,
                  taskStatus == TaskStatus.Completed
                )
                .url
            )
        }

      val taskLists: List[TaskListView] = taskList.sections.toList.zipWithIndex.map {
        case (taskSection, taskSectionIndex) =>
          val taskListItems = taskSection.tasks.toList.zipWithIndex.collect {
            case (task, taskIndex) if visibleTaskCoordinates.contains(evalCoordinates(taskSectionIndex, taskIndex)) =>
              val coordinates = evalCoordinates(taskSectionIndex, taskIndex)
              val status: TaskStatus = statusesLookup.toList.toMap.getOrElse(
                coordinates,
                TaskStatus.NotStarted
              )
              new TaskListItem(
                title = new TaskListItemTitle(content = content.Text(task.title.value())),
                hint = None,
                status = taskListStatus(status),
                href = taskUrl(coordinates, status)
              )
          }
          TaskListView(taskSection.title, TaskList(items = taskListItems, idPrefix = s"task-list-$taskSectionIndex"))
      }

      val submitSection = formTemplate.submitSection.map { submitSection =>
        val status =
          if (completedTasks === visibleTaskCoordinates.size || formTemplate.isSpecimen)
            TaskStatus.NotStarted
          else TaskStatus.CannotStartYet
        val href =
          if (status === TaskStatus.CannotStartYet) None
          else
            Some(
              routes.TaskListController.summaryPage(formTemplate._id, maybeAccessCode).url
            )
        val taskListItem = new TaskListItem(
          title = new TaskListItemTitle(content = content.Text(submitSection.taskLabel.value())),
          hint = None,
          status = taskListStatus(status),
          href = href
        )
        TaskListView(submitSection.label, new TaskList(items = List(taskListItem), idPrefix = "submit-section"))
      }

      val taskListViews = taskLists ++ submitSection

      uk.gov.hmrc.gform.views.html.tasklist
        .task_list(
          formTemplate,
          maybeAccessCode,
          taskListViews,
          completedTasks,
          frontendAppConfig
        )
    }

  private def evalCoordinates(taskSectionIndex: Int, taskIndex: Int) =
    Coordinates(TaskSectionNumber(taskSectionIndex), TaskNumber(taskIndex))

  private def taskListStatus(taskStatus: TaskStatus)(implicit messages: Messages): TaskListItemStatus = {
    val statusContent = content.Text(messages(s"taskList.$taskStatus"))
    val statusTag = taskStatus match {
      case TaskStatus.NotStarted => Some(new Tag(content = statusContent, classes = "govuk-tag--blue"))
      case TaskStatus.InProgress => Some(new Tag(content = statusContent, classes = "govuk-tag--light-blue"))
      case _                     => None
    }

    val statusClasses = taskStatus match {
      case TaskStatus.CannotStartYet => "govuk-task-list__status--cannot-start-yet"
      case _                         => ""
    }

    new TaskListItemStatus(tag = statusTag, content = statusContent, classes = statusClasses)
  }

  private def statuses(
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    hc: HeaderCarrier,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[NonEmptyList[(Coordinates, TaskStatus)]] = {
    val formModel = formModelOptics.formModelVisibilityOptics.formModel
    val taskList: BracketsWithSectionNumber.TaskList[Visibility] = formModel.brackets.unsafeToTaskList
    val coordinates: NonEmptyList[Coordinates] = taskList.brackets.map(_._1)

    if (cache.formTemplate.isSpecimen) {
      Future.successful(coordinates.map(coordinates => coordinates -> TaskStatus.NotStarted))
    } else {
      val cannotStartYetResolver = CannotStartYetResolver.create(formModelOptics.formModelRenderPageOptics.formModel)
      val notRequiredResolver = NotRequiredResolver.create(formModelOptics.formModelVisibilityOptics)
      coordinates
        .traverse { coordinate =>
          val dataForCoordinate: Set[VariadicValue] =
            formModelOptics.formModelVisibilityOptics.data.forCoordinate(coordinate)
          val hasTerminationPage = formModel.taskList.availablePages(coordinate).exists(_.isTerminationPage)

          for {
            formHandlerResult <-
              validationService.validateFormModel(
                cache,
                envelope,
                formModelOptics.formModelVisibilityOptics,
                Some(coordinate)
              )
            validatedATLs = validationService.validateATLs(
                              formModel.taskList.availablePages(coordinate),
                              formModelOptics.formModelVisibilityOptics
                            )
          } yield {
            val taskStatus =
              if (dataForCoordinate.isEmpty) {
                TaskStatus.NotStarted
              } else if (formHandlerResult.isFormValid && !hasTerminationPage && validatedATLs.isValid) {
                TaskStatus.Completed
              } else {
                TaskStatus.InProgress
              }
            coordinate -> taskStatus
          }

        }
        .map(notRequiredResolver.resolveNotRequired)
        .map(cannotStartYetResolver.resolveCannotStartYet)
    }

  }

  private def completedTasksCount(statuses: List[(Coordinates, TaskStatus)]): Int =
    statuses.count({ case (_, taskStatus) =>
      taskStatus === TaskStatus.Completed || taskStatus === TaskStatus.NotRequired
    })
}
