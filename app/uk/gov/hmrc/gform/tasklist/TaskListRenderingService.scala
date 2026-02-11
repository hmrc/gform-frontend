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

import cats.syntax.eq._
import play.api.i18n.Messages
import play.api.mvc.Request
import play.twirl.api.Html

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.{ RealSmartStringEvaluatorFactory, SmartStringEvaluationSyntax, SmartStringEvaluator, SmartStringEvaluatorFactory }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.gform.NoSpecificAction
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.{ ProcessDataService, SectionSelectorType }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.form.{ FormIdData, FormModelOptics, TaskIdTaskStatusMapping, UserData, Validated }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, FormTemplate }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.govukfrontend.views.Aliases.{ TaskList, TaskListItemTitle, Text }
import uk.gov.hmrc.govukfrontend.views.viewmodels.hint.Hint
import uk.gov.hmrc.govukfrontend.views.viewmodels.tag.Tag
import uk.gov.hmrc.govukfrontend.views.viewmodels.tasklist.{ TaskListItem, TaskListItemStatus }
import uk.gov.hmrc.http.HeaderCarrier

class TaskListRenderingService(
  frontendAppConfig: FrontendAppConfig,
  validationService: ValidationService,
  gformConnector: GformConnector,
  processDataService: ProcessDataService[Future]
)(implicit ec: ExecutionContext) {
  def renderTaskList(
    formTemplate: FormTemplate,
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    envelope: EnvelopeWithMapping,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[_],
    hc: HeaderCarrier,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[Html] = {
    val taskCoordinatesMap = TaskListUtils.toTaskCoordinatesMap(cache.formTemplate)
    for {
      statusesLookup <-
        TaskListUtils
          .evalStatusLookup(cache.toCacheData, envelope, formModelOptics, validationService, taskCoordinatesMap)
      taskIdTaskStatusMapping = if (TaskListUtils.hasTaskStatusExpr(cache, formModelOptics)) {
                                  TaskListUtils.evalTaskIdTaskStatusMapping(taskCoordinatesMap, statusesLookup)
                                } else TaskIdTaskStatusMapping.empty
      _ <-
        gformConnector.updateUserData(
          FormIdData(cache.retrievals, cache.formTemplateId, maybeAccessCode),
          UserData(
            cache.form.formData,
            Validated,
            cache.form.visitsIndex,
            cache.form.thirdPartyData,
            cache.form.componentIdToFileId,
            taskIdTaskStatusMapping
          )
        )
      cacheUpd = cache.copy(form = cache.form.copy(taskIdTaskStatus = taskIdTaskStatusMapping))
      newDataRaw = cacheUpd.variadicFormData[SectionSelectorType.Normal]
      processData <- processDataService
                       .getProcessData[SectionSelectorType.Normal](
                         newDataRaw,
                         cacheUpd,
                         gformConnector.getAllTaxPeriods,
                         NoSpecificAction,
                         formModelOptics
                       )
    } yield TaskListUtils.withTaskList(formTemplate) { taskList =>
      val visibleTaskCoordinates: List[Coordinates] = taskCoordinatesMap.collect {
        case (task, coordinates)
            if task.includeIf.forall(formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(_, None)) =>
          coordinates
      }.toList

      val smartStringEvaluatorFactory: SmartStringEvaluatorFactory = new RealSmartStringEvaluatorFactory(messages)
      val formModelVisibilityOptics = processData.formModelOptics.formModelVisibilityOptics

      implicit val sse: SmartStringEvaluator =
        smartStringEvaluatorFactory(DataOrigin.swapDataOrigin(formModelVisibilityOptics))(messages, l)

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
                  taskStatus === TaskStatus.Completed
                )
                .url
            )
        }

      val taskLists: List[TaskListView] = taskList.sections.toList.zipWithIndex.map {
        case (taskSection, taskSectionIndex) =>
          val taskListItems = taskSection.tasks.toList.zipWithIndex.collect {
            case (task, taskIndex)
                if visibleTaskCoordinates.contains(TaskListUtils.evalCoordinates(taskSectionIndex, taskIndex)) =>
              val coordinates = TaskListUtils.evalCoordinates(taskSectionIndex, taskIndex)
              val status: TaskStatus = statusesLookup.toList.toMap.getOrElse(
                coordinates,
                TaskStatus.NotStarted
              )
              new TaskListItem(
                title = new TaskListItemTitle(content = content.Text(task.title.value())),
                hint = task.hint.map(hint => Hint(content = Text(hint.value()))),
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
  }

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

  private def completedTasksCount(statuses: List[(Coordinates, TaskStatus)]): Int =
    statuses.count({ case (_, taskStatus) =>
      taskStatus === TaskStatus.Completed || taskStatus === TaskStatus.NotRequired
    })
}
