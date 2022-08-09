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

package uk.gov.hmrc.gform.tasklist

import cats.data.NonEmptyList
import cats.syntax.eq._
import play.api.i18n.Messages
import play.api.mvc.Request
import play.twirl.api.Html
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.{ BracketsWithSectionNumber, Coordinates, Visibility }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.VariadicValue
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, TaskSectionNumber }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.validation.ValidationService
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
      val completedSection = completedTaskSection(statusesLookup)
      uk.gov.hmrc.gform.views.html.tasklist
        .task_list(
          formTemplate,
          maybeAccessCode,
          taskList,
          statusesLookup.toList.toMap,
          completedSection,
          frontendAppConfig
        )
    }

  def availableCoordinates(formModelOptics: FormModelOptics[DataOrigin.Mongo]): NonEmptyList[Coordinates] =
    formModelOptics.formModelVisibilityOptics.formModel.brackets.unsafeToTaskList.brackets.map(_._1)

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

    val cannotStartYetResolver = CannotStartYetResolver.create(formModelOptics.formModelRenderPageOptics.formModel)
    val notRequiredResolver = NotRequiredResolver.create(formModelOptics.formModelVisibilityOptics)
    coordinates
      .traverse { coordinate =>
        val dataForCoordinate: Set[VariadicValue] =
          formModelOptics.formModelVisibilityOptics.data.forCoordinate(coordinate)

        for {
          formHandlerResult <-
            validationService.validateFormModel(
              cache,
              envelope,
              formModelOptics.formModelVisibilityOptics,
              Some(coordinate)
            )
        } yield {
          val taskStatus =
            if (dataForCoordinate.isEmpty) {
              TaskStatus.NotStarted
            } else if (formHandlerResult.isFormValid) {
              TaskStatus.Completed
            } else {
              TaskStatus.InProgress
            }
          coordinate -> taskStatus
        }

      }
      .map(cannotStartYetResolver.resolveCannotStartYet)
      .map(notRequiredResolver.resolveNotRequired)

  }

  private def completedTaskSection(statuses: NonEmptyList[(Coordinates, TaskStatus)]): Int = {
    val taskSections: Map[TaskSectionNumber, List[(Coordinates, TaskStatus)]] = statuses.toList.groupBy {
      case (coordinates, _) => coordinates.taskSectionNumber
    }
    taskSections.values.count(_.forall { case (_, taskStatus) =>
      taskStatus === TaskStatus.Completed || taskStatus === TaskStatus.NotRequired
    })
  }
}
