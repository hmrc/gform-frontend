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
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.{ BracketsWithSectionNumber, Visibility }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.VariadicValue
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, FormTemplate, HMRCClaimForm, HMRCReturnForm, TaskSectionNumber }
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
      val taskListH2key = formTemplate.formCategory match {
        case HMRCReturnForm => "taskList.h2.formCategory.return"
        case HMRCClaimForm  => "taskList.h2.formCategory.claim"
        case _              => "taskList.h2"

      }
      uk.gov.hmrc.gform.views.html.tasklist
        .task_list(
          formTemplate,
          maybeAccessCode,
          taskList,
          statusesLookup.toList.toMap,
          completedSection,
          frontendAppConfig,
          taskListH2key,
          formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(_, None)
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
            validatedATLs <- validationService.validateATLs(
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

  private def completedTaskSection(statuses: NonEmptyList[(Coordinates, TaskStatus)]): Int = {
    val taskSections: Map[TaskSectionNumber, List[(Coordinates, TaskStatus)]] = statuses.toList.groupBy {
      case (coordinates, _) => coordinates.taskSectionNumber
    }
    taskSections.values.count(_.forall { case (_, taskStatus) =>
      taskStatus === TaskStatus.Completed || taskStatus === TaskStatus.NotRequired
    })
  }
}
