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

import cats.instances.future._
import cats.syntax.applicative._
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.gform.fileupload.{ EnvelopeWithMapping, FileUploadService }
import uk.gov.hmrc.gform.gform.routes.SummaryController
import uk.gov.hmrc.gform.models.{ Coordinates, FastForward, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class TaskListController(
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  taskListRenderingService: TaskListRenderingService,
  fileUploadService: FileUploadService,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {
  import i18nSupport._

  def landingPage(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        for {
          envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)
          html <-
            taskListRenderingService
              .renderTaskList(
                cache.formTemplate,
                maybeAccessCode,
                cache.toCacheData,
                EnvelopeWithMapping(envelope, cache.form),
                formModelOptics
              )
        } yield Ok(html)

    }

  def newTask(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    taskSectionNumber: TaskSectionNumber,
    taskNumber: TaskNumber
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      request => l => cache => implicit sse => formModelOptics =>
        TaskListUtils.withTask(cache.formTemplate, taskSectionNumber, taskNumber) { task =>
          val sectionTitle4Ga: SectionTitle4Ga = SectionTitle4Ga(task.title.value)

          val href = uk.gov.hmrc.gform.gform.routes.FormController.form(
            cache.formTemplate._id,
            maybeAccessCode,
            SectionNumber.TaskList(Coordinates(taskSectionNumber, taskNumber), 0),
            sectionTitle4Ga,
            SuppressErrors.Yes,
            FastForward.Yes
          )
          Redirect(href).pure[Future]
        }
    }

  def summaryPage(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      request => l => cache => sse => formModelOptics =>
        Redirect(
          SummaryController.summaryById(
            formTemplateId,
            maybeAccessCode,
            None // No coordinates means show all tasks in summary page
          )
        ).pure[Future]
    }
}
