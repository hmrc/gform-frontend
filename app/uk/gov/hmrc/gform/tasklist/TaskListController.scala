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
import uk.gov.hmrc.gform.gform.FastForwardService
import uk.gov.hmrc.gform.gform.routes.SummaryController
import uk.gov.hmrc.gform.gformbackend.GformConnector
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
  messagesControllerComponents: MessagesControllerComponents,
  fastForwardService: FastForwardService,
  gformConnector: GformConnector
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
          envelope          <- fileUploadService.getEnvelope(cache.form.envelopeId)(cache.formTemplate.objectStore)
          notificatioBanner <- gformConnector.notificationBanner
          html <-
            taskListRenderingService
              .renderTaskList(
                cache.formTemplate,
                maybeAccessCode,
                cache.toCacheData,
                EnvelopeWithMapping(envelope, cache.form),
                formModelOptics,
                notificatioBanner
              )
        } yield Ok(html)

    }

  def newTask(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    taskSectionNumber: TaskSectionNumber,
    taskNumber: TaskNumber,
    isCompleted: Boolean
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        TaskListUtils.withTask(cache.formTemplate, taskSectionNumber, taskNumber) { task =>
          val sectionTitle4Ga: SectionTitle4Ga = SectionTitle4Ga(task.title.value)

          def sectionUrl(sectionNumber: SectionNumber) = uk.gov.hmrc.gform.gform.routes.FormController.form(
            cache.formTemplate._id,
            maybeAccessCode,
            sectionNumber,
            sectionTitle4Ga,
            SuppressErrors.Yes,
            List(FastForward.StopAt(sectionNumber.increment))
          )

          if (isCompleted) {
            val isSummarySectionVisible = task.summarySection
              .flatMap(_.includeIf)
              .fold(true)(includeIf => formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None))

            if (task.summarySection.isDefined && isSummarySectionVisible) {
              Redirect(
                uk.gov.hmrc.gform.gform.routes.SummaryController
                  .summaryById(
                    cache.formTemplate._id,
                    maybeAccessCode,
                    Some(Coordinates(taskSectionNumber, taskNumber)),
                    Some(true)
                  )
              ).pure[Future]
            } else {
              val formModel = formModelOptics.formModelVisibilityOptics.formModel
              val nextVisibleSectionNumber = formModel.taskList.nextVisibleSectionNumber(
                SectionNumber.TaskList(Coordinates(taskSectionNumber, taskNumber), 0)
              )
              val isAddToListSectionNumber = formModel.addToListSectionNumbers.contains(nextVisibleSectionNumber)

              val sn =
                if (isAddToListSectionNumber) formModel.addToListRepeaterSectionNumbers.reverse.head
                else nextVisibleSectionNumber

              Redirect(sectionUrl(sn)).pure[Future]
            }
          } else {
            val sn = SectionNumber.TaskList(Coordinates(taskSectionNumber, taskNumber), 0)
            if (cache.formTemplate.isSpecimen) {

              Redirect(sectionUrl(sn)).pure[Future]
            } else {
              fastForwardService
                .redirectFastForward[SectionSelectorType.Normal](
                  cache,
                  maybeAccessCode,
                  formModelOptics,
                  Some(sn)
                )
            }
          }
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
            None, // No coordinates means show all tasks in summary page
            Some(true)
          )
        ).pure[Future]
    }
}
