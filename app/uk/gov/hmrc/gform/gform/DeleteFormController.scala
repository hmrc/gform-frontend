/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.gform

import org.slf4j.{ Logger, LoggerFactory }
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc._
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.govukfrontend.views.viewmodels.button.Button
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.gform.views.html.hardcoded.pages.confirm_form_delete
import uk.gov.hmrc.govukfrontend.views.viewmodels.content

import scala.concurrent.Future

class DeleteFormController(
  i18nSupport: I18nSupport,
  frontendAppConfig: FrontendAppConfig,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  fastForwardService: FastForwardService,
  messagesControllerComponents: MessagesControllerComponents
) extends FrontendController(messagesControllerComponents) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  import i18nSupport._

  private val noAccessCode = Option.empty[AccessCode]

  def deleteOnExit(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: Option[SectionNumber],
    sectionTitle4Ga: Option[SectionTitle4Ga]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, noAccessCode, OperationWithForm.DeleteForm) {
      implicit request => implicit l => cache => sse => formModelOptics =>
        val formTemplate = cache.formTemplate

        val backLink = uk.gov.hmrc.gform.gform.routes.DeleteFormController
          .cancelConfirmation(formTemplate._id, maybeAccessCode, sectionNumber, sectionTitle4Ga)

        val messages = implicitly[Messages]

        val cancelButton = Button(
          content = content.Text(messages("confirm.delete.button.cancel")),
          inputType = Some("submit"),
          attributes = Map("formaction" -> backLink.path),
          classes = "govuk-button--secondary"
        )

        val confirmHref =
          uk.gov.hmrc.gform.gform.routes.DeleteFormController.confirmDeleteOnExit(formTemplate._id, maybeAccessCode)

        Future.successful(
          Ok(confirm_form_delete(cache.formTemplate, frontendAppConfig, backLink, confirmHref, cancelButton))
        )
    }

  def confirmDeleteOnExit(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth
      .authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.DeleteForm) {
        implicit request => l => cache => sse => formModelOptics =>
          logger.info(s"Deleting form $formTemplateId")
          fastForwardService.deleteForm(formTemplateId, cache, QueryParams.empty)
      }

  def cancelConfirmation(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: Option[SectionNumber],
    sectionTitle4Ga: Option[SectionTitle4Ga]
  ): Action[AnyContent] =
    auth
      .authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.DeleteForm) {
        request => l => cache => sse => formModelOptics =>
          Future.successful(
            Redirect(
              uk.gov.hmrc.gform.gform.routes.SaveAcknowledgementController
                .saveAndExit(formTemplateId, maybeAccessCode, sectionNumber, sectionTitle4Ga)
            )
          )
      }
}
