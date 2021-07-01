/*
 * Copyright 2021 HM Revenue & Customs
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

import cats.instances.future._
import cats.syntax.applicative._
import play.api.data
import play.api.i18n.I18nSupport
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.gform.processor.FormProcessor
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.gform.NoSpecificAction
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, FormTemplateId, SectionNumber }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, VariadicFormData }
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class FormAddToListController(
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  processDataService: ProcessDataService[Future],
  gformConnector: GformConnector,
  addToListProcessor: FormProcessor,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  def requestRemoval(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    index: Int,
    addToListId: AddToListId
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit lang => _ => implicit sse => formModelOptics =>
        val formTemplate = request.attrs(FormTemplateKey)
        val formModel = formModelOptics.formModelRenderPageOptics.formModel
        val formAction =
          routes.FormAddToListController
            .confirmRemoval(formTemplateId, maybeAccessCode, sectionNumber, index, addToListId)
        val bracket = formModel.bracket(sectionNumber)
        bracket match {
          case Bracket.AddToList(iterations, _) =>
            Ok(
              html.form
                .addToList_requestRemoval(
                  formTemplate,
                  iterations.toList(index).repeater.repeater,
                  maybeAccessCode,
                  sectionNumber,
                  frontendAppConfig,
                  formAction
                )
            ).pure[Future]
          case _ =>
            throw new IllegalArgumentException(
              "FormAddToListController.requestRemoval can only be requested from AddToList section"
            )
        }

    }

  private val form: data.Form[String] = play.api.data.Form(
    play.api.data.Forms.single(
      "remove" -> play.api.data.Forms.nonEmptyText
    )
  )

  def confirmRemoval(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    index: Int,
    addToListId: AddToListId
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        form
          .bindFromRequest()
          .fold(
            _ => throw new IllegalArgumentException("'remove' param missing in confirmRemoval request"),
            {
              case "Yes" =>
                for {
                  processData <- processDataService
                                   .getProcessData[SectionSelectorType.Normal](
                                     formModelOptics.formModelRenderPageOptics.recData.variadicFormData
                                       .asInstanceOf[VariadicFormData[OutOfDate]],
                                     cache,
                                     formModelOptics,
                                     gformConnector.getAllTaxPeriods,
                                     NoSpecificAction
                                   )
                  redirect <- addToListProcessor.processRemoveAddToList(
                                cache,
                                maybeAccessCode,
                                FastForward.Yes,
                                formModelOptics,
                                processData,
                                index,
                                addToListId
                              )
                } yield redirect
              case "No" =>
                Redirect(routes.FormController.formSection(formTemplateId, maybeAccessCode, sectionNumber)).pure[Future]
            }
          )
    }
}
