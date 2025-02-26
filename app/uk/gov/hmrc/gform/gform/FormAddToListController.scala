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

package uk.gov.hmrc.gform.gform

import cats.instances.future._
import cats.syntax.applicative._
import play.api.data
import play.api.i18n.I18nSupport
import play.api.mvc.AnyContent
import play.api.mvc.MessagesControllerComponents
import play.api.mvc.Request
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.commons.MarkDownUtil.markDownParser
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.gform.processor.FormProcessor
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.gform.NoSpecificAction
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, AtlDescription, FormTemplateId, SectionNumber, TemplateSectionIndex }
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.govukfrontend.views.Aliases.{ Table, TableRow }
import uk.gov.hmrc.govukfrontend.views.html.components
import uk.gov.hmrc.govukfrontend.views.html.components.{ ErrorMessage, GovukTable, Text }
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.ErrorLink
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.ErrorSummary
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

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
    addToListId: AddToListId,
    fastForward: List[FastForward]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit lang => _ => implicit sse => formModelOptics =>
        val formTemplateContext = request.attrs(FormTemplateKey)
        val formTemplate = formTemplateContext.formTemplate

        val formModel = formModelOptics.formModelRenderPageOptics.formModel
        val formAction =
          routes.FormAddToListController
            .confirmRemoval(formTemplateId, maybeAccessCode, sectionNumber, index, addToListId, fastForward)
        val maybeBracket = formModel.bracket(sectionNumber)
        maybeBracket match {
          case bracket @ Bracket.AddToList(iterations, source) =>
            val (pageError, fieldErrors) = {
              val errorMessage =
                source.errorMessage.fold(request.messages.messages("addToList.error.selectOption"))(error =>
                  error.value()
                )

              request.flash.get("removeParamMissing").fold((NoErrors: HasErrors, Map.empty[String, ErrorMessage])) {
                _ =>
                  (
                    Errors(
                      new components.GovukErrorSummary()(
                        ErrorSummary(
                          errorList = List(
                            ErrorLink(
                              href = Some("#remove"),
                              content = content.Text(errorMessage)
                            )
                          ),
                          title = content.Text(request.messages.messages("error.summary.heading"))
                        )
                      )
                    ),
                    Map(
                      "remove" -> ErrorMessage.errorMessageWithDefaultStringsTranslated(
                        content = Text(errorMessage)
                      )
                    )
                  )
              }
            }

            iterations.toList.lift(index) match {
              case Some(iteration) =>
                val repeater = iteration.repeater.repeater
                val description = repeater.expandedDescription match {
                  case AtlDescription.SmartStringBased(ss) => markDownParser(ss)
                  case AtlDescription.KeyValueBased(k, v) =>
                    new GovukTable()(
                      Table(
                        head = None,
                        rows = Seq(
                          Seq(
                            TableRow(content = HtmlContent(markDownParser(k))),
                            TableRow(content = HtmlContent(markDownParser(v)))
                          )
                        ),
                        firstCellIsHeader = true
                      )
                    )
                }
                Ok(
                  html.form
                    .addToList_requestRemoval(
                      formTemplate,
                      description,
                      maybeAccessCode,
                      sectionNumber,
                      frontendAppConfig,
                      formAction,
                      pageError,
                      fieldErrors
                    )
                ).pure[Future]
              case None =>
                val lastSectionNumber = bracket.lastSectionNumber
                Redirect(routes.FormController.formSection(formTemplateId, maybeAccessCode, lastSectionNumber))
                  .pure[Future]
            }

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
    addToListId: AddToListId,
    fastForward: List[FastForward]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        form
          .bindFromRequest()
          .fold(
            _ =>
              Redirect(
                routes.FormAddToListController
                  .requestRemoval(formTemplateId, maybeAccessCode, sectionNumber, index, addToListId, fastForward)
              )
                .flashing("removeParamMissing" -> "true")
                .pure[Future],
            {
              case "Yes" =>
                removeAndRedirect(
                  formModelOptics,
                  cache,
                  maybeAccessCode,
                  sectionNumber.templateSectionIndex,
                  index,
                  addToListId,
                  fastForward
                )
                  .map(_.flashing("success" -> request.messages.messages("generic.successfullyRemoved")))
              case "No" =>
                Redirect(
                  routes.FormController.formSection(formTemplateId, maybeAccessCode, sectionNumber, Option(fastForward))
                )
                  .pure[Future]
            }
          )
    }

  private def removeAndRedirect(
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    templateSectionIndex: TemplateSectionIndex,
    index: Int,
    addToListId: AddToListId,
    fastForward: List[FastForward]
  )(implicit
    request: Request[AnyContent],
    hc: HeaderCarrier,
    lang: LangADT,
    sse: SmartStringEvaluator
  ) =
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
                    fastForward,
                    formModelOptics,
                    processData,
                    templateSectionIndex,
                    index,
                    addToListId
                  )
    } yield redirect

  def removeItem(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    index: Int,
    addToListId: AddToListId,
    fastForward: List[FastForward]
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        removeAndRedirect(
          formModelOptics,
          cache,
          maybeAccessCode,
          sectionNumber.templateSectionIndex,
          index,
          addToListId,
          fastForward
        )
    }
}
