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

package uk.gov.hmrc.gform.builder

import cats.implicits._
import play.api.i18n.I18nSupport
import play.api.libs.json.Json
import play.api.mvc.{ MessagesControllerComponents, Result }
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.gform.{ ExtraInfo, RenderUnit, SectionRenderingService }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.{ DataExpanded, FormModel, SectionSelectorType, Singleton }
import uk.gov.hmrc.gform.sharedmodel.{ NotChecked, Obligations }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.summary.AddressRecordLookup
import uk.gov.hmrc.gform.upscan.UpscanInitiate
import uk.gov.hmrc.gform.validation.{ ComponentField, FieldOk, FormFieldValidationResult, HtmlFieldId }
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.gform.validation.ValidationResult
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class BuilderController(
  auth: AuthenticatedRequestActions,
  renderer: SectionRenderingService,
  i18nSupport: I18nSupport,
  gformConnector: GformConnector,
  messagesControllerComponents: MessagesControllerComponents
)(implicit
  ex: ExecutionContext
) extends FrontendController(messagesControllerComponents) {

  def fetchSectionHeaderHtml(formTemplateId: FormTemplateId, sectionNumber: SectionNumber) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        import i18nSupport._
        val formModel: FormModel[DataExpanded] = formModelOptics.formModelRenderPageOptics.formModel
        val singleton = formModel(sectionNumber).asInstanceOf[Singleton[DataExpanded]]

        val sectionHeader = singleton.page.sectionHeader()

        val validationResult = ValidationResult.empty

        val formLevelHeading = renderer.shouldDisplayHeading(singleton, formModelOptics, validationResult)

        val sectionHtml = html.form.section_header(sectionHeader, !formLevelHeading)

        Ok(sectionHtml).pure[Future]
    }

  private def badRequest(error: String): Result = BadRequest(Json.obj("error" -> error))

  def fetchFormComponentHtml(
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    formComponentId: FormComponentId
  ) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.EditForm) {
      implicit request => implicit lang => cache => implicit sse => formModelOptics =>
        import i18nSupport._
        val formModel: FormModel[DataExpanded] = formModelOptics.formModelRenderPageOptics.formModel

        val maybeFormComponent = formModel.fcLookup.get(formComponentId)

        maybeFormComponent match {
          case None => badRequest(s"No component with id = $formComponentId found").pure[Future]
          case Some(formComponent) =>
            val singleton = formModel(sectionNumber).asInstanceOf[Singleton[DataExpanded]]

            val formFieldValidationResult: FormFieldValidationResult = formComponent match {
              case IsOverseasAddress(_) =>
                val data = Map(HtmlFieldId.pure(formComponent.modelComponentId) -> FieldOk(formComponent, ""))

                ComponentField(formComponent, data)
              case IsChoice(_) | IsRevealingChoice(_) =>
                val maybeCheckedOptions: Option[Seq[String]] =
                  formModelOptics.formModelVisibilityOptics.data.many(formComponent.modelComponentId)

                val data = maybeCheckedOptions.fold(Map.empty[HtmlFieldId, FormFieldValidationResult]) {
                  checkedOptions =>
                    checkedOptions.map { index =>
                      HtmlFieldId.indexed(formComponent.id, index) -> FieldOk(formComponent, index)
                    }.toMap
                }
                ComponentField(formComponent, data)
              case _ =>
                val currentValue = formModelOptics.formModelVisibilityOptics.data.one(formComponent.modelComponentId)
                FieldOk(formComponent, currentValue.getOrElse(""))
            }

            val validationResult = ValidationResult(Map(formComponent.id -> formFieldValidationResult), None)

            val formLevelHeading = renderer.shouldDisplayHeading(singleton, formModelOptics, validationResult)

            val renderUnit = RenderUnit.Pure(formComponent)

            val ei: ExtraInfo = ExtraInfo(
              singleton = singleton,
              maybeAccessCode = None,
              sectionNumber = sectionNumber,
              formModelOptics = formModelOptics,
              formTemplate = cache.formTemplate,
              envelopeId = cache.form.envelopeId,
              envelope = EnvelopeWithMapping.empty,
              formMaxAttachmentSizeMB = 10,
              retrievals = cache.retrievals,
              formLevelHeading = formLevelHeading,
              specialAttributes = Map.empty[String, String],
              addressRecordLookup = AddressRecordLookup.from(cache.form.thirdPartyData)
            )
            val obligations: Obligations = NotChecked
            val upscanInitiate: UpscanInitiate = UpscanInitiate.empty

            val formComponentHtml = renderer.htmlFor(
              renderUnit,
              formTemplateId,
              ei,
              validationResult,
              obligations,
              upscanInitiate
            )

            Ok(formComponentHtml).pure[Future]
        }
    }
}
