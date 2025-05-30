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

import cats.syntax.all._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.SaveAndExit
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ DataExpanded, FastForward, Singleton }
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, VariadicValue }
import uk.gov.hmrc.gform.summary.AddressRecordLookup
import uk.gov.hmrc.gform.validation.ValidationResult
import uk.gov.hmrc.govukfrontend.views.viewmodels.button.Button
import uk.gov.hmrc.govukfrontend.views.viewmodels.content

import scala.util.{ Failure, Success, Try }

final case class ExtraInfo(
  singleton: Singleton[DataExpanded],
  maybeAccessCode: Option[AccessCode],
  sectionNumber: SectionNumber,
  formModelOptics: FormModelOptics[DataOrigin.Mongo],
  formTemplate: FormTemplate,
  envelopeId: EnvelopeId,
  envelope: EnvelopeWithMapping,
  formMaxAttachmentSizeMB: Int,
  retrievals: MaterialisedRetrievals,
  formLevelHeading: Boolean,
  specialAttributes: Map[String, String],
  addressRecordLookup: AddressRecordLookup
) {

  val formTemplateId = formTemplate._id
  private val defaultMaxFiles = 5
  private val defaultMultiUploadMandatory = 1

  private val modelComponentIds: List[ModelComponentId] =
    singleton.allFormComponents.flatMap(_.multiValueId.toModelComponentIds)

  val valueLookup: Map[ModelComponentId, Option[VariadicValue]] =
    modelComponentIds
      .map(modelComponentId => (modelComponentId, formModelOptics.pageOpticsData.get(modelComponentId)))
      .toMap

  def getFileIdSingle(formComponent: FormComponent): FileId =
    envelope
      .findSingle(formComponent.modelComponentId)
      .map(_.fileId)
      .getOrElse(
        envelope.mapping.fileIdFor(FileComponentId.Single(formComponent.id))
      )

  def saveAndComeBackLaterButton(implicit messages: Messages) = Button(
    content = content.Text(messages("linkText.saveAndComeBackLater")),
    inputType = Some("submit"),
    attributes = Map(
      "formaction" -> uk.gov.hmrc.gform.gform.routes.FormController
        .updateFormData(formTemplate._id, maybeAccessCode, sectionNumber, List(FastForward.Yes), SaveAndExit)
        .path
    ),
    classes = "govuk-button--secondary"
  )

  private val renderableAsSingleFileUpload: List[FormComponent] = singleton.page.allFields.filterNot {
    case IsInformationMessage(_) => true
    case IsTableComp(_)          => true
    case _                       => false
  }

  def isFileUploadOnlyPage(validationResult: ValidationResult): Option[(FormComponent, FileUpload)] =
    renderableAsSingleFileUpload match {
      // If file is uploaded we want to fallback to standard form implementation
      case (fc @ IsFileUpload(fu)) :: Nil if fc.mandatory && !isAlreadyUploaded(fc, validationResult) =>
        Some(fc -> fu)
      case _ => None
    }

  def getButtonName(validationResult: ValidationResult): Option[String] =
    renderableAsSingleFileUpload match {
      case (fc @ IsFileUpload(_)) :: Nil if fc.mandatory && !isAlreadyUploaded(fc, validationResult) =>
        Some("singleFile")
      case (fc @ IsMultiFileUpload(fu)) :: Nil if isMultiUploadMandatory(fc, fu, validationResult) =>
        Some("multiFile")
      case _ => None
    }

  def maxFilesRequired(maxFiles: Option[Expr]): Int = Try(
    maxFiles
      .map(
        formModelOptics.formModelVisibilityOptics
          .evalAndApplyTypeInfoFirst(_)
          .numberRepresentation
          .fold(defaultMaxFiles)(_.toInt)
      )
      .getOrElse(defaultMaxFiles)
  ) match {
    case Success(maxFiles) => maxFiles
    case Failure(_)        => defaultMaxFiles
  }

  def isAlreadyUploaded(formComponent: FormComponent, validationResult: ValidationResult): Boolean =
    !validationResult(formComponent).getCurrentValue.forall(_ === "")

  private def isMultiUploadMandatory(
    formComponent: FormComponent,
    fileUpload: MultiFileUpload,
    validationResult: ValidationResult
  ): Boolean = {
    val minFilesRequired: Int =
      if (!formComponent.mandatory) 0
      else
        Try(
          fileUpload.minFiles
            .map(
              formModelOptics.formModelVisibilityOptics
                .evalAndApplyTypeInfoFirst(_)
                .numberRepresentation
                .fold(defaultMultiUploadMandatory)(_.toInt)
            )
            .getOrElse(defaultMultiUploadMandatory)
        ) match {
          case Success(minFiles) => minFiles
          case Failure(_)        => defaultMultiUploadMandatory
        }
    val numberUploadedSoFar: Int = validationResult(formComponent).getComponentFieldIndices(formComponent.id).size

    minFilesRequired > numberUploadedSoFar
  }
}
