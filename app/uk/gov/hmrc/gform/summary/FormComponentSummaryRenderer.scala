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

package uk.gov.hmrc.gform.summary

import cats.syntax.eq._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluator, _ }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions.{ getMonthValue, renderMonth }
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper.formatDate
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ Atom, FastForward }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, Obligations }
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, HtmlFieldId, ValidationResult }
import uk.gov.hmrc.gform.views.html.errorInline
import uk.gov.hmrc.gform.views.summary.SummaryListRowHelper.summaryListRow
import uk.gov.hmrc.gform.views.summary.TextFormatter
import uk.gov.hmrc.gform.views.summary.TextFormatter.formatText
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow

object FormComponentSummaryRenderer {

  def summaryListRows[D <: DataOrigin, T <: RenderType](
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    obligations: Obligations,
    validationResult: ValidationResult,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)

    formComponent match {
      case IsText(_) =>
        getTextSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          envelope,
        )

      case IsTextArea(_) =>
        getTextAreaSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          envelope
        )

      case IsUkSortCode(_) =>
        getUkSortCodeSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult)

      case IsDate(_) =>
        getDateSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult)

      case IsTime(_) =>
        getTimeSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult)

      case IsAddress(_) =>
        getAddressSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult)

      case IsInformationMessage(_) =>
        List(SummaryListRow())

      case IsFileUpload() =>
        getFileUploadSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          envelope
        )

      case IsHmrcTaxPeriod(h) =>
        getHmrcTaxPeriodSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          obligations,
          h,
          envelope
        )

      case IsChoice(choice) =>
        getChoiceSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          choice)

      case IsRevealingChoice(rc) =>
        getRevealingChoiceSummaryListRows(
          formComponent,
          formTemplateId,
          formModelVisibilityOptics,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          validationResult,
          rc,
          obligations,
          envelope
        )

      case IsGroup(group) =>
        getGroupSummaryListRows(
          group,
          formComponent,
          formTemplateId,
          formModelVisibilityOptics,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          obligations,
          formFieldValidationResult,
          validationResult,
          envelope
        )
    }
  }

  private def checkErrors(fieldValue: FormComponent, formFieldValidationResult: FormFieldValidationResult) =
    formFieldValidationResult.fieldErrors.toList.map { e =>
      errorInline(s"${fieldValue.id.value}-error-message", e, Seq("error-message"))
    }

  private def getVisuallyHiddenText(fieldValue: FormComponent)(implicit lise: SmartStringEvaluator) =
    Some(fieldValue.shortName.map(ls => ls.value()).getOrElse(fieldValue.label.value()))

  private def getKeyClasses(hasErrors: Boolean) =
    if (hasErrors)
      "summary--error"
    else
      ""

  private def getTextSummaryListRows[T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(fieldValue, formFieldValidationResult)

    val label = fcrd.label(fieldValue)

    val visuallyHiddenText = getVisuallyHiddenText(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)

    val value = if (hasErrors) errors.mkString(" ") else formatText(formFieldValidationResult, envelope)

    List(
      summaryListRow(
        label,
        value,
        visuallyHiddenText,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (fieldValue.editable) messages("summary.change") else messages("summary.view")))
      ))

  }

  private def getTextAreaSummaryListRows[T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(fieldValue, formFieldValidationResult)

    val label = fcrd.label(fieldValue)

    val visuallyHiddenText = getVisuallyHiddenText(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)

    val currentValueLines = formatText(formFieldValidationResult, envelope).split("\\R")

    val currentValue = if (currentValueLines.nonEmpty) {
      currentValueLines.init.map { line =>
        s"$line<br>"
      }.mkString + currentValueLines.last
    } else ""

    val value = if (hasErrors) errors.mkString(" ") else currentValue

    List(
      summaryListRow(
        label,
        value,
        visuallyHiddenText,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (fieldValue.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getUkSortCodeSummaryListRows[T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult
  )(
    implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(fieldValue, formFieldValidationResult)

    val label = fcrd.label(fieldValue)

    val visuallyHiddenText = getVisuallyHiddenText(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)

    val currentValue = UkSortCode
      .fields(fieldValue.modelComponentId.indexedComponentId) // TODO JoVl, this is weird, let's use MultiValueId instead
      .toList
      .map { fieldId =>
        formFieldValidationResult.getCurrentValue(HtmlFieldId.pure(fieldId))
      }
      .mkString("-")

    val value = if (hasErrors) errors.mkString(" ") else currentValue

    List(
      summaryListRow(
        label,
        value,
        visuallyHiddenText,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (fieldValue.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getDateSummaryListRows[T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult
  )(
    implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(fieldValue, formFieldValidationResult)

    val label = fcrd.label(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)

    def safeId(atom: Atom) = HtmlFieldId.pure(fieldValue.atomicFormComponentId(atom))

    def monthKey = getMonthValue(formFieldValidationResult.getCurrentValue(safeId(Date.month)))

    val value =
      if (hasErrors)
        errors.head.toString
      else {
        val day = renderMonth(formFieldValidationResult.getCurrentValue(safeId(Date.day)))
        val month = messages(s"date.$monthKey")
        val year = formFieldValidationResult.getCurrentValue(safeId(Date.year))

        s"$day $month $year"
      }

    List(
      summaryListRow(
        label,
        value,
        None,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (fieldValue.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getTimeSummaryListRows[T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult
  )(
    implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(fieldValue, formFieldValidationResult)

    val label = fcrd.label(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)

    val value = if (hasErrors) errors.head.toString else formFieldValidationResult.getCurrentValue.getOrElse("")

    List(
      summaryListRow(
        label,
        value,
        None,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (fieldValue.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getAddressSummaryListRows[T <: RenderType](
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
  )(
    implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(formComponent, formFieldValidationResult)
    val label = fcrd.label(formComponent).capitalize

    val keyClasses = getKeyClasses(hasErrors)

    val value = if (hasErrors) {
      errors.mkString(" ")
    } else {
      Address
        .renderToString(formComponent, formFieldValidationResult)
        .mkString("", "<br>", "<br>")
    }

    List(
      summaryListRow(
        label,
        value,
        None,
        keyClasses,
        "",
        "",
        if (formComponent.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (formComponent.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getFileUploadSummaryListRows[T <: RenderType](
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = formFieldValidationResult.fieldErrors.toList.map { e =>
      errorInline("summary", e, Seq("error-message"))
    }

    val label = fcrd.label(formComponent)

    val keyClasses = getKeyClasses(hasErrors)

    val value = if (hasErrors) errors.mkString(" ") else envelope.userFileName(formComponent)

    List(
      summaryListRow(
        label,
        value,
        None,
        keyClasses,
        "",
        "",
        if (formComponent.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (formComponent.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getHmrcTaxPeriodSummaryListRows[T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    obligations: Obligations,
    h: HmrcTaxPeriod,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = formFieldValidationResult.fieldErrors.toList.map { e =>
      errorInline("summary", e, Seq())
    }

    val label = fcrd.label(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)
    val periodId = TaxPeriodHelper.formatTaxPeriodOutput(formFieldValidationResult, envelope)

    val maybeObligation = obligations.findByPeriodKey(h, periodId)

    val value =
      if (hasErrors)
        errors.mkString(" ")
      else
        maybeObligation.fold("Value Lost!") { od =>
          messages("generic.From") + " " + formatDate(od.inboundCorrespondenceFromDate) + " " +
            messages("generic.to") + " " + formatDate(od.inboundCorrespondenceToDate)
        }

    List(
      summaryListRow(
        label,
        value,
        None,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (fieldValue.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getChoiceSummaryListRows[T <: RenderType](
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    choice: Choice
  )(
    implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = formFieldValidationResult.fieldErrors.toList.map { e =>
      errorInline("summary", e, Seq())
    }

    val label = fcrd.label(formComponent)

    val keyClasses = getKeyClasses(hasErrors)

    val value =
      if (hasErrors)
        errors.mkString(" ")
      else
        choice.renderToString(formComponent, formFieldValidationResult).map(s => s"<p>$s</p>").mkString

    List(
      summaryListRow(
        label,
        value,
        None,
        keyClasses,
        "",
        "",
        if (formComponent.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (formComponent.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getRevealingChoiceSummaryListRows[D <: DataOrigin, T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    validationResult: ValidationResult,
    rc: RevealingChoice,
    obligations: Obligations,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]): List[SummaryListRow] = {

    val indices = formFieldValidationResult.getComponentFieldIndices(fieldValue.id)

    val selections: List[Option[List[SummaryListRow]]] = rc.options
      .zip(indices)
      .map {
        case (element, index) =>
          val hasErrors = formFieldValidationResult.isNotOk

          val errors = formFieldValidationResult.fieldErrors.toList.map { e =>
            errorInline("summary", e, Seq())
          }

          val label = fcrd.label(fieldValue)

          val keyClasses = getKeyClasses(hasErrors)

          val value =
            if (hasErrors)
              errors.mkString(" ")
            else
              element.choice.value()

          formFieldValidationResult
            .getOptionalCurrentValue(HtmlFieldId.indexed(fieldValue.id, index))
            .map { _ =>
              val revealingFields = fcrd.prepareRenderables(element.revealingFields).flatMap {
                summaryListRows(
                  _,
                  formTemplateId,
                  formModelVisibilityOptics,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  obligations,
                  validationResult,
                  envelope
                )
              }

              summaryListRow(
                label,
                value,
                None,
                keyClasses,
                "",
                "",
                if (fieldValue.onlyShowOnSummary)
                  Nil
                else
                  List(
                    (
                      uk.gov.hmrc.gform.gform.routes.FormController
                        .form(
                          formTemplateId,
                          maybeAccessCode,
                          sectionNumber,
                          sectionTitle4Ga,
                          SuppressErrors.Yes,
                          FastForward.Yes),
                      if (fieldValue.editable) messages("summary.change") else messages("summary.view")))
              ) +: revealingFields
            }
      }

    selections.collect { case Some(v) => v }.flatten
  }

  private def getGroupSummaryListRows[D <: DataOrigin, T <: RenderType](
    group: Group,
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    obligations: Obligations,
    formFieldValidationResult: FormFieldValidationResult,
    validationResult: ValidationResult,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val keyClasses = getKeyClasses(hasErrors)

    val label = group.repeatLabel.map(_.value()).getOrElse(fcrd.label(formComponent))

    val visuallyHiddenText = Some(label)

    formComponent.presentationHint match {
      case Some(hints) if hints.contains(SummariseGroupAsGrid) =>
        val formFieldValidationResults: List[FormFieldValidationResult] =
          fcrd.prepareRenderables(group.fields).map(validationResult.apply)

        val errorResults = formFieldValidationResults.filter(_.isNotOk)

        val value =
          errorResults.headOption match {
            case None =>
              formFieldValidationResults.map(ffvr => s"${TextFormatter.formatText(ffvr, envelope)}<br>").mkString
            case Some(formFieldValidationResult) =>
              val errors = checkErrors(formComponent, formFieldValidationResult)
              errors.mkString(" ")
          }

        if (value.nonEmpty) {
          List(
            summaryListRow(
              label,
              value,
              visuallyHiddenText,
              keyClasses,
              "",
              "",
              if (formComponent.onlyShowOnSummary)
                Nil
              else
                List(
                  (
                    uk.gov.hmrc.gform.gform.routes.FormController
                      .form(
                        formTemplateId,
                        maybeAccessCode,
                        sectionNumber,
                        sectionTitle4Ga,
                        SuppressErrors.Yes,
                        FastForward.Yes),
                    if (formComponent.editable) messages("summary.change") else messages("summary.view")))
            )
          )

        } else List(SummaryListRow())

      case _ =>
        val rows = fcrd.prepareRenderables(group.fields).flatMap { formComponent =>
          summaryListRows(
            formComponent,
            formTemplateId,
            formModelVisibilityOptics,
            maybeAccessCode,
            sectionNumber,
            sectionTitle4Ga,
            obligations,
            validationResult,
            envelope
          )
        }

        val label = fcrd.label(formComponent)
        if (label.nonEmpty && formComponent.modelComponentId.maybeIndex.fold(false)(_ === 1)) {
          val customKeyClasses = "summary-group-label"

          summaryListRow(label, "", None, customKeyClasses, "", "", Nil) :: rows
        } else rows
    }
  }
}

sealed trait RenderType
trait InstructionRender extends RenderType
trait SummaryRender extends RenderType

sealed trait FormComponentRenderDetails[T <: RenderType] {
  def label(formComponent: FormComponent)(implicit lise: SmartStringEvaluator): String
  def prepareRenderables(fields: List[FormComponent]): List[FormComponent]
}

object FormComponentRenderDetails {

  implicit val instructionsFormComponentRenderInfo: FormComponentRenderDetails[InstructionRender] =
    new FormComponentRenderDetails[InstructionRender] {

      override def label(formComponent: FormComponent)(implicit lise: SmartStringEvaluator): String =
        formComponent.instruction.map(_.name.value()).getOrElse("")

      override def prepareRenderables(fields: List[FormComponent]): List[FormComponent] =
        fields
          .filter(f => !f.hideOnSummary && f.instruction.isDefined)
          .sortBy(_.instruction.flatMap(_.order).getOrElse(Integer.MAX_VALUE))
    }

  implicit val summaryFormComponentRenderInfo: FormComponentRenderDetails[SummaryRender] =
    new FormComponentRenderDetails[SummaryRender] {

      override def label(formComponent: FormComponent)(implicit lise: SmartStringEvaluator): String =
        formComponent.shortName.map(ls => ls.value()).getOrElse(formComponent.label.value())

      override def prepareRenderables(fields: List[FormComponent]): List[FormComponent] =
        fields.filter(f => !f.hideOnSummary)
    }
}
