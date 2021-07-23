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
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions.{ getMonthValue, renderMonth }
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper.formatDate
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ Atom, FastForward }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, Obligations, SmartString }
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
    envelope: EnvelopeWithMapping,
    iterationTitle: Option[String] = None
  )(implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)

    formComponent match {
      case IsText(Text(_, _, _, _, prefix, suffix)) =>
        getTextSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          envelope,
          prefix,
          suffix,
          iterationTitle
        )

      case IsTextArea(_) =>
        getTextAreaSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          envelope,
          iterationTitle
        )

      case IsUkSortCode(_) =>
        getUkSortCodeSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          iterationTitle
        )

      case IsDate(_) =>
        getDateSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          iterationTitle
        )

      case IsCalendarDate() =>
        getCalendarDateSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          iterationTitle
        )

      case IsTime(_) =>
        getTimeSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          iterationTitle
        )

      case IsAddress(_) =>
        getAddressSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          iterationTitle
        )

      case IsOverseasAddress(_) =>
        getOverseasAddressSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          iterationTitle
        )

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
          envelope,
          iterationTitle
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
          envelope,
          iterationTitle
        )

      case IsChoice(choice) =>
        getChoiceSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          choice,
          iterationTitle
        )

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
          envelope,
          iterationTitle
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
          envelope,
          iterationTitle
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
    envelope: EnvelopeWithMapping,
    prefix: Option[SmartString],
    suffix: Option[SmartString],
    iterationTitle: Option[String]
  )(implicit
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

    val value = if (hasErrors) errors.mkString(" ") else formatText(formFieldValidationResult, envelope, prefix, suffix)

    val changeOrViewLabel = if (fieldValue.editable) messages("summary.change") else messages("summary.view")

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
                  FastForward.Yes
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
      )
    )

  }

  private def getTextAreaSummaryListRows[T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    envelope: EnvelopeWithMapping,
    iterationTitle: Option[String]
  )(implicit
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
        else {
          val changeOrViewLabel = if (fieldValue.editable) messages("summary.change") else messages("summary.view")
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        }
      )
    )
  }

  private def getUkSortCodeSummaryListRows[T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    iterationTitle: Option[String]
  )(implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(fieldValue, formFieldValidationResult)

    val label = fcrd.label(fieldValue)

    val visuallyHiddenText = getVisuallyHiddenText(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)

    val currentValue = UkSortCode
      .fields(
        fieldValue.modelComponentId.indexedComponentId
      ) // TODO JoVl, this is weird, let's use MultiValueId instead
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
        else {
          val changeOrViewLabel = if (fieldValue.editable) messages("summary.change") else messages("summary.view")
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        }
      )
    )
  }

  private def getDateSummaryListRows[T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    iterationTitle: Option[String]
  )(implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(fieldValue, formFieldValidationResult)

    val label = fcrd.label(fieldValue)

    val visuallyHiddenText = getVisuallyHiddenText(fieldValue)

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
        visuallyHiddenText,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else {
          val changeOrViewLabel = if (fieldValue.editable) messages("summary.change") else messages("summary.view")
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        }
      )
    )
  }

  private def getCalendarDateSummaryListRows[T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    iterationTitle: Option[String]
  )(implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(fieldValue, formFieldValidationResult)

    val label = fcrd.label(fieldValue)

    val visuallyHiddenText = getVisuallyHiddenText(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)

    def safeId(atom: Atom) = HtmlFieldId.pure(fieldValue.atomicFormComponentId(atom))

    def monthKey = getMonthValue(formFieldValidationResult.getCurrentValue(safeId(CalendarDate.month)))

    val value =
      if (hasErrors)
        errors.head.toString
      else {
        val day = renderMonth(formFieldValidationResult.getCurrentValue(safeId(CalendarDate.day)))
        val month = messages(s"date.$monthKey")

        s"$day $month"
      }

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
        else {
          val changeOrViewLabel = if (fieldValue.editable) messages("summary.change") else messages("summary.view")
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        }
      )
    )
  }

  private def getTimeSummaryListRows[T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    iterationTitle: Option[String]
  )(implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(fieldValue, formFieldValidationResult)

    val label = fcrd.label(fieldValue)

    val visuallyHiddenText = getVisuallyHiddenText(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)

    val value = if (hasErrors) errors.head.toString else formFieldValidationResult.getCurrentValue.getOrElse("")

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
        else {
          val changeOrViewLabel = if (fieldValue.editable) messages("summary.change") else messages("summary.view")
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        }
      )
    )
  }

  private def getAddressSummaryListRows[T <: RenderType](
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    iterationTitle: Option[String]
  )(implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(formComponent, formFieldValidationResult)
    val label = fcrd.label(formComponent).capitalize

    val visuallyHiddenText = getVisuallyHiddenText(formComponent)

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
        visuallyHiddenText,
        keyClasses,
        "",
        "",
        if (formComponent.onlyShowOnSummary)
          Nil
        else {
          val changeOrViewLabel = if (formComponent.editable) messages("summary.change") else messages("summary.view")
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        }
      )
    )
  }

  private def getOverseasAddressSummaryListRows[T <: RenderType](
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    iterationTitle: Option[String]
  )(implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(formComponent, formFieldValidationResult)
    val label = fcrd.label(formComponent).capitalize

    val visuallyHiddenText = getVisuallyHiddenText(formComponent)

    val keyClasses = getKeyClasses(hasErrors)

    val value = if (hasErrors) {
      errors.mkString(" ")
    } else {
      OverseasAddress
        .renderToString(formComponent, formFieldValidationResult)
        .mkString("", "<br>", "<br>")
    }

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
        else {
          val changeOrViewLabel = if (formComponent.editable) messages("summary.change") else messages("summary.view")
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        }
      )
    )
  }

  private def getFileUploadSummaryListRows[T <: RenderType](
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    envelope: EnvelopeWithMapping,
    iterationTitle: Option[String]
  )(implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = formFieldValidationResult.fieldErrors.toList.map { e =>
      errorInline("summary", e, Seq("error-message"))
    }

    val label = fcrd.label(formComponent)

    val visuallyHiddenText = getVisuallyHiddenText(formComponent)

    val keyClasses = getKeyClasses(hasErrors)

    val value = if (hasErrors) errors.mkString(" ") else envelope.userFileName(formComponent)

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
        else {
          val changeOrViewLabel = if (formComponent.editable) messages("summary.change") else messages("summary.view")
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        }
      )
    )
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
    envelope: EnvelopeWithMapping,
    iterationTitle: Option[String]
  )(implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = formFieldValidationResult.fieldErrors.toList.map { e =>
      errorInline("summary", e, Seq())
    }

    val label = fcrd.label(fieldValue)

    val visuallyHiddenText = getVisuallyHiddenText(fieldValue)

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
        visuallyHiddenText,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else {
          val changeOrViewLabel = if (fieldValue.editable) messages("summary.change") else messages("summary.view")
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        }
      )
    )
  }

  private def getChoiceSummaryListRows[T <: RenderType](
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    choice: Choice,
    iterationTitle: Option[String]
  )(implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = formFieldValidationResult.fieldErrors.toList.map { e =>
      errorInline("summary", e, Seq())
    }

    val label = fcrd.label(formComponent)

    val visuallyHiddenText = getVisuallyHiddenText(formComponent)

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
        visuallyHiddenText,
        keyClasses,
        "",
        "",
        if (formComponent.onlyShowOnSummary)
          Nil
        else {
          val changeOrViewLabel = if (formComponent.editable) messages("summary.change") else messages("summary.view")
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        }
      )
    )
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
    envelope: EnvelopeWithMapping,
    iterationTitle: Option[String]
  )(implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val indices = formFieldValidationResult.getComponentFieldIndices(fieldValue.id)

    val selections: List[Option[List[SummaryListRow]]] = rc.options
      .zip(indices)
      .map { case (element, index) =>
        val hasErrors = formFieldValidationResult.isNotOk

        val errors = formFieldValidationResult.fieldErrors.toList.map { e =>
          errorInline("summary", e, Seq())
        }

        val label = fcrd.label(fieldValue)

        val visuallyHiddenText = getVisuallyHiddenText(fieldValue)

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
              visuallyHiddenText,
              keyClasses,
              "",
              "",
              if (fieldValue.onlyShowOnSummary)
                Nil
              else {
                val changeOrViewLabel =
                  if (fieldValue.editable) messages("summary.change")
                  else messages("summary.view")
                List(
                  (
                    uk.gov.hmrc.gform.gform.routes.FormController
                      .form(
                        formTemplateId,
                        maybeAccessCode,
                        sectionNumber,
                        sectionTitle4Ga,
                        SuppressErrors.Yes,
                        FastForward.Yes
                      ),
                    changeOrViewLabel,
                    iterationTitle
                      .fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
                  )
                )
              }
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
    envelope: EnvelopeWithMapping,
    iterationTitle: Option[String]
  )(implicit
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
              else {
                val changeOrViewLabel =
                  if (formComponent.editable) messages("summary.change") else messages("summary.view")
                List(
                  (
                    uk.gov.hmrc.gform.gform.routes.FormController
                      .form(
                        formTemplateId,
                        maybeAccessCode,
                        sectionNumber,
                        sectionTitle4Ga,
                        SuppressErrors.Yes,
                        FastForward.Yes
                      ),
                    changeOrViewLabel,
                    iterationTitle.fold(changeOrViewLabel + " " + label)(it =>
                      changeOrViewLabel + " " + it + " " + label
                    )
                  )
                )
              }
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

  implicit val summaryFormComponentRenderInfo: FormComponentRenderDetails[SummaryRender] =
    new FormComponentRenderDetails[SummaryRender] {

      override def label(formComponent: FormComponent)(implicit lise: SmartStringEvaluator): String =
        formComponent.shortName.map(ls => ls.value()).getOrElse(formComponent.label.value())

      override def prepareRenderables(fields: List[FormComponent]): List[FormComponent] =
        fields.filter(f => !f.hideOnSummary)
    }
}
