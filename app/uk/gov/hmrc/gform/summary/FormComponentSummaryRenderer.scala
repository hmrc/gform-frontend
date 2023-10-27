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

package uk.gov.hmrc.gform.summary

import cats.syntax.all._
import play.api.i18n.Messages
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions.{ getMonthValue, renderMonth }
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper.formatDate
import uk.gov.hmrc.gform.models.ids.ModelPageId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ Atom, FastForward }
import uk.gov.hmrc.gform.monoidHtml
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, Obligations, SmartString }
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, HtmlFieldId, ValidationResult }
import uk.gov.hmrc.gform.views.html.errorInline
import uk.gov.hmrc.gform.views.html.hardcoded.pages.br
import uk.gov.hmrc.gform.views.summary.SummaryListRowHelper.summaryListRow
import uk.gov.hmrc.gform.views.summary.TextFormatter
import uk.gov.hmrc.gform.views.summary.TextFormatter.formatText
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent

object FormComponentSummaryRenderer {
  def summaryListRows[D <: DataOrigin, T <: RenderType](
    formComponent: FormComponent,
    modelPageId: Option[ModelPageId],
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    obligations: Obligations,
    validationResult: ValidationResult,
    envelope: EnvelopeWithMapping,
    addressRecordLookup: AddressRecordLookup,
    iterationTitle: Option[String] = None,
    fastForward0: Option[List[FastForward]]
  )(implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)
    val fastForward = fastForward0.getOrElse(List(FastForward.CYA(SectionOrSummary.FormSummary)))

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
          iterationTitle,
          fastForward,
          formModelVisibilityOptics
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
          iterationTitle,
          fastForward,
          formModelVisibilityOptics
        )

      case IsDate(_) =>
        getDateSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          iterationTitle,
          fastForward
        )

      case IsCalendarDate() =>
        getCalendarDateSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          iterationTitle,
          fastForward
        )

      case IsTaxPeriodDate() =>
        getTaxPeriodDateSummartListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          iterationTitle,
          fastForward
        )

      case IsPostcodeLookup(_) =>
        getPostcodeLookupRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          iterationTitle,
          fastForward,
          addressRecordLookup
        )

      case IsTime(_) =>
        getTimeSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          iterationTitle,
          fastForward
        )

      case IsAddress(_) =>
        getAddressSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          iterationTitle,
          fastForward
        )

      case IsOverseasAddress(_) =>
        getOverseasAddressSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          iterationTitle,
          fastForward
        )

      case IsInformationMessage(_) =>
        List(SummaryListRow())

      case IsTableComp(table) =>
        getTableSummaryListRows(
          table,
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          envelope,
          iterationTitle,
          fastForward
        )

      case IsFileUpload(_) =>
        getFileUploadSummaryListRows(
          formComponent,
          formTemplateId,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          envelope,
          iterationTitle,
          fastForward
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
          iterationTitle,
          fastForward,
          formModelVisibilityOptics
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
          iterationTitle,
          fastForward,
          formModelVisibilityOptics
        )

      case IsRevealingChoice(rc) =>
        getRevealingChoiceSummaryListRows(
          formComponent,
          modelPageId,
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
          iterationTitle,
          fastForward,
          addressRecordLookup
        )

      case IsGroup(group) =>
        getGroupSummaryListRows(
          group,
          formComponent,
          modelPageId,
          formTemplateId,
          formModelVisibilityOptics,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          obligations,
          formFieldValidationResult,
          validationResult,
          envelope,
          iterationTitle,
          fastForward,
          addressRecordLookup
        )
      case IsMiniSummaryList(_) => List[SummaryListRow]()
      case otherFormComponent   => throw new Exception(s"$otherFormComponent is not supported in summary list row")
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

  private def getTextSummaryListRows[T <: RenderType, D <: DataOrigin](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    envelope: EnvelopeWithMapping,
    prefix: Option[SmartString],
    suffix: Option[SmartString],
    iterationTitle: Option[String],
    fastForward: List[FastForward],
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
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

    val value =
      if (hasErrors) errors
      else
        formatText(formFieldValidationResult, envelope, prefix, suffix, formModelVisibilityOptics).map(
          HtmlFormat.escape
        )

    val changeOrViewLabel = if (fieldValue.editable) messages("summary.change") else messages("summary.view")

    List(
      summaryListRow(
        label,
        value.intercalate(br()),
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
                  fastForward
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          ),
        if (fieldValue.onlyShowOnSummary)
          "govuk-summary-list__row--no-actions"
        else
          ""
      )
    )

  }

  private def getTextAreaSummaryListRows[T <: RenderType, D <: DataOrigin](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    envelope: EnvelopeWithMapping,
    iterationTitle: Option[String],
    fastForward: List[FastForward],
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
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

    val currentValueLines =
      formatText(formFieldValidationResult, envelope, formModelVisibilityOptics = formModelVisibilityOptics).flatMap(
        _.split("\\R").toList
      )

    val currentValue =
      currentValueLines.map(HtmlFormat.escape)

    val value = if (hasErrors) errors else currentValue

    List(
      summaryListRow(
        label,
        value.intercalate(br()),
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
                  fastForward
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        },
        if (fieldValue.onlyShowOnSummary)
          "govuk-summary-list__row--no-actions"
        else
          ""
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
    iterationTitle: Option[String],
    fastForward: List[FastForward]
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
        errors.head
      else {
        val day = renderMonth(formFieldValidationResult.getCurrentValue(safeId(Date.day)))
        val month = if (monthKey.trim.nonEmpty) messages(s"date.$monthKey") else ""
        val year = formFieldValidationResult.getCurrentValue(safeId(Date.year))

        HtmlFormat.escape(s"$day $month $year")
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
                  fastForward
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        },
        if (fieldValue.onlyShowOnSummary)
          "govuk-summary-list__row--no-actions"
        else
          ""
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
    iterationTitle: Option[String],
    fastForward: List[FastForward]
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
        errors.head
      else {
        val day = renderMonth(formFieldValidationResult.getCurrentValue(safeId(CalendarDate.day)))
        val month = if (monthKey.trim.nonEmpty) messages(s"date.$monthKey") else ""

        HtmlFormat.escape(s"$day $month")
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
                  fastForward
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        },
        if (fieldValue.onlyShowOnSummary)
          "govuk-summary-list__row--no-actions"
        else
          ""
      )
    )
  }

  private def getTaxPeriodDateSummartListRows[T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    iterationTitle: Option[String],
    fastForward: List[FastForward]
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

    def monthKey = getMonthValue(formFieldValidationResult.getCurrentValue(safeId(TaxPeriodDate.month)))

    val value =
      if (hasErrors)
        errors.head
      else {
        val year = renderMonth(formFieldValidationResult.getCurrentValue(safeId(TaxPeriodDate.year)))
        val month = if (monthKey.trim.nonEmpty) messages(s"date.$monthKey") else ""

        HtmlFormat.escape(s"$month $year")
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
                  fastForward
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        },
        if (fieldValue.onlyShowOnSummary)
          "govuk-summary-list__row--no-actions"
        else
          ""
      )
    )

  }

  private def getPostcodeLookupRows[T <: RenderType](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    iterationTitle: Option[String],
    fastForward: List[FastForward],
    addressRecordLookup: AddressRecordLookup
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

    def printAddress(addressLines: List[String]): Html =
      addressLines
        .filter(_.nonEmpty)
        .map(Html(_))
        .intercalate(br())

    val value =
      if (hasErrors) errors
      else List(addressRecordLookup.lookup(fieldValue.id).map(printAddress).getOrElse(Html("")))

    val changeOrViewLabel = if (fieldValue.editable) messages("summary.change") else messages("summary.view")

    val changeLink =
      if (addressRecordLookup.isEntered(fieldValue.id)) {

        uk.gov.hmrc.gform.addresslookup.routes.AddressLookupController
          .enterAddress(
            formTemplateId,
            maybeAccessCode,
            fieldValue.id,
            sectionNumber,
            SuppressErrors.Yes,
            fastForward
          )
      } else
        uk.gov.hmrc.gform.gform.routes.FormController
          .form(
            formTemplateId,
            maybeAccessCode,
            sectionNumber,
            sectionTitle4Ga,
            SuppressErrors.Yes,
            fastForward
          )

    List(
      summaryListRow(
        label,
        value.intercalate(br()),
        visuallyHiddenText,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else
          List(
            (
              changeLink,
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          ),
        if (fieldValue.onlyShowOnSummary)
          "govuk-summary-list__row--no-actions"
        else
          ""
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
    iterationTitle: Option[String],
    fastForward: List[FastForward]
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

    val value =
      if (hasErrors) errors.head else HtmlFormat.escape(formFieldValidationResult.getCurrentValue.getOrElse(""))

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
                  fastForward
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        },
        if (fieldValue.onlyShowOnSummary)
          "govuk-summary-list__row--no-actions"
        else
          ""
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
    iterationTitle: Option[String],
    fastForward: List[FastForward]
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
      errors.head
    } else {
      Address
        .renderToString(formComponent, formFieldValidationResult)
        .map(HtmlFormat.escape(_))
        .intercalate(br())
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
                  fastForward
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        },
        if (formComponent.onlyShowOnSummary)
          "govuk-summary-list__row--no-actions"
        else
          ""
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
    iterationTitle: Option[String],
    fastForward: List[FastForward]
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
      errors.head
    } else {
      OverseasAddress
        .renderToString(formComponent, formFieldValidationResult)
        .map(HtmlFormat.escape(_))
        .intercalate(br())
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
                  fastForward
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        },
        if (formComponent.onlyShowOnSummary)
          "govuk-summary-list__row--no-actions"
        else
          ""
      )
    )
  }

  private def getTableSummaryListRows[T <: RenderType](
    table: TableComp,
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    envelope: EnvelopeWithMapping,
    iterationTitle: Option[String],
    fastForward: List[FastForward]
  )(implicit
    messages: Messages,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {
    val label = fcrd.label(formComponent)

    val visuallyHiddenText = getVisuallyHiddenText(formComponent)
    val viewLabel = messages("summary.view")

    List(
      summaryListRow(
        label,
        Html(table.summaryValue.value()),
        visuallyHiddenText,
        "",
        "",
        "",
        List(
          (
            uk.gov.hmrc.gform.gform.routes.FormController
              .form(
                formTemplateId,
                maybeAccessCode,
                sectionNumber,
                sectionTitle4Ga,
                SuppressErrors.Yes,
                fastForward
              ),
            viewLabel,
            iterationTitle.fold(viewLabel + " " + label)(it => viewLabel + " " + it + " " + label)
          )
        ),
        ""
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
    iterationTitle: Option[String],
    fastForward: List[FastForward]
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

    val value = if (hasErrors) errors.head else HtmlFormat.escape(envelope.userFileName(formComponent))

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
                  fastForward
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        },
        if (formComponent.onlyShowOnSummary)
          "govuk-summary-list__row--no-actions"
        else
          ""
      )
    )
  }

  private def getHmrcTaxPeriodSummaryListRows[T <: RenderType, D <: DataOrigin](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    obligations: Obligations,
    h: HmrcTaxPeriod,
    envelope: EnvelopeWithMapping,
    iterationTitle: Option[String],
    fastForward: List[FastForward],
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
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
    val periodId = TaxPeriodHelper.formatTaxPeriodOutput(formFieldValidationResult, envelope, formModelVisibilityOptics)

    val maybeObligation = obligations.findByPeriodKey(h, periodId)

    val value =
      if (hasErrors)
        errors.head
      else
        HtmlFormat
          .escape(maybeObligation.fold("Value Lost!") { od =>
            messages("generic.From") + " " + formatDate(od.inboundCorrespondenceFromDate) + " " +
              messages("generic.to") + " " + formatDate(od.inboundCorrespondenceToDate)
          })

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
                  fastForward
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        },
        if (fieldValue.onlyShowOnSummary)
          "govuk-summary-list__row--no-actions"
        else
          ""
      )
    )
  }

  private def getChoiceSummaryListRows[D <: DataOrigin, T <: RenderType](
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    choice: Choice,
    iterationTitle: Option[String],
    fastForward: List[FastForward],
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
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
        errors
      else
        choice
          .renderToString(formComponent, formFieldValidationResult, formModelVisibilityOptics)
          .map(s => uk.gov.hmrc.gform.views.html.hardcoded.pages.pWrapper(HtmlFormat.escape(s)))

    List(
      summaryListRow(
        label,
        HtmlFormat.fill(value),
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
                  fastForward
                ),
              changeOrViewLabel,
              iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
            )
          )
        },
        if (formComponent.onlyShowOnSummary)
          "govuk-summary-list__row--no-actions"
        else
          ""
      )
    )
  }

  private def getRevealingChoiceSummaryListRows[D <: DataOrigin, T <: RenderType](
    fieldValue: FormComponent,
    modelPageId: Option[ModelPageId],
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
    iterationTitle: Option[String],
    fastForward: List[FastForward],
    addressRecordLookup: AddressRecordLookup
  )(implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    fcrd: FormComponentRenderDetails[T]
  ): List[SummaryListRow] = {

    val indices = formFieldValidationResult.getComponentFieldIndices(fieldValue.id)

    val label = fcrd.label(fieldValue)
    val visuallyHiddenText = getVisuallyHiddenText(fieldValue)

    val selectionsWithRevealings: List[(SummaryListRow, List[SummaryListRow])] = rc.options
      .zip(indices)
      .map { case (element, index) =>
        val hasErrors = formFieldValidationResult.isNotOk

        val errors: List[Html] = formFieldValidationResult.fieldErrors.toList.map { e =>
          errorInline("summary", e, Seq())
        }

        val keyClasses = getKeyClasses(hasErrors)

        val value =
          if (hasErrors)
            errors.head
          else
            HtmlFormat.escape(element.choice.label.value())

        formFieldValidationResult
          .getOptionalCurrentValue(HtmlFieldId.indexed(fieldValue.id, index))
          .map { _ =>
            val revealingFields = fcrd.prepareRenderables(element.revealingFields.filterNot(_.hideOnSummary)).flatMap {
              summaryListRows(
                _,
                modelPageId,
                formTemplateId,
                formModelVisibilityOptics,
                maybeAccessCode,
                sectionNumber,
                sectionTitle4Ga,
                obligations,
                validationResult,
                envelope,
                addressRecordLookup,
                None,
                Some(fastForward)
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
                        fastForward
                      ),
                    changeOrViewLabel,
                    iterationTitle
                      .fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
                  )
                )
              },
              if (fieldValue.onlyShowOnSummary)
                "govuk-summary-list__row--no-actions"
              else
                ""
            ) -> revealingFields
          }
      }
      .flatten

    val isSeparate = fieldValue.presentationHint.map(hints => hints.contains(SeparateInSummary)).getOrElse(false)

    val selectionsContent = if (isSeparate) {
      val (optionsSelectionsRows, revealingSelections) = selectionsWithRevealings.reverse
        .foldLeft((List.empty[SummaryListRow], List.empty[SummaryListRow])) {
          case ((accChoiceRows, accRevealingRows), (choiceRow, revealingRows)) =>
            (choiceRow :: accChoiceRows, revealingRows ++ accRevealingRows)
        }

      val squashedOptionsContent =
        HtmlFormat.fill(optionsSelectionsRows.flatMap(row => List(row.value.content.asHtml, Html("<br>"))).dropRight(1))

      optionsSelectionsRows.headOption
        .map(row => row.copy(value = row.value.copy(content = HtmlContent(squashedOptionsContent))))
        .toList ++ revealingSelections

    } else {
      selectionsWithRevealings.flatMap { case (optionsRow, revealingRows) =>
        optionsRow :: revealingRows
      }
    }

    if (selectionsContent.isEmpty) {
      List(
        summaryListRow(
          label,
          HtmlFormat.empty,
          visuallyHiddenText,
          "",
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
                    fastForward
                  ),
                changeOrViewLabel,
                iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
              )
            )
          },
          if (fieldValue.onlyShowOnSummary)
            "govuk-summary-list__row--no-actions"
          else
            ""
        )
      )
    } else selectionsContent
  }

  private def getGroupSummaryListRows[D <: DataOrigin, T <: RenderType](
    group: Group,
    formComponent: FormComponent,
    modelPageId: Option[ModelPageId],
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    obligations: Obligations,
    formFieldValidationResult: FormFieldValidationResult,
    validationResult: ValidationResult,
    envelope: EnvelopeWithMapping,
    iterationTitle: Option[String],
    fastForward: List[FastForward],
    addressRecordLookup: AddressRecordLookup
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
              formFieldValidationResults
                .flatMap(ffvr =>
                  TextFormatter.formatText(ffvr, envelope, formModelVisibilityOptics = formModelVisibilityOptics)
                )
                .map(HtmlFormat.escape)
                .intercalate(br())
            case Some(formFieldValidationResult) =>
              val errors = checkErrors(formComponent, formFieldValidationResult)
              Html(errors.mkString(" "))
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
                      fastForward
                    ),
                  changeOrViewLabel,
                  iterationTitle.fold(changeOrViewLabel + " " + label)(it => changeOrViewLabel + " " + it + " " + label)
                )
              )
            },
            if (formComponent.onlyShowOnSummary)
              "govuk-summary-list__row--no-actions"
            else
              ""
          )
        )

      case _ =>
        val rows = fcrd.prepareRenderables(group.fields).flatMap { formComponent =>
          summaryListRows(
            formComponent,
            modelPageId,
            formTemplateId,
            formModelVisibilityOptics,
            maybeAccessCode,
            sectionNumber,
            sectionTitle4Ga,
            obligations,
            validationResult,
            envelope,
            addressRecordLookup,
            iterationTitle,
            Some(fastForward)
          )
        }

        val label = fcrd.label(formComponent)
        if (label.nonEmpty && formComponent.modelComponentId.maybeIndex.fold(false)(_ === 1)) {
          val customKeyClasses = "summary-group-label"

          summaryListRow(
            label,
            Html(""),
            None,
            customKeyClasses,
            "",
            "",
            Nil,
            "govuk-summary-list__row--no-actions"
          ) :: rows
        } else rows
    }
  }
}

sealed trait RenderType
trait SummaryRender extends RenderType
trait AddToListCYARender extends RenderType

sealed trait FormComponentRenderDetails[T <: RenderType] {
  def label(formComponent: FormComponent)(implicit lise: SmartStringEvaluator, messages: Messages): String
  def prepareRenderables(fields: List[FormComponent]): List[FormComponent]
}

object FormComponentRenderDetails {

  implicit val summaryFormComponentRenderInfo: FormComponentRenderDetails[SummaryRender] =
    new FormComponentRenderDetails[SummaryRender] {

      override def label(
        formComponent: FormComponent
      )(implicit lise: SmartStringEvaluator, messages: Messages): String = getLabel(formComponent)

      override def prepareRenderables(fields: List[FormComponent]): List[FormComponent] =
        fields.filter(f => !f.hideOnSummary)
    }

  implicit val addToListCYARender: FormComponentRenderDetails[AddToListCYARender] =
    new FormComponentRenderDetails[AddToListCYARender] {
      override def label(
        formComponent: FormComponent
      )(implicit lise: SmartStringEvaluator, messages: Messages): String = getLabel(formComponent)

      override def prepareRenderables(fields: List[FormComponent]): List[FormComponent] = fields
    }

  private def getLabel(
    formComponent: FormComponent
  )(implicit lise: SmartStringEvaluator, messages: Messages): String = {
    val optionalPattern = """\(|\)""".r
    val label = formComponent.shortName.map(ls => ls.value()).getOrElse(formComponent.label.value())
    if (formComponent.mandatory) {
      label
    } else if (optionalPattern.findFirstIn(label).isDefined) { label }
    else { s"$label ${messages("summary.label.optional")}" }
  }
}
