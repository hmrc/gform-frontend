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

package uk.gov.hmrc.gform.pdf.model

import play.api.i18n.Messages
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluator, _ }
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions.{ getMonthValue, renderMonth }
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper.formatDate
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ HtmlFieldId, ValidationResult }
import uk.gov.hmrc.gform.pdf.model.PDFModel._
import uk.gov.hmrc.gform.pdf.model.TextFormatter._

object PDFPageFieldBuilder {
  def build[T <: PDFType](
    formComponent: FormComponent,
    cache: AuthCacheWithForm,
    sectionNumber: SectionNumber,
    validationResult: ValidationResult,
    envelopeWithMapping: EnvelopeWithMapping
  )(implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    pdfFunctions: PDFCustomRender[T]
  ): PageField = {
    import pdfFunctions._
    formComponent match {
      case IsText(Text(_, _, _, _, prefix, suffix)) =>
        SimpleField(
          getFormComponentLabel(formComponent),
          formatText(validationResult(formComponent), envelopeWithMapping, prefix, suffix)
        )

      case IsTextArea(_) =>
        SimpleField(
          getFormComponentLabel(formComponent),
          formatText(validationResult(formComponent), envelopeWithMapping)
        )

      case IsUkSortCode(_) =>
        SimpleField(
          getFormComponentLabel(formComponent),
          List(
            Html(
              UkSortCode
                .fields(
                  formComponent.modelComponentId.indexedComponentId
                ) // TODO JoVl, this is weird, let's use MultiValueId instead
                .toList
                .map { fieldId =>
                  validationResult(formComponent).getCurrentValue(HtmlFieldId.pure(fieldId))
                }
                .mkString("-")
            )
          )
        )

      case IsDate(_) =>
        def safeId(atom: Atom) = HtmlFieldId.pure(formComponent.atomicFormComponentId(atom))

        def monthKey = getMonthValue(validationResult(formComponent).getCurrentValue(safeId(Date.month)))

        SimpleField(
          getFormComponentLabel(formComponent),
          List {
            val day = renderMonth(validationResult(formComponent).getCurrentValue(safeId(Date.day)))
            val month = messages(s"date.$monthKey")
            val year = validationResult(formComponent).getCurrentValue(safeId(Date.year))

            Html(s"$day $month $year")
          }
        )

      case IsCalendarDate() =>
        def safeId(atom: Atom) = HtmlFieldId.pure(formComponent.atomicFormComponentId(atom))

        def monthKey = getMonthValue(validationResult(formComponent).getCurrentValue(safeId(CalendarDate.month)))

        SimpleField(
          getFormComponentLabel(formComponent),
          List {
            val day = renderMonth(validationResult(formComponent).getCurrentValue(safeId(CalendarDate.day)))
            val month = messages(s"date.$monthKey")
            Html(s"$day $month")
          }
        )

      case IsTime(_) =>
        SimpleField(
          getFormComponentLabel(formComponent),
          List(Html(validationResult(formComponent).getCurrentValue.getOrElse("")))
        )
      case IsAddress(_) =>
        SimpleField(
          getFormComponentLabel(formComponent),
          Address
            .renderToString(formComponent, validationResult(formComponent))
            .map(HtmlFormat.escape(_))
        )

      case IsOverseasAddress(_) =>
        SimpleField(
          getFormComponentLabel(formComponent),
          OverseasAddress
            .renderToString(formComponent, validationResult(formComponent))
            .map(HtmlFormat.escape(_))
        )

      case IsInformationMessage(_) =>
        SimpleField(None, List.empty)

      case IsFileUpload() =>
        SimpleField(
          getFormComponentLabel(formComponent),
          List(HtmlFormat.escape(envelopeWithMapping.userFileName(formComponent)))
        )

      case IsHmrcTaxPeriod(h) =>
        val periodId = TaxPeriodHelper.formatTaxPeriodOutput(validationResult(formComponent), envelopeWithMapping)
        val maybeObligation = cache.form.thirdPartyData.obligations.findByPeriodKey(h, periodId)

        SimpleField(
          getFormComponentLabel(formComponent).map(_.capitalize),
          List(Html(maybeObligation.fold("Value Lost!") { od =>
            messages("generic.From") + " " + formatDate(od.inboundCorrespondenceFromDate) + " " +
              messages("generic.to") + " " + formatDate(od.inboundCorrespondenceToDate)
          }))
        )

      case IsChoice(choice) =>
        SimpleField(
          getFormComponentLabel(formComponent),
          choice.renderToString(formComponent, validationResult(formComponent)).map(Html(_))
        )

      case IsRevealingChoice(rc) =>
        val selections: List[ChoiceElement] = rc.options
          .zip(validationResult(formComponent).getComponentFieldIndices(formComponent.id))
          .flatMap { case (element, index) =>
            validationResult(formComponent)
              .getOptionalCurrentValue(HtmlFieldId.indexed(formComponent.id, index))
              .map { _ =>
                val filteredFields = doFilter(element.revealingFields)
                val revealingFields = formComponentOrdering
                  .fold(filteredFields)(filteredFields.sorted(_))
                  .map(f => build(f, cache, sectionNumber, validationResult, envelopeWithMapping))
                ChoiceElement(element.choice.value(), revealingFields)
              }
          }
        RevealingChoiceField(
          getFormComponentLabel(formComponent),
          selections
        )

      case IsGroup(group) =>
        val groupFields = group.fields
        val fields = formComponentOrdering.fold(groupFields)(groupFields.sorted(_)).map { f =>
          build(f, cache, sectionNumber, validationResult, envelopeWithMapping)
        }

        GroupField(getFormComponentLabel(formComponent), fields)
    }
  }
}
