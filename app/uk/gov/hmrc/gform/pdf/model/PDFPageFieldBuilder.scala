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

package uk.gov.hmrc.gform.pdf.model

import cats.implicits.catsSyntaxEq
import play.api.i18n.Messages
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions.{ getMonthValue, renderMonth }
import uk.gov.hmrc.gform.models.helpers.MiniSummaryListHelper.{ evaluateIncludeIf, getFormattedExprStr }
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper.formatDate
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ HtmlFieldId, ValidationResult }
import uk.gov.hmrc.gform.pdf.model.PDFModel._
import uk.gov.hmrc.gform.pdf.model.TextFormatter._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayInSummary

object PDFPageFieldBuilder {
  def build[T <: PDFType, D <: DataOrigin](
    formComponent: FormComponent,
    cache: AuthCacheWithForm,
    sectionNumber: SectionNumber,
    validationResult: ValidationResult,
    envelopeWithMapping: EnvelopeWithMapping,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    pdfFunctions: PDFCustomRender[T]
  ): List[PageField] = {
    import pdfFunctions._
    formComponent match {
      case IsText(Text(_, _, _, _, prefix, suffix, _)) =>
        List(
          SimpleField(
            getFormComponentLabel(formComponent),
            formatText(validationResult(formComponent), envelopeWithMapping, prefix, suffix, formModelVisibilityOptics)
          )
        )

      case IsTextArea(_) =>
        List(
          SimpleField(
            getFormComponentLabel(formComponent),
            formatText(
              validationResult(formComponent),
              envelopeWithMapping,
              formModelVisibilityOptics = formModelVisibilityOptics
            )
          )
        )

      case IsDate(_) =>
        def safeId(atom: Atom) = HtmlFieldId.pure(formComponent.atomicFormComponentId(atom))

        def monthKey = getMonthValue(validationResult(formComponent).getCurrentValue(safeId(Date.month)))

        List(
          SimpleField(
            getFormComponentLabel(formComponent),
            List {
              val day = renderMonth(validationResult(formComponent).getCurrentValue(safeId(Date.day)))
              val month = messages(s"date.$monthKey")
              val year = validationResult(formComponent).getCurrentValue(safeId(Date.year))

              Html(s"$day $month $year")
            }
          )
        )

      case IsCalendarDate() =>
        def safeId(atom: Atom) = HtmlFieldId.pure(formComponent.atomicFormComponentId(atom))

        def monthKey = getMonthValue(validationResult(formComponent).getCurrentValue(safeId(CalendarDate.month)))

        List(
          SimpleField(
            getFormComponentLabel(formComponent),
            List {
              val day = renderMonth(validationResult(formComponent).getCurrentValue(safeId(CalendarDate.day)))
              val month = messages(s"date.$monthKey")
              Html(s"$day $month")
            }
          )
        )

      case IsPostcodeLookup(_) =>
        val addressLines: List[String] =
          cache.form.thirdPartyData
            .addressLines(formComponent.id)
            .getOrElse(Nil)

        List(
          SimpleField(
            getFormComponentLabel(formComponent),
            addressLines.map(HtmlFormat.escape(_))
          )
        )

      case IsTaxPeriodDate() =>
        def safeId(atom: Atom) = HtmlFieldId.pure(formComponent.atomicFormComponentId(atom))

        def monthKey = getMonthValue(validationResult(formComponent).getCurrentValue(safeId(TaxPeriodDate.month)))

        List(
          SimpleField(
            getFormComponentLabel(formComponent),
            List {
              val year = renderMonth(validationResult(formComponent).getCurrentValue(safeId(TaxPeriodDate.year)))
              val month = messages(s"date.$monthKey")
              Html(s"$month $year")
            }
          )
        )

      case IsTime(_) =>
        List(
          SimpleField(
            getFormComponentLabel(formComponent),
            List(Html(validationResult(formComponent).getCurrentValue.getOrElse("")))
          )
        )
      case IsAddress(_) =>
        List(
          SimpleField(
            getFormComponentLabel(formComponent),
            Address
              .renderToString(formComponent, validationResult(formComponent))
              .map(HtmlFormat.escape(_))
          )
        )

      case IsOverseasAddress(_) =>
        List(
          SimpleField(
            getFormComponentLabel(formComponent),
            OverseasAddress
              .renderToString(formComponent, validationResult(formComponent))
              .map(HtmlFormat.escape(_))
          )
        )

      case IsInformationMessage(InformationMessage(_, _, summaryValue)) =>
        summaryValue.fold(List(SimpleField(None, List.empty)))(sv =>
          List(
            SimpleField(
              getFormComponentLabel(formComponent),
              List(Html(sv.value()))
            )
          )
        )

      case IsTableComp(TableComp(_, _, summaryValue, _, _, _, _)) =>
        List(
          SimpleField(
            getFormComponentLabel(formComponent),
            List(Html(summaryValue.value()))
          )
        )

      case IsMiniSummaryList(msl) =>
        if (msl.displayInSummary === DisplayInSummary.Yes) {
          msl.rows
            .collect {
              case MiniSummaryRow.ValueRow(label, value, includeIf, _, _)
                  if evaluateIncludeIf(includeIf, formModelVisibilityOptics) =>
                value match {
                  case MiniSummaryListValue.AnyExpr(e) =>
                    label -> getFormattedExprStr(formModelVisibilityOptics, e)
                  case MiniSummaryListValue.Reference(e) =>
                    label -> getFormattedExprStr(formModelVisibilityOptics, e)
                }
              case MiniSummaryRow.SmartStringRow(label, value, includeIf, _, _)
                  if evaluateIncludeIf(includeIf, formModelVisibilityOptics) =>
                label -> value.value()
            }
            .map { case (label, value) =>
              SimpleField(
                label.map(lise(_, false)),
                List(Html(value))
              )
            }
        } else {
          List.empty[SimpleField]
        }

      case IsFileUpload(_) =>
        List(
          SimpleField(
            getFormComponentLabel(formComponent),
            List(HtmlFormat.escape(envelopeWithMapping.userFileName(formComponent)))
          )
        )

      case IsMultiFileUpload(_) =>
        List(
          SimpleField(
            getFormComponentLabel(formComponent),
            envelopeWithMapping.userFileNames(formComponent).map(fileName => HtmlFormat.escape(fileName))
          )
        )

      case IsHmrcTaxPeriod(h) =>
        val periodId = TaxPeriodHelper.formatTaxPeriodOutput(
          validationResult(formComponent),
          envelopeWithMapping,
          formModelVisibilityOptics
        )
        val maybeObligation = cache.form.thirdPartyData.obligations.findByPeriodKey(h, periodId)

        List(
          SimpleField(
            getFormComponentLabel(formComponent).map(_.capitalize),
            List(Html(maybeObligation.fold("Value Lost!") { od =>
              messages("generic.From") + " " + formatDate(od.inboundCorrespondenceFromDate) + " " +
                messages("generic.to") + " " + formatDate(od.inboundCorrespondenceToDate)
            }))
          )
        )

      case IsChoice(choice) =>
        List(
          ChoiceField(
            getFormComponentLabel(formComponent),
            choice
              .renderToString(formComponent, validationResult(formComponent), formModelVisibilityOptics)
              .map(HtmlFormat.escape)
          )
        )

      case IsRevealingChoice(rc) =>
        val isSeparate = formComponent.presentationHint.exists(hints => hints.contains(SeparateInSummary))
        val selections: List[ChoiceElement] = rc.options
          .zip(validationResult(formComponent).getComponentFieldIndices(formComponent.id))
          .flatMap { case (element, index) =>
            validationResult(formComponent)
              .getOptionalCurrentValue(HtmlFieldId.indexed(formComponent.id, index))
              .map { _ =>
                val filteredFields = doFilter(element.revealingFields)
                val revealingFields = formComponentOrdering
                  .fold(filteredFields)(filteredFields.sorted(_))
                  .flatMap(f =>
                    build(f, cache, sectionNumber, validationResult, envelopeWithMapping, formModelVisibilityOptics)
                  )
                ChoiceElement(
                  element.choice.summaryValue match {
                    case Some(ss) => ss.value()
                    case _        => element.choice.label.value()
                  },
                  revealingFields
                )
              }
          }
        List(
          RevealingChoiceField(
            getFormComponentLabel(formComponent),
            selections,
            isSeparate
          )
        )

      case IsGroup(group) =>
        val groupFields = group.fields
        val fields = formComponentOrdering.fold(groupFields)(groupFields.sorted(_)).flatMap { f =>
          build(f, cache, sectionNumber, validationResult, envelopeWithMapping, formModelVisibilityOptics)
        }

        List(GroupField(getFormComponentLabel(formComponent), fields))

      case IsButton(_) => List.empty
      case other       => throw new Exception(s"Invalid formComponent in PDFPageFieldBuilder, got $other")
    }
  }
}
