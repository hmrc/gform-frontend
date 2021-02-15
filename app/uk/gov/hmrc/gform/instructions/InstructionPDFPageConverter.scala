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

package uk.gov.hmrc.gform.instructions
import play.api.i18n.Messages
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluator, _ }
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.instructions.FormModelInstructionSummaryConverter._
import uk.gov.hmrc.gform.instructions.TextFormatter.formatText
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions.{ getMonthValue, renderMonth }
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper.formatDate
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ HtmlFieldId, ValidationResult }

object InstructionPDFPageConverter {

  def convert(
    page: Page[Visibility],
    sectionNumber: SectionNumber,
    cache: AuthCacheWithForm,
    envelopeWithMapping: EnvelopeWithMapping,
    validationResult: ValidationResult)(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    fieldOrdering: Ordering[FormComponent]): Option[PageData] = {
    val pageFields = filteredAndSorted(page.fields)
      .map(c => mapFormComponent(c, cache, sectionNumber, validationResult, envelopeWithMapping))
    if (pageFields.isEmpty)
      None
    else
      Some(PageData(page.instruction.flatMap(_.name).map(_.value()), pageFields))
  }

  def mapFormComponent(
    formComponent: FormComponent,
    cache: AuthCacheWithForm,
    sectionNumber: SectionNumber,
    validationResult: ValidationResult,
    envelopeWithMapping: EnvelopeWithMapping)(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator,
    fieldOrdering: Ordering[FormComponent]): PageField =
    formComponent match {
      case IsText(Text(_, _, _, _, prefix, suffix)) =>
        SimpleField(
          formComponent.instruction.flatMap(_.name.map(_.value())),
          formatText(validationResult(formComponent), envelopeWithMapping, prefix, suffix)
        )

      case IsTextArea(_) =>
        SimpleField(
          formComponent.instruction.flatMap(_.name.map(_.value())),
          formatText(validationResult(formComponent), envelopeWithMapping).flatMap(_.split("\\R"))
        )

      case IsUkSortCode(_) =>
        SimpleField(
          formComponent.instruction.flatMap(_.name.map(_.value())),
          List(
            UkSortCode
              .fields(formComponent.modelComponentId.indexedComponentId) // TODO JoVl, this is weird, let's use MultiValueId instead
              .toList
              .map { fieldId =>
                validationResult(formComponent).getCurrentValue(HtmlFieldId.pure(fieldId))
              }
              .mkString("-"))
        )

      case IsDate(_) =>
        def safeId(atom: Atom) = HtmlFieldId.pure(formComponent.atomicFormComponentId(atom))

        def monthKey = getMonthValue(validationResult(formComponent).getCurrentValue(safeId(Date.month)))

        SimpleField(
          formComponent.instruction.flatMap(_.name.map(_.value())),
          List({
            val day = renderMonth(validationResult(formComponent).getCurrentValue(safeId(Date.day)))
            val month = messages(s"date.$monthKey")
            val year = validationResult(formComponent).getCurrentValue(safeId(Date.year))

            s"$day $month $year"
          })
        )

      case IsTime(_) =>
        SimpleField(
          formComponent.instruction.flatMap(_.name.map(_.value())),
          List(validationResult(formComponent).getCurrentValue.getOrElse(""))
        )
      case IsAddress(_) =>
        SimpleField(
          formComponent.instruction.flatMap(_.name.map(_.value())),
          Address
            .renderToString(formComponent, validationResult(formComponent))
        )

      case IsInformationMessage(_) =>
        SimpleField(None, List.empty)

      case IsFileUpload() =>
        SimpleField(
          formComponent.instruction.flatMap(_.name.map(_.value())),
          List(envelopeWithMapping.userFileName(formComponent))
        )

      case IsHmrcTaxPeriod(h) =>
        val periodId = TaxPeriodHelper.formatTaxPeriodOutput(validationResult(formComponent), envelopeWithMapping)
        val maybeObligation = cache.form.thirdPartyData.obligations.findByPeriodKey(h, periodId)

        SimpleField(
          formComponent.instruction.flatMap(_.name.map(_.value())).map(_.capitalize),
          List(maybeObligation.fold("Value Lost!") { od =>
            messages("generic.From") + " " + formatDate(od.inboundCorrespondenceFromDate) + " " +
              messages("generic.to") + " " + formatDate(od.inboundCorrespondenceToDate)
          })
        )

      case IsChoice(choice) =>
        SimpleField(
          formComponent.instruction.flatMap(_.name.map(_.value())),
          choice.renderToString(formComponent, validationResult(formComponent))
        )

      case IsRevealingChoice(rc) =>
        val selections: List[ChoiceElement] = rc.options
          .zip(validationResult(formComponent).getComponentFieldIndices(formComponent.id))
          .flatMap {
            case (element, index) =>
              validationResult(formComponent)
                .getOptionalCurrentValue(HtmlFieldId.indexed(formComponent.id, index))
                .map { _ =>
                  val revealingFields = filteredAndSorted(element.revealingFields).map(f =>
                    mapFormComponent(f, cache, sectionNumber, validationResult, envelopeWithMapping))
                  ChoiceElement(element.choice.value(), revealingFields)
                }
          }
        RevealingChoiceField(
          formComponent.instruction.flatMap(_.name.map(_.value())),
          selections
        )

      case IsGroup(group) =>
        val fields = group.fields.sorted.map { f =>
          mapFormComponent(f, cache, sectionNumber, validationResult, envelopeWithMapping)
        }

        GroupField(formComponent.instruction.flatMap(_.name.map(_.value())), fields)
    }

  private def filteredAndSorted(fields: List[FormComponent])(
    implicit fieldOrdering: Ordering[FormComponent]): List[FormComponent] =
    fields
      .filter(f => !f.hideOnSummary && f.instruction.isDefined)
      .sorted
}
