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
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluator, _ }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.instructions.FormModelSummaryConverter._
import uk.gov.hmrc.gform.instructions.TextFormatter.formatText
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions.{ getMonthValue, renderMonth }
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper.formatDate
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ HtmlFieldId, ValidationResult }

object InstructionsPDFPageFieldConverters {

  implicit val textConverter: PageFieldConverter[Text] = new PageFieldConverter[Text] {
    override def convert(
      formComponent: FormComponent,
      cache: AuthCacheWithForm,
      sectionNumber: SectionNumber,
      validationResult: ValidationResult,
      envelope: Envelope)(implicit lise: SmartStringEvaluator, messages: Messages, l: LangADT): PageField = {
      val (prefix, suffix) = formComponent match {
        case IsText(Text(_, _, _, _, prefix, suffix)) => (prefix, suffix)
        case _                                        => (None, None)
      }

      SimpleField(
        formComponent.instruction.flatMap(_.name.map(_.value())).getOrElse(""),
        formatText(validationResult(formComponent), envelope, prefix, suffix)
      )
    }
  }

  implicit val textAreaConverter: PageFieldConverter[TextArea] = new PageFieldConverter[TextArea] {
    override def convert(
      formComponent: FormComponent,
      cache: AuthCacheWithForm,
      sectionNumber: SectionNumber,
      validationResult: ValidationResult,
      envelope: Envelope)(implicit lise: SmartStringEvaluator, messages: Messages, l: LangADT): PageField =
      SimpleField(
        formComponent.instruction.flatMap(_.name.map(_.value())).getOrElse(""),
        formatText(validationResult(formComponent), envelope).flatMap(_.split("\\R"))
      )
  }

  implicit val ukSortCodeConverter: PageFieldConverter[UkSortCode] = new PageFieldConverter[UkSortCode] {
    override def convert(
      formComponent: FormComponent,
      cache: AuthCacheWithForm,
      sectionNumber: SectionNumber,
      validationResult: ValidationResult,
      envelope: Envelope)(implicit lise: SmartStringEvaluator, messages: Messages, l: LangADT): PageField =
      SimpleField(
        formComponent.instruction.flatMap(_.name.map(_.value())).getOrElse(""),
        List(
          UkSortCode
            .fields(formComponent.modelComponentId.indexedComponentId) // TODO JoVl, this is weird, let's use MultiValueId instead
            .toList
            .map { fieldId =>
              validationResult(formComponent).getCurrentValue(HtmlFieldId.pure(fieldId))
            }
            .mkString("-"))
      )
  }

  implicit val dateConverter: PageFieldConverter[Date] = new PageFieldConverter[Date] {
    override def convert(
      formComponent: FormComponent,
      cache: AuthCacheWithForm,
      sectionNumber: SectionNumber,
      validationResult: ValidationResult,
      envelope: Envelope)(implicit lise: SmartStringEvaluator, messages: Messages, l: LangADT): PageField = {
      def safeId(atom: Atom) = HtmlFieldId.pure(formComponent.atomicFormComponentId(atom))

      def monthKey = getMonthValue(validationResult(formComponent).getCurrentValue(safeId(Date.month)))

      SimpleField(
        formComponent.instruction.flatMap(_.name.map(_.value())).getOrElse(""),
        List({
          val day = renderMonth(validationResult(formComponent).getCurrentValue(safeId(Date.day)))
          val month = messages(s"date.$monthKey")
          val year = validationResult(formComponent).getCurrentValue(safeId(Date.year))

          s"$day $month $year"
        })
      )
    }
  }

  implicit val timeConverter: PageFieldConverter[Time] = new PageFieldConverter[Time] {
    override def convert(
      formComponent: FormComponent,
      cache: AuthCacheWithForm,
      sectionNumber: SectionNumber,
      validationResult: ValidationResult,
      envelope: Envelope)(implicit lise: SmartStringEvaluator, messages: Messages, l: LangADT): PageField =
      SimpleField(
        formComponent.instruction.flatMap(_.name.map(_.value())).getOrElse(""),
        List(validationResult(formComponent).getCurrentValue.getOrElse(""))
      )
  }

  implicit val addressConverter: PageFieldConverter[Address] = new PageFieldConverter[Address] {
    override def convert(
      formComponent: FormComponent,
      cache: AuthCacheWithForm,
      sectionNumber: SectionNumber,
      validationResult: ValidationResult,
      envelope: Envelope)(implicit lise: SmartStringEvaluator, messages: Messages, l: LangADT): PageField =
      SimpleField(
        formComponent.instruction.flatMap(_.name.map(_.value())).getOrElse(""),
        Address
          .renderToString(formComponent, validationResult(formComponent))
      )
  }

  implicit val informationMessageConverter: PageFieldConverter[InformationMessage] =
    new PageFieldConverter[InformationMessage] {
      override def convert(
        formComponent: FormComponent,
        cache: AuthCacheWithForm,
        sectionNumber: SectionNumber,
        validationResult: ValidationResult,
        envelope: Envelope)(implicit lise: SmartStringEvaluator, messages: Messages, l: LangADT): PageField =
        SimpleField("", List.empty)
    }

  implicit val fileUploadConverter: PageFieldConverter[FileUpload] = new PageFieldConverter[FileUpload] {
    override def convert(
      formComponent: FormComponent,
      cache: AuthCacheWithForm,
      sectionNumber: SectionNumber,
      validationResult: ValidationResult,
      envelope: Envelope)(implicit lise: SmartStringEvaluator, messages: Messages, l: LangADT): PageField =
      SimpleField(
        formComponent.instruction.flatMap(_.name.map(_.value())).getOrElse(""),
        List(envelope.userFileName(formComponent))
      )
  }

  implicit val hmrcTaxPeriodConverter: PageFieldConverter[HmrcTaxPeriod] =
    new PageFieldConverter[HmrcTaxPeriod] {
      override def convert(
        formComponent: FormComponent,
        cache: AuthCacheWithForm,
        sectionNumber: SectionNumber,
        validationResult: ValidationResult,
        envelope: Envelope)(implicit lise: SmartStringEvaluator, messages: Messages, l: LangADT): PageField = {

        val periodId = TaxPeriodHelper.formatTaxPeriodOutput(validationResult(formComponent), envelope)
        val mayBeHmrcTaxPeriod = formComponent.`type`.cast[HmrcTaxPeriod]
        val maybeObligation =
          mayBeHmrcTaxPeriod.flatMap(h => cache.form.thirdPartyData.obligations.findByPeriodKey(h, periodId))

        SimpleField(
          formComponent.instruction.flatMap(_.name.map(_.value())).getOrElse("").capitalize,
          List(maybeObligation.fold("Value Lost!") { od =>
            messages("generic.From") + " " + formatDate(od.inboundCorrespondenceFromDate) + " " +
              messages("generic.to") + " " + formatDate(od.inboundCorrespondenceToDate)
          })
        )
      }
    }

  implicit val choiceConverter: PageFieldConverter[Choice] = new PageFieldConverter[Choice] {
    override def convert(
      formComponent: FormComponent,
      cache: AuthCacheWithForm,
      sectionNumber: SectionNumber,
      validationResult: ValidationResult,
      envelope: Envelope)(implicit lise: SmartStringEvaluator, messages: Messages, l: LangADT): PageField =
      SimpleField(
        formComponent.instruction.flatMap(_.name.map(_.value())).getOrElse(""),
        formComponent.`type`.asInstanceOf[Choice].renderToString(formComponent, validationResult(formComponent))
      )
  }

  implicit val revealingChoiceConverter: PageFieldConverter[RevealingChoice] =
    new PageFieldConverter[RevealingChoice] {
      override def convert(
        formComponent: FormComponent,
        cache: AuthCacheWithForm,
        sectionNumber: SectionNumber,
        validationResult: ValidationResult,
        envelope: Envelope)(implicit lise: SmartStringEvaluator, messages: Messages, l: LangADT): PageField = {
        val rc = formComponent.`type`.asInstanceOf[RevealingChoice]
        val selections: List[ChoiceElement] = rc.options
          .zip(validationResult(formComponent).getComponentFieldIndices(formComponent.id))
          .flatMap {
            case (element, index) =>
              validationResult(formComponent)
                .getOptionalCurrentValue(HtmlFieldId.indexed(formComponent.id, index))
                .map { _ =>
                  val revealingFields = element.revealingFields.map(f =>
                    FormModelSummaryConverter.mapFormComponent(f, cache, sectionNumber, validationResult, envelope))
                  ChoiceElement(element.choice.value(), revealingFields)
                }
          }
        RevealingChoiceField(
          formComponent.instruction.flatMap(_.name.map(_.value())).getOrElse(""),
          selections
        )
      }
    }

  implicit val groupConverter: PageFieldConverter[Group] =
    new PageFieldConverter[Group] {
      override def convert(
        formComponent: FormComponent,
        cache: AuthCacheWithForm,
        sectionNumber: SectionNumber,
        validationResult: ValidationResult,
        envelope: Envelope)(implicit lise: SmartStringEvaluator, messages: Messages, l: LangADT): PageField = {

        val fields = formComponent.`type`.asInstanceOf[Group].fields.map { f =>
          FormModelSummaryConverter.mapFormComponent(f, cache, sectionNumber, validationResult, envelope)
        }

        GroupField(formComponent.instruction.flatMap(_.name.map(_.value())).getOrElse(""), fields)
      }
    }
}
