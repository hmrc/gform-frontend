/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import org.intellij.markdown.flavours.gfm.GFMFlavourDescriptor
import org.intellij.markdown.html.HtmlGenerator
import org.intellij.markdown.parser.MarkdownParser
import play.twirl.api.Html
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.models.helpers.Javascript.fieldJavascript
import uk.gov.hmrc.gform.prepop.PrepopService
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PageShader(
    formId: FormId,
    sectionNumber: SectionNumber,
    fieldData: Map[FieldId, Seq[String]],
    formTemplate: FormTemplate,
    f: Option[FieldValue => Option[FormFieldValidationResult]],
    repeatService: RepeatingComponentService,
    envelope: Envelope,
    envelopeId: EnvelopeId,
    prepopService: PrepopService,
    dynamicSections: List[Section],
    formMaxAttachmentSizeMB: Int,
    contentTypes: List[ContentType]
)(implicit retrievals: Retrievals, hc: HeaderCarrier) {

  def render(): Future[PageForRender] = {
    val section = dynamicSections(sectionNumber.value)
    for {
      snippets <- Future.sequence(section.fields.map(f => htmlFor(f, 0)))
      javascript = fieldJavascript(dynamicSections.flatMap(_.atomicFields(repeatService)))
      hiddenTemplateFields = dynamicSections.filterNot(_ == section).flatMap(_.atomicFields(repeatService))
      hiddenSnippets = Fields.toFormField(fieldData, hiddenTemplateFields, repeatService).map(formField => uk.gov.hmrc.gform.views.html.hidden_field(formField))
    } yield PageForRender(formId, sectionNumber, section.title, section.description, hiddenSnippets, snippets, javascript, envelopeId, formMaxAttachmentSizeMB, contentTypes)
  }

  private def htmlFor(fieldValue: FieldValue, index: Int): Future[Html] = {
    fieldValue.`type` match {
      case sortCode @ UkSortCode(expr) => htmlForSortCode(fieldValue, sortCode, expr, index)
      case g @ Group(_, _, _, _, _, _) => htmlForGroup(g, fieldValue, index)
      case Date(_, offset, dateValue) => htmlForDate(fieldValue, offset, dateValue, index)
      case Address(international) => htmlForAddress(fieldValue, international, index)
      case t @ Text(_, expr, _) => htmlForText(fieldValue, t, expr, index)
      case Choice(choice, options, orientation, selections, optionalHelpText) => htmlForChoice(fieldValue, choice, options, orientation, selections, optionalHelpText, index)
      case FileUpload() => htmlForFileUpload(fieldValue, index)
      case InformationMessage(infoType, infoText) => htmlForInformationMessage(fieldValue, infoType, infoText, index)
    }
  }

  private def htmlForInformationMessage(fieldValue: FieldValue, infoType: InfoType, infoText: String, index: Int) = {
    val flavour = new GFMFlavourDescriptor
    val parsedTree = new MarkdownParser(flavour).buildMarkdownTreeFromString(infoText)
    val parsedMarkdownText = new HtmlGenerator(infoText, parsedTree, flavour, false).generateHtml
    Future.successful(uk.gov.hmrc.gform.views.html.field_template_info(fieldValue, infoType, Html(parsedMarkdownText), index))
  }

  private def htmlForFileUpload(fieldValue: FieldValue, index: Int) = {
    validate(fieldValue).map { validatedValue =>
      uk.gov.hmrc.gform.views.html.field_template_file_upload(formId, sectionNumber, fieldValue, validatedValue, index, formMaxAttachmentSizeMB)
    }
  }

  private def htmlForChoice(fieldValue: FieldValue, choice: ChoiceType, options: NonEmptyList[String], orientation: Orientation, selections: List[Int], optionalHelpText: Option[List[String]], index: Int) = {
    val prepopValues = fieldData.get(fieldValue.id) match {
      case None => selections.map(_.toString).toSet
      case Some(_) => Set.empty[String] // Don't prepop something we already submitted
    }

    val snippetF = validate(fieldValue).map { validatedValue =>
      choice match {
        case Radio | YesNo => uk.gov.hmrc.gform.views.html.choice("radio", fieldValue, options, orientation, prepopValues, validatedValue, optionalHelpText, index)
        case Checkbox => uk.gov.hmrc.gform.views.html.choice("checkbox", fieldValue, options, orientation, prepopValues, validatedValue, optionalHelpText, index)
        case Inline => uk.gov.hmrc.gform.views.html.choiceInline(fieldValue, options, prepopValues, validatedValue, optionalHelpText, index)
      }
    }

    snippetF
  }

  private def htmlForText(fieldValue: FieldValue, t: Text, expr: Expr, index: Int) = {
    val prepopValueF = fieldData.get(fieldValue.id) match {
      case None => prepopService.prepopData(expr, formTemplate._id)
      case _ => Future.successful("") // Don't prepop something we already submitted
    }
    val validatedValueF = validate(fieldValue)

    for {
      prepopValue <- prepopValueF
      validatedValue <- validatedValueF
    } yield uk.gov.hmrc.gform.views.html.field_template_text(fieldValue, t, prepopValue, validatedValue, index)
  }

  private def htmlForSortCode(fieldValue: FieldValue, sC: UkSortCode, expr: Expr, index: Int) = {
    val prepopValueF = fieldData.get(fieldValue.id) match {
      case None => prepopService.prepopData(expr, formTemplate._id)
      case _ => Future.successful("") // Don't prepop something we already submitted
    }
    val validatedValueF = validate(fieldValue)

    for {
      prepopValue <- prepopValueF
      validatedValue <- validatedValueF
    } yield uk.gov.hmrc.gform.views.html.field_template_sort_code(fieldValue, sC, prepopValue, validatedValue, index)

  }

  private def htmlForAddress(fieldValue: FieldValue, international: Boolean, index: Int) = {
    validate(fieldValue).map { validatedValue =>
      uk.gov.hmrc.gform.views.html.field_template_address(international, fieldValue, validatedValue, index)
    }
  }

  private def htmlForDate(fieldValue: FieldValue, offset: Offset, dateValue: Option[DateValue], index: Int) = {
    val prepopValues = dateValue.map(DateExpr.fromDateValue).map(DateExpr.withOffset(offset, _))
    validate(fieldValue).map { validatedValue =>
      uk.gov.hmrc.gform.views.html.field_template_date(fieldValue, validatedValue, prepopValues, index)
    }
  }

  private def htmlForGroup(grp: Group, fieldValue: FieldValue, index: Int): Future[Html] = {
    val fgrpHtml = htmlForGroup0(grp, fieldValue, index)

    fieldValue.presentationHint.map(_.contains(CollapseGroupUnderLabel)) match {
      case Some(true) => fgrpHtml.map(grpHtml => uk.gov.hmrc.gform.views.html.collapsable(fieldValue.id, fieldValue.label, grpHtml, FormDataHelpers.dataEnteredInGroup(grp, fieldData)))
      case _ => fgrpHtml
    }
  }

  private def htmlForGroup0(groupField: Group, fieldValue: FieldValue, index: Int) = {
    for {
      (lhtml, limitReached) <- getGroupForRendering(fieldValue, groupField, groupField.orientation)
    } yield uk.gov.hmrc.gform.views.html.group(fieldValue, groupField, lhtml, groupField.orientation, limitReached, index)
  }

  private def getGroupForRendering(fieldValue: FieldValue, groupField: Group, orientation: Orientation): Future[(List[Html], Boolean)] = {
    if (groupField.repeatsMax.isDefined) {
      repeatService.getRepeatingGroupsForRendering(fieldValue, groupField).flatMap {
        case (groupList, isLimit) =>
          Future.sequence((1 to groupList.size).map { count =>
            Future.sequence(groupList(count - 1).map(fv =>
              htmlFor(fv, count))).map { lhtml =>
              uk.gov.hmrc.gform.views.html.group_element(fieldValue, groupField, lhtml, orientation, count, count == 1)
            }
          }.toList).map(a => (a, isLimit))
      }
    } else {
      Future.sequence(groupField.fields.map(fv => htmlFor(fv, 0))).map(a => (a, true))
    }
  }

  private def validate(fieldValue: FieldValue): Future[Option[FormFieldValidationResult]] = {
    repeatService.getAllSections(formTemplate, fieldData).map { sections =>
      val section = sections(sectionNumber.value)
      lazy val okF: FieldValue => Option[FormFieldValidationResult] =
        Fields.okValues(fieldData, section.atomicFields(repeatService), repeatService, envelope)
      f.getOrElse(okF)(fieldValue)
    }
  }
}

