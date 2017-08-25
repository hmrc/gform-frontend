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

package uk.gov.hmrc.gform.service

import javax.inject.{ Inject, Singleton }

import cats.data.NonEmptyList
import org.intellij.markdown.flavours.gfm.GFMFlavourDescriptor
import org.intellij.markdown.html.HtmlGenerator
import org.intellij.markdown.parser.MarkdownParser
import play.api.i18n.Messages
import play.api.mvc.Request
import play.twirl.api.Html
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.models.helpers.Javascript._
import uk.gov.hmrc.gform.models.{ DateExpr, FormFieldValidationResult, SectionRenderingInformation }
import uk.gov.hmrc.gform.prepop.{ PrepopModule, PrepopService }
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class SectionRenderingService @Inject() (repeatService: RepeatingComponentService, prePopModule: PrepopModule) {
  val prepopService: PrepopService = prePopModule.prepopService

  case class ExtraInfo(
    formId: FormId,
    sectionNumber: SectionNumber,
    fieldData: Map[FieldId, Seq[String]],
    formTemplate: FormTemplate,
    f: Option[FieldValue => Option[FormFieldValidationResult]],
    envelope: Envelope,
    dynamicSections: List[BaseSection],
    formMaxAttachmentSizeMB: Int
  )

  def renderSection(
    formId: FormId,
    sectionNumber: SectionNumber,
    fieldData: Map[FieldId, Seq[String]],
    formTemplate: FormTemplate,
    f: Option[FieldValue => Option[FormFieldValidationResult]],
    envelope: Envelope,
    envelopeId: EnvelopeId,
    dynamicSections: List[Section],
    formMaxAttachmentSizeMB: Int,
    contentTypes: List[ContentType]
  )(implicit hc: HeaderCarrier, request: Request[_], messages: Messages, retrievals: Retrievals): Future[Html] = {

    val ei = ExtraInfo(formId, sectionNumber, fieldData, formTemplate, f, envelope, dynamicSections, formMaxAttachmentSizeMB)
    val section = dynamicSections(sectionNumber.value)
    val actionForm = uk.gov.hmrc.gform.controllers.routes.FormController.updateFormData(formId, sectionNumber)

    for {
      snippets <- Future.sequence(section.fields.map(fieldValue => htmlFor(fieldValue, 0, ei)))
      javascript = createJavascript(dynamicSections.flatMap(_.fields), dynamicSections.flatMap(repeatService.atomicFields))
      hiddenTemplateFields = dynamicSections.filterNot(_ == section).flatMap(repeatService.atomicFields)
      hiddenSnippets = Fields.toFormField(fieldData, hiddenTemplateFields).map(formField => uk.gov.hmrc.gform.views.html.hidden_field(formField))
      renderingInfo = SectionRenderingInformation(formId, sectionNumber, section.title, section.description, hiddenSnippets, snippets, javascript, envelopeId, actionForm, true, "Save and Continue", formMaxAttachmentSizeMB, contentTypes)
    } yield uk.gov.hmrc.gform.views.html.form(formTemplate, renderingInfo, formId)
  }

  def renderDeclarationSection(formId: FormId, formTemplate: FormTemplate, f: Option[FieldValue => Option[FormFieldValidationResult]])(implicit hc: HeaderCarrier, request: Request[_], messages: Messages, retrievals: Retrievals): Future[Html] = {

    val ei = ExtraInfo(formId, SectionNumber(0), Map.empty, formTemplate, f, Envelope(Nil), List(formTemplate.declarationSection), 0)

    for {
      snippets <- Future.sequence(formTemplate.declarationSection.fields.map(fieldValue => htmlFor(fieldValue, 0, ei)))
      renderingInfo = SectionRenderingInformation(formId, SectionNumber(0), formTemplate.declarationSection.title, formTemplate.declarationSection.description, Nil, snippets, "", EnvelopeId(""), uk.gov.hmrc.gform.controllers.routes.DeclarationController.submitDeclaration(formId), false, "Confirm and send", 0, Nil)
    } yield uk.gov.hmrc.gform.views.html.form(formTemplate, renderingInfo, formId)
  }

  private def createJavascript(fieldList: List[FieldValue], atomicFields: List[FieldValue]) = {
    val groups: List[(FieldId, Group)] = fieldList.map(fv => (fv.id, fv.`type`)).collect {
      case (fieldId, group: Group) => (fieldId, group)
    }
    groups.map { case (fieldId, group) => collapsingGroupJavascript(fieldId, group) }.mkString(";\n") + fieldJavascript(atomicFields)
  }

  private def htmlFor(fieldValue: FieldValue, index: Int, ei: ExtraInfo)(implicit hc: HeaderCarrier, request: Request[_], messages: Messages, retrievals: Retrievals): Future[Html] = {
    fieldValue.`type` match {
      case sortCode @ UkSortCode(expr) => htmlForSortCode(fieldValue, sortCode, expr, index, ei)
      case g @ Group(_, _, _, _, _, _) => htmlForGroup(g, fieldValue, index, ei)
      case Date(_, offset, dateValue) => Future.successful(htmlForDate(fieldValue, offset, dateValue, index, ei))
      case Address(international) => Future.successful(htmlForAddress(fieldValue, international, index, ei))
      case t @ Text(_, expr) => htmlForText(fieldValue, t, expr, index, ei)
      case Choice(choice, options, orientation, selections, optionalHelpText) => Future.successful(htmlForChoice(fieldValue, choice, options, orientation, selections, optionalHelpText, index, ei))
      case FileUpload() => Future.successful(htmlForFileUpload(fieldValue, index, ei))
      case InformationMessage(infoType, infoText) => htmlForInformationMessage(fieldValue, infoType, infoText, index, ei)
    }
  }

  private def htmlForInformationMessage(fieldValue: FieldValue, infoType: InfoType, infoText: String, index: Int, ei: ExtraInfo) = {
    val flavour = new GFMFlavourDescriptor
    val parsedTree = new MarkdownParser(flavour).buildMarkdownTreeFromString(infoText)
    val parsedMarkdownText = new HtmlGenerator(infoText, parsedTree, flavour, false).generateHtml
    Future.successful(uk.gov.hmrc.gform.views.html.field_template_info(fieldValue, infoType, Html(parsedMarkdownText), index))
  }

  private def htmlForFileUpload(fieldValue: FieldValue, index: Int, ei: ExtraInfo)(implicit hc: HeaderCarrier) = {
    uk.gov.hmrc.gform.views.html.field_template_file_upload(ei.formId, ei.sectionNumber, fieldValue, validate(fieldValue, ei), index, ei.formMaxAttachmentSizeMB)
  }

  private def htmlForChoice(fieldValue: FieldValue, choice: ChoiceType, options: NonEmptyList[String], orientation: Orientation, selections: List[Int], optionalHelpText: Option[List[String]], index: Int, ei: ExtraInfo)(implicit hc: HeaderCarrier) = {
    val prepopValues = ei.fieldData.get(fieldValue.id) match {
      case None => selections.map(_.toString).toSet
      case Some(_) => Set.empty[String] // Don't prepop something we already submitted
    }

    val validatedValue = validate(fieldValue, ei)

    choice match {
      case Radio | YesNo => uk.gov.hmrc.gform.views.html.choice("radio", fieldValue, options, orientation, prepopValues, validatedValue, optionalHelpText, index)
      case Checkbox => uk.gov.hmrc.gform.views.html.choice("checkbox", fieldValue, options, orientation, prepopValues, validatedValue, optionalHelpText, index)
      case Inline => uk.gov.hmrc.gform.views.html.choiceInline(fieldValue, options, prepopValues, validatedValue, optionalHelpText, index)
    }
  }

  private def htmlForText(fieldValue: FieldValue, t: Text, expr: Expr, index: Int, ei: ExtraInfo)(implicit hc: HeaderCarrier, retrievals: Retrievals) = {
    val prepopValueF = ei.fieldData.get(fieldValue.id) match {
      case None => prepopService.prepopData(expr, ei.formTemplate._id)
      case _ => Future.successful("") // Don't prepop something we already submitted
    }
    val validatedValue = validate(fieldValue, ei)

    for {
      prepopValue <- prepopValueF
    } yield uk.gov.hmrc.gform.views.html.field_template_text(fieldValue, t, prepopValue, validatedValue, index)
  }

  private def htmlForSortCode(fieldValue: FieldValue, sC: UkSortCode, expr: Expr, index: Int, ei: ExtraInfo)(implicit hc: HeaderCarrier, retrievals: Retrievals) = {
    val prepopValueF = ei.fieldData.get(fieldValue.id) match {
      case None => prepopService.prepopData(expr, ei.formTemplate._id)
      case _ => Future.successful("") // Don't prepop something we already submitted
    }
    val validatedValue = validate(fieldValue, ei)

    for {
      prepopValue <- prepopValueF
    } yield uk.gov.hmrc.gform.views.html.field_template_sort_code(fieldValue, sC, prepopValue, validatedValue, index)

  }

  private def htmlForAddress(fieldValue: FieldValue, international: Boolean, index: Int, ei: ExtraInfo)(implicit hc: HeaderCarrier) = {
    uk.gov.hmrc.gform.views.html.field_template_address(international, fieldValue, validate(fieldValue, ei), index)
  }

  private def htmlForDate(fieldValue: FieldValue, offset: Offset, dateValue: Option[DateValue], index: Int, ei: ExtraInfo)(implicit hc: HeaderCarrier) = {
    val prepopValues = dateValue.map(DateExpr.fromDateValue).map(DateExpr.withOffset(offset, _))
    uk.gov.hmrc.gform.views.html.field_template_date(fieldValue, validate(fieldValue, ei), prepopValues, index)
  }

  private def htmlForGroup(grp: Group, fieldValue: FieldValue, index: Int, ei: ExtraInfo)(implicit hc: HeaderCarrier, request: Request[_], messages: Messages, retrievals: Retrievals): Future[Html] = {
    val fgrpHtml = htmlForGroup0(grp, fieldValue, index, ei)

    fieldValue.presentationHint.map(_.contains(CollapseGroupUnderLabel)) match {
      case Some(true) => fgrpHtml.map(grpHtml => uk.gov.hmrc.gform.views.html.collapsable(fieldValue.id, fieldValue.label, grpHtml, FormDataHelpers.dataEnteredInGroup(grp, ei.fieldData)))
      case _ => fgrpHtml
    }
  }

  private def htmlForGroup0(groupField: Group, fieldValue: FieldValue, index: Int, ei: ExtraInfo)(implicit hc: HeaderCarrier, request: Request[_], messages: Messages, retrievals: Retrievals) = {
    for {
      (lhtml, limitReached) <- getGroupForRendering(fieldValue, groupField, groupField.orientation, ei)
    } yield uk.gov.hmrc.gform.views.html.group(fieldValue, groupField, lhtml, groupField.orientation, limitReached, index)
  }

  private def getGroupForRendering(fieldValue: FieldValue, groupField: Group, orientation: Orientation, ei: ExtraInfo)(implicit hc: HeaderCarrier, request: Request[_], messsages: Messages, retrievals: Retrievals): Future[(List[Html], Boolean)] = {
    if (groupField.repeatsMax.isDefined) {
      repeatService.getRepeatingGroupsForRendering(fieldValue, groupField).flatMap {
        case (groupList, isLimit) =>
          Future.sequence((1 to groupList.size).map { count =>
            Future.sequence(groupList(count - 1).map(fv =>
              htmlFor(fv, count, ei))).map { lhtml =>
              uk.gov.hmrc.gform.views.html.group_element(fieldValue, groupField, lhtml, orientation, count, count == 1)
            }
          }.toList).map(a => (a, isLimit))
      }
    } else {
      Future.sequence(groupField.fields.map(fv => htmlFor(fv, 0, ei))).map(a => (a, true))
    }
  }

  private def validate(fieldValue: FieldValue, ei: ExtraInfo)(implicit hc: HeaderCarrier): Option[FormFieldValidationResult] = {
    val section = ei.dynamicSections(ei.sectionNumber.value)
    lazy val okF: FieldValue => Option[FormFieldValidationResult] =
      Fields.okValues(ei.fieldData, repeatService.atomicFields(section), ei.envelope)
    ei.f.getOrElse(okF)(fieldValue)
  }
}

