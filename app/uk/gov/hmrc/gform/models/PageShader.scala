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
import uk.gov.hmrc.gform.gformbackend.model.FormTemplate
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions.withOffset
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.models.helpers.Javascript.fieldJavascript
import uk.gov.hmrc.gform.service.PrepopService
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PageShader(
    curr: Int,
    fieldData: Map[FieldId, Seq[String]],
    formTemplate: FormTemplate,
    section: Section,
    f: Option[FieldValue => Option[FormFieldValidationResult]]
)(implicit authContext: AuthContext, hc: HeaderCarrier) {

  def render(): Future[PageForRender] = {
    val snippetsSeq = section.fields.map(htmlFor)
    val snippets = Future.sequence(snippetsSeq)
    val javasctipt = fieldJavascript(formTemplate.sections.flatMap(_.atomicFields))
    snippets.map(snippets => PageForRender(curr, section.title, hiddenSnippets, snippets, javasctipt))
  }

  private def htmlFor(fieldValue: FieldValue): Future[Html] = fieldValue.`type` match {
    case Group(fvs, orientation) => htmlForGroup(fieldValue, fvs, orientation)
    case Date(_, offset, dateValue) => htmlForDate(fieldValue, offset, dateValue)
    case Address(international) => htmlForAddress(fieldValue, international)
    case t @ Text(expr, _) => htmlForText(fieldValue, t, expr)
    case Choice(choice, options, orientation, selections, optionalHelpText) => htmlForChoice(fieldValue, choice, options, orientation, selections, optionalHelpText)
    case FileUpload() => htmlForFileUpload(fieldValue)
    case InformationMessage(infoType, infoText) => htmlForInformationMessage(fieldValue, infoType, infoText)
  }

  private def htmlForInformationMessage(fieldValue: FieldValue, infoType: InfoType, infoText: String) = {
    val flavour = new GFMFlavourDescriptor
    val parsedTree = new MarkdownParser(flavour).buildMarkdownTreeFromString(infoText)
    val parsedMarkdownText = new HtmlGenerator(infoText, parsedTree, flavour, false).generateHtml
    Future.successful(uk.gov.hmrc.gform.views.html.field_template_info(fieldValue, infoType, Html(parsedMarkdownText)))
  }

  private def htmlForFileUpload(fieldValue: FieldValue) = {
    Future.successful(uk.gov.hmrc.gform.views.html.file_upload(fieldValue))
  }

  private def htmlForChoice(fieldValue: FieldValue, choice: ChoiceType, options: NonEmptyList[String], orientation: Orientation, selections: List[Int], optionalHelpText: Option[List[String]]) = {
    val prepopValues = fieldData.get(fieldValue.id) match {
      case None => selections.map(_.toString).toSet
      case Some(_) => Set.empty[String] // Don't prepop something we already submitted
    }

    val snippet =
      choice match {
        case Radio | YesNo => uk.gov.hmrc.gform.views.html.choice("radio", fieldValue, options, orientation, prepopValues, f.getOrElse(okF)(fieldValue), optionalHelpText)
        case Checkbox => uk.gov.hmrc.gform.views.html.choice("checkbox", fieldValue, options, orientation, prepopValues, f.getOrElse(okF)(fieldValue), optionalHelpText)
        case Inline => uk.gov.hmrc.gform.views.html.choiceInline(fieldValue, options, prepopValues, f.getOrElse(okF)(fieldValue), optionalHelpText)
      }

    Future.successful(snippet)
  }

  private def htmlForText(fieldValue: FieldValue, t: Text, expr: Expr) = {
    val prepopValueF = fieldData.get(fieldValue.id) match {
      case None => PrepopService.prepopData(expr, formTemplate.formTypeId)
      case _ => Future.successful("") // Don't prepop something we already submitted
    }
    prepopValueF.map(prepopValue => uk.gov.hmrc.gform.views.html.field_template_text(fieldValue, t, prepopValue, f.getOrElse(okF)(fieldValue)))
  }

  private def htmlForAddress(fieldValue: FieldValue, international: Boolean) = {
    Future.successful(uk.gov.hmrc.gform.views.html.field_template_address(international, fieldValue, f.getOrElse(okF)(fieldValue)))
  }

  private def htmlForDate(fieldValue: FieldValue, offset: Offset, dateValue: Option[DateValue]) = {
    val prepopValues = dateValue.map(DateExpr.fromDateValue).map(withOffset(offset, _))
    Future.successful(uk.gov.hmrc.gform.views.html.field_template_date(fieldValue, f.getOrElse(okF)(fieldValue), prepopValues))
  }

  private def htmlForGroup(fieldValue: FieldValue, fvs: List[FieldValue], orientation: Orientation) = {

    val listofeventualhtmls: List[Future[Html]] = fvs.map {
      case (fv: FieldValue) => htmlFor(fv)
    }
    Future.sequence(listofeventualhtmls).flatMap {
      case (lhtml) => Future.successful(uk.gov.hmrc.gform.views.html.group(fieldValue, lhtml, orientation))
    }

  }

  private lazy val hiddenTemplateFields = formTemplate.sections.filterNot(_ == section).flatMap(_.fields)
  private lazy val hiddenSnippets = Fields.toFormField(fieldData, hiddenTemplateFields).map(formField => uk.gov.hmrc.gform.views.html.hidden_field(formField))
  private lazy val okF: FieldValue => Option[FormFieldValidationResult] = Fields.okValues(fieldData, section.atomicFields)
}
