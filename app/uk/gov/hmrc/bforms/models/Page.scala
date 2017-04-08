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

package uk.gov.hmrc.bforms.models

import play.api.i18n.Messages
import play.api.mvc.{Request, Result}
import play.api.mvc.Results.Ok
import play.twirl.api.Html
import uk.gov.hmrc.bforms.models.helpers.Fields
import uk.gov.hmrc.bforms.models.helpers.Javascript.fieldJavascript
import uk.gov.hmrc.bforms.core._
import uk.gov.hmrc.bforms.core.utils.DateHelperFunctions


case class PageForRender(curr: Int, hiddenFieldsSnippets: List[Html], snippets: List[Html], javascripts: String)

object PageForRender {
  def apply(curr: Int, formFields: Map[FieldId, Seq[String]], formTemplate: FormTemplate, section: Section, f: Option[FieldValue => Option[FormFieldValidationResult]]): PageForRender = {

    val hiddenFields = formTemplate.sections.filterNot(_ == section).flatMap(_.fields)

    val hiddenSnippets = Fields.toFormField(formFields, hiddenFields).map(formField => uk.gov.hmrc.bforms.views.html.hidden_field(formField))

    val okF: FieldValue => Option[FormFieldValidationResult] = Fields.okValues(formFields, section.fields)

    val snippets: List[Html] = {
      section.fields
        .map { fieldValue =>
          fieldValue.`type` match {
            case Date(_, offset, dateValue) =>
              val prepopValues = dateValue.map(DateExpr.fromDateValue) // TODO add Offset to the calculation
              uk.gov.hmrc.bforms.views.html.field_template_date(fieldValue, f.getOrElse(okF)(fieldValue), prepopValues)
            case Address => uk.gov.hmrc.bforms.views.html.address(fieldValue, f.getOrElse(okF)(fieldValue))
            case t @ Text(expr) =>
              val prepopValue = (formFields.get(fieldValue.id), expr) match {
                case (None, Constant(constant)) => constant
                case _ => "" // Don't prepop something we already submitted
              }
              uk.gov.hmrc.bforms.views.html.field_template_text(fieldValue, t, prepopValue, f.getOrElse(okF)(fieldValue))
            case Choice(choice, options, orientation, selections) =>
              val prepopValues = formFields.get(fieldValue.id) match {
                case None => selections.map(_.toString).toSet
                case Some(_) => Set.empty[String] // Don't prepop something we already submitted
              }

              choice match {
                case Radio | YesNo => uk.gov.hmrc.bforms.views.html.choice("radio", fieldValue, options, orientation, prepopValues, f.getOrElse(okF)(fieldValue))
                case Checkbox => uk.gov.hmrc.bforms.views.html.choice("checkbox", fieldValue, options, orientation, prepopValues, f.getOrElse(okF)(fieldValue))
              }
          }
        }
    }
    PageForRender(curr, hiddenSnippets, snippets, fieldJavascript(formTemplate.sections.flatMap(_.fields)))
  }
}

case class Page(prev: Int, curr: Int, next: Int, section: Section, formTemplate: FormTemplate) {

  def pageForRender(formFields: Map[FieldId, Seq[String]], f: Option[FieldValue => Option[FormFieldValidationResult]]): PageForRender =
    PageForRender(curr, formFields, formTemplate, section, f)

  def renderPage(formFields: Map[FieldId, Seq[String]], formId: Option[FormId], f: Option[FieldValue => Option[FormFieldValidationResult]])
                (implicit request: Request[_], messages: Messages): Result = {
    Ok(uk.gov.hmrc.bforms.views.html.form(formTemplate, pageForRender(formFields, f), formId))
  }

}

object Page {
  def apply(currentPage: Int, formTemplate: FormTemplate): Page = {
    val lastPage = formTemplate.sections.size - 1

    val curr = currentPage match {
      case x if x <= 0 => 0
      case x if x >= lastPage => lastPage
      case _ => currentPage
    }

    val section = formTemplate.sections(curr)

    Page(Math.max(0, curr - 1), curr, Math.min(lastPage, curr + 1), section, formTemplate)
  }
}
