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
import uk.gov.hmrc.bforms.controllers.FormFieldValidationResult
import uk.gov.hmrc.bforms.controllers.FieldOk
import play.twirl.api.Html
import uk.gov.hmrc.bforms.core.{Add, Expr, FormCtx}

case class PageForRender(curr: Int, hiddenFieldsSnippets: List[Html], snippets: List[Html], javascripts: String)

case class Page(prev: Int, curr: Int, next: Int, section: Section, formTemplate: FormTemplate) {
  def renderPage(formFields: Map[FieldId, Seq[String]], formId: Option[FormId])(implicit request: Request[_], messages: Messages): Result = {
    def toFormField(fieldValue: List[FieldValue]) = {
      fieldValue.map(fv => fv -> formFields.get(fv.id).toList.flatten)
        .map { case (fv, v) => FormField(fv.id, v.headOption.getOrElse("")) }
    }

    val hiddenSections = formTemplate.sections.filterNot(_ == section)

    val hiddenFormFields = toFormField(hiddenSections.flatMap(_.fields)).map(formField => uk.gov.hmrc.bforms.views.html.hidden_field(formField)).toList

    val pageFormFields = toFormField(section.fields).map(hf => hf.id -> hf).toMap

    def snippetsWithError(section: Section, f: FieldValue => Option[FormFieldValidationResult]): List[Html] = {
      section.fields
        .map { fieldValue =>

          fieldValue.`type` match {
            case Some(Date) => uk.gov.hmrc.bforms.views.html.field_template_date(fieldValue, f(fieldValue))
            case _ => uk.gov.hmrc.bforms.views.html.field_template_text(fieldValue, f(fieldValue))
          }
        }
    }

    val snippets: List[Html] = {
      snippetsWithError(section, fieldValue =>
        pageFormFields
          .get(fieldValue.id)
          .map(formField => FieldOk(fieldValue, formField.value))
      )
    }

    val fieldIdWithExpr: List[(FieldId, Expr)] = {
      val fieldNamesValues: List[(FieldId, Option[Expr])] = formTemplate.sections.flatMap(_.fields.map(s => (s.id, s.value)))

      fieldNamesValues.collect { case (f, Some(value)) => (f, value) }

    }

    def toJavascriptFn(fieldId: FieldId, expr: Expr): String = {

      expr match {
        case Add(FormCtx(amountA), FormCtx(amountB)) =>

          val functionName = "add" + fieldId.value;

          val eventListeners =
            for {
              elementId <- List(amountA, amountB)
              event <- List("change", "keyup")
            } yield
              s"""document.getElementById("$elementId").addEventListener("$event",$functionName);"""

          s"""|function $functionName() {
              |  var el1 = document.getElementById("$amountA").value;
              |  var el2 = document.getElementById("$amountB").value;
              |  var result = (parseInt(el1) || 0) + (parseInt(el2) || 0);
              |  return document.getElementById("${fieldId.value}").value = result;
              |};
              |${eventListeners.mkString("\n")}
              |""".stripMargin
        case otherwise => ""
      }
    }

    val rea = fieldIdWithExpr.map((toJavascriptFn _).tupled)
    val page = PageForRender(curr, hiddenFormFields, snippets, rea.mkString(";\n"))
    Ok(uk.gov.hmrc.bforms.views.html.form(formTemplate, page, formId))
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
