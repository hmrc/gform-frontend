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
import uk.gov.hmrc.bforms.core.{Add, Expr, FormCtx}

case class PageForRender(curr: Int, hiddenFieldsSnippets: List[Html], snippets: List[Html], javascripts: String)

case class Page(prev: Int, curr: Int, next: Int, section: Section, formTemplate: FormTemplate) {
  def renderPage(formFields: Map[FieldId, Seq[String]], formId: Option[FormId], f: Option[FieldValue => Option[FormFieldValidationResult]])(implicit request: Request[_], messages: Messages): Result = {

    val getFormFieldValue: FieldId => FormField = fieldId => {
      val value = formFields.get(fieldId).toList.flatten.headOption.getOrElse("")
      FormField(fieldId, value)
    }

    def toFormField(fieldValue: List[FieldValue]) = {
      fieldValue.flatMap { fv =>
        fv.`type` match {
          case Some(Address) => Address.fields(fv.id).map(getFormFieldValue)
          case Some(Date) => Date.fields(fv.id).map(getFormFieldValue)
          case _ => List(getFormFieldValue(fv.id))
        }
      }
    }

    val hiddenSections = formTemplate.sections.filterNot(_ == section)

    val hiddenFormFields = toFormField(hiddenSections.flatMap(_.fields)).map(formField => uk.gov.hmrc.bforms.views.html.hidden_field(formField)).toList

    val pageFormFields = toFormField(section.fields).map(hf => hf.id -> hf).toMap

    val okValues: FieldValue => Option[FormFieldValidationResult] = fieldValue =>
      fieldValue.`type` match {
        case Some(Address) | Some(Date) =>
          val fieldOkData =
            pageFormFields.filter {
              case (fieldId, formField) => fieldId.value.startsWith(fieldValue.id.value) // Get just fieldIds related to fieldValue
            }.map {
              case (fieldId, formField) => fieldId.value.replace(fieldValue.id + ".", "") -> FieldOk(fieldValue, formField.value)
            }
          Some(ComponentField(fieldValue, fieldOkData))
        case _ => pageFormFields.get(fieldValue.id).map { formField =>
          FieldOk(fieldValue, formField.value)
        }
      }

    val snippets: List[Html] = {
      section.fields
        .map { fieldValue =>

          fieldValue.`type` match {
            case Some(Date) => uk.gov.hmrc.bforms.views.html.field_template_date(fieldValue, f.getOrElse(okValues)(fieldValue))
            case Some(Address) => uk.gov.hmrc.bforms.views.html.address(fieldValue, f.getOrElse(okValues)(fieldValue))
            case _ => uk.gov.hmrc.bforms.views.html.field_template_text(fieldValue, f.getOrElse(okValues)(fieldValue))
          }
        }
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
