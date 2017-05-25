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

import play.api.i18n.Messages
import play.api.mvc.{Request, Result}
import play.api.mvc.Results.Ok
import play.twirl.api.Html

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.gform.service.PrepopService
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.gform.models.form.FormId
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.models.helpers.Javascript.fieldJavascript
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions._


case class PageForRender(curr: Int, sectionTitle: String, hiddenFieldsSnippets: List[Html], snippets: List[Html], javascripts: String)

object PageForRender {
  def apply(
    curr: Int,
    fieldData: Map[FieldId, Seq[String]],
    formTemplate: FormTemplate,
    section: Section,
    f: Option[FieldValue => Option[FormFieldValidationResult]])(implicit authContext: AuthContext, hc: HeaderCarrier): Future[PageForRender] = {

    val hiddenTemplateFields = formTemplate.sections.filterNot(_ == section).flatMap(_.fields)

    val hiddenSnippets = Fields.toFormField(fieldData, hiddenTemplateFields).map(formField => uk.gov.hmrc.gform.views.html.hidden_field(formField))

    val okF: FieldValue => Option[FormFieldValidationResult] = Fields.okValues(fieldData, section.atomicFields)

    def htmlFor(fieldValue: FieldValue): Future[Html] =
          fieldValue.`type` match {
            case Group(fvs) => {
              val listofeventualhtmls: List[Future[Html]] = fvs.map {
                case (fv: FieldValue) => htmlFor(fv)
              }
              Future.sequence(listofeventualhtmls).flatMap {
                case (lhtml) => Future.successful(uk.gov.hmrc.gform.views.html.group(fieldValue, lhtml))
              }
            }
            case Date(_, offset, dateValue) =>
              val prepopValues = dateValue.map(DateExpr.fromDateValue).map(withOffset(offset, _))
              Future.successful(uk.gov.hmrc.gform.views.html.field_template_date(fieldValue, f.getOrElse(okF)(fieldValue), prepopValues))

            case Address =>
              Future.successful(uk.gov.hmrc.gform.views.html.address(fieldValue, f.getOrElse(okF)(fieldValue)))

            case t @ Text(expr, _) =>
              val prepopValueF = fieldData.get(fieldValue.id) match {
                case None  => PrepopService.prepopData(expr, formTemplate.formTypeId)
                case _ => Future.successful("") // Don't prepop something we already submitted
              }
              prepopValueF.map(prepopValue => uk.gov.hmrc.gform.views.html.field_template_text(fieldValue, t, prepopValue, f.getOrElse(okF)(fieldValue)))

            case Choice(choice, options, orientation, selections, optionalHelpText) =>
              val prepopValues = fieldData.get(fieldValue.id) match {
                case None => selections.map(_.toString).toSet
                case Some(_) => Set.empty[String] // Don't prepop something we already submitted
              }

              val snippet =
                choice match {
                  case Radio | YesNo => uk.gov.hmrc.gform.views.html.choice("radio", fieldValue, options, orientation, prepopValues, f.getOrElse(okF)(fieldValue), optionalHelpText)
                  case Checkbox => uk.gov.hmrc.gform.views.html.choice("checkbox", fieldValue, options, orientation, prepopValues, f.getOrElse(okF)(fieldValue), optionalHelpText)
                }
              Future.successful(snippet)
          }

    val snippetsF: List[Future[Html]] = {
      val sectionFields: List[FieldValue] = section.fields
      sectionFields.map {
        case (fv: FieldValue) => htmlFor(fv)
      }
    }
    Future.sequence(snippetsF).map(snippets => PageForRender(curr, section.title, hiddenSnippets, snippets, fieldJavascript(formTemplate.sections.flatMap(_.atomicFields))))
  }
}

case class Page(prev: Int, curr: Int, next: Int, section: Section, formTemplate: FormTemplate) {

  def pageForRender(fieldData: Map[FieldId, Seq[String]], f: Option[FieldValue => Option[FormFieldValidationResult]])(implicit authContext: AuthContext, hc: HeaderCarrier) : Future[PageForRender] =
    PageForRender(curr, fieldData, formTemplate, section, f)

  def renderPage(fieldData: Map[FieldId, Seq[String]], formId: Option[FormId], f: Option[FieldValue => Option[FormFieldValidationResult]])
                (implicit request: Request[_], messages: Messages, authContext: AuthContext, hc: HeaderCarrier): Future[Result] = {
    pageForRender(fieldData, f).map(page => Ok(uk.gov.hmrc.gform.views.html.form(formTemplate, page, formId)))
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
