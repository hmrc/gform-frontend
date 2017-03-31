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
import uk.gov.hmrc.bforms.models.helpers.Fields._
import uk.gov.hmrc.bforms.models.helpers.Javascript.fieldJavascript

case class SummaryForRender(snippets: List[Html], javascripts: String)

object SummaryForRender {
  def apply(formFields: Map[FieldId, Seq[String]], formId: FormId, formTemplate: FormTemplate): SummaryForRender = {

    val fields: List[FieldValue] = formTemplate.sections.flatMap(s => s.fields)

    val values: FieldValue => Option[FormFieldValidationResult] = okValues(formFields, fields)

    val snippets: List[Html] = {
      formTemplate.sections.zipWithIndex.flatMap { s =>
        uk.gov.hmrc.bforms.views.html.snippets.summary.begin_section(formTemplate.formTypeId, formTemplate.version, formId, s._1.title, s._2) ::
          s._1.fields
            .map { fieldValue =>
              fieldValue.`type` match {
                case Date => uk.gov.hmrc.bforms.views.html.snippets.summary.date(fieldValue, values(fieldValue))
                case Address => uk.gov.hmrc.bforms.views.html.snippets.summary.address(fieldValue, values(fieldValue))
                case _ => uk.gov.hmrc.bforms.views.html.snippets.summary.text(fieldValue, values(fieldValue))
              }
            } ++
            List(uk.gov.hmrc.bforms.views.html.snippets.summary.end_section(formTemplate.formTypeId, formTemplate.version, formId, s._1.title, s._2))
      }
    }
    SummaryForRender(snippets, fieldJavascript(fields))
  }
}

case class Summary(formTemplate: FormTemplate) {
  def summaryForRender(formFields: Map[FieldId, Seq[String]], formId: FormId) : SummaryForRender =
    SummaryForRender(formFields, formId, formTemplate)

  def renderSummary(formFields: Map[FieldId, Seq[String]], formId: FormId)(implicit request: Request[_], messages: Messages): Result = {
    Ok(uk.gov.hmrc.bforms.views.html.summary(formTemplate, summaryForRender(formFields, formId), formId))
  }
}
