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
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.gform.models.form.FormId
import uk.gov.hmrc.gform.models.helpers.Fields._
import uk.gov.hmrc.gform.models.helpers.Javascript.fieldJavascript

case class SummaryForRender(snippets: List[Html], javascripts: String)

object SummaryForRender {
  def apply(data: Map[FieldId, Seq[String]], formId: FormId, formTemplate: FormTemplate): SummaryForRender = {

    val fields: List[FieldValue] = formTemplate.sections.flatMap(s => s.fields)

    val values: FieldValue => Option[FormFieldValidationResult] = okValues(data, fields)

    val snippets: List[Html] = {
      val allSections = formTemplate.sections
      val sectionsToRender = allSections.filter ( section => BooleanExpr.isTrue(section.includeIf.getOrElse(IncludeIf(IsTrue)).expr, data) )
      sectionsToRender.zipWithIndex.flatMap { case (section, index) =>
        uk.gov.hmrc.gform.views.html.snippets.summary.begin_section(formTemplate.formTypeId, formTemplate.version, formId, section.shortName.getOrElse(section.title), index) ::
          section.atomicFields.filter(_.submissible)
            .map { fieldValue =>
              fieldValue.`type` match {
                case Date(_, _, _) => uk.gov.hmrc.gform.views.html.snippets.summary.date(fieldValue, values(fieldValue))
                case Address(_) => uk.gov.hmrc.gform.views.html.snippets.summary.address(fieldValue, values(fieldValue))
                case t @ Text(_, _) => uk.gov.hmrc.gform.views.html.snippets.summary.text(fieldValue, t, values(fieldValue))
                case Choice(_, options, _, _, _) =>
                  val selections = options.toList.zipWithIndex.map { case (option, index) =>
                    values(fieldValue).flatMap(_.getOptionalCurrentValue(fieldValue.id.value + index.toString)).map(_ => option)
                  }.collect { case Some(selection) => selection }

                  uk.gov.hmrc.gform.views.html.snippets.summary.choice(fieldValue, selections)
                case FileUpload() => {
                  val fuFieldValue = FieldValue(
                    FieldId("regNum"),
                    Text(Constant(""), total = false),
                    label = "files uploaded go here...",
                    shortName = fieldValue.shortName,
                    helpText = None,
                    mandatory = true,
                    editable = true,
                    submissible = true)
                  uk.gov.hmrc.gform.views.html.snippets.summary.text(fuFieldValue, Text(Constant("file"), false), values(fuFieldValue))
                }
                case InformationMessage(_, _) | Group(_, _) => Html("")
              }
            } ++
            List(uk.gov.hmrc.gform.views.html.snippets.summary.end_section(formTemplate.formTypeId, formTemplate.version, formId, section.title, index))
      }
    }
    SummaryForRender(snippets, fieldJavascript(fields))
  }
}

case class Summary(formTemplate: FormTemplate) {
  def summaryForRender(formFields: Map[FieldId, Seq[String]], formId: FormId): SummaryForRender =
    SummaryForRender(formFields, formId, formTemplate)

  def renderSummary(formFields: Map[FieldId, Seq[String]], formId: FormId)(implicit request: Request[_], messages: Messages): Result = {
    Ok(uk.gov.hmrc.gform.views.html.summary(formTemplate, summaryForRender(formFields, formId), formId))
  }
}