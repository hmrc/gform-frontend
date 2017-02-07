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

import uk.gov.hmrc.bforms.controllers.FormFieldValidationResult
import uk.gov.hmrc.bforms.controllers.FieldOk
import play.twirl.api.Html

case class Page(prev: Int, curr: Int, next: Int, section: Section, hiddenFieldsSnippets: List[Html], snippets: List[Html])

object Page {

  def apply(currentPage: Int, ff: FormTemplate, formFields: Map[String, Seq[String]]): Page = {

    def toFormField(fieldValue: List[FieldValue]) = {
      fieldValue.map(fv => fv -> formFields.get(fv.id).toList.flatten)
        .map{ case (fv, v) => FormField(fv.id, v.headOption.getOrElse(""))}
    }

    val lastPage = ff.sections.size - 1

    val curr = currentPage match {
      case x if x <= 0        => 0
      case x if x >= lastPage => lastPage
      case _                  => currentPage
    }

    val section = ff.sections(curr)

    val hiddenSections = ff.sections.filterNot(_ == section)

    val hiddenFormFields = toFormField(hiddenSections.flatMap(_.fields)).map(formField => uk.gov.hmrc.bforms.views.html.hidden_field(formField)).toList

    val pageFormFields = toFormField(section.fields).map(hf => hf.id -> hf).toMap

    val snippets: List[Html] = {
      snippetsWithError(section, fieldValue =>
        pageFormFields
          .get(fieldValue.id)
          .map(formField => FieldOk(fieldValue, formField.value))
      )
    }

    Page(Math.max(0, curr - 1), curr, Math.min(lastPage, curr + 1), section, hiddenFormFields, snippets)
  }

  def snippetsWithError(section: Section, f: FieldValue => Option[FormFieldValidationResult]): List[Html] = {
    section.fields
      .map { fieldValue =>
      uk.gov.hmrc.bforms.views.html.field_template_text(fieldValue, f(fieldValue))
    }
  }
}
