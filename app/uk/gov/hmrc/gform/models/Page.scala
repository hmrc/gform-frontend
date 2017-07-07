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
import play.api.mvc.Results.Ok
import play.api.mvc.{ Request, Result }
import play.twirl.api.Html
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gformbackend.model.{ FormId, FormTemplate }
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class PageForRender(curr: Int, sectionTitle: String, hiddenFieldsSnippets: List[Html], snippets: List[Html], javascripts: String)

object PageForRender {
  def apply(
    curr: Int,
    fieldData: Map[FieldId, Seq[String]],
    formTemplate: FormTemplate,
    section: Section,
    f: Option[FieldValue => Option[FormFieldValidationResult]],
    repeatService: RepeatingComponentService,
    envelope: Envelope
  )(implicit authContext: AuthContext, hc: HeaderCarrier): Future[PageForRender] = new PageShader(curr, fieldData, formTemplate, section, f, repeatService, envelope).render()

}

case class Page(prev: Int, curr: Int, next: Int, section: Section, formTemplate: FormTemplate, repeatService: RepeatingComponentService, envelope: Envelope) {

  def pageForRender(fieldData: Map[FieldId, Seq[String]], f: Option[FieldValue => Option[FormFieldValidationResult]])(implicit authContext: AuthContext, hc: HeaderCarrier): Future[PageForRender] =
    PageForRender(curr, fieldData, formTemplate, section, f, repeatService, envelope)

  def renderPage(fieldData: Map[FieldId, Seq[String]], formId: Option[FormId], f: Option[FieldValue => Option[FormFieldValidationResult]])(implicit request: Request[_], messages: Messages, authContext: AuthContext, hc: HeaderCarrier): Future[Result] = {
    pageForRender(fieldData, f).map(page => Ok(uk.gov.hmrc.gform.views.html.form(formTemplate, page, formId)))
  }

}

object Page {
  def apply(currentPage: Int, formTemplate: FormTemplate, repeatService: RepeatingComponentService, envelope: Envelope): Page = {
    val lastPage = formTemplate.sections.size - 1

    val curr = currentPage match {
      case x if x <= 0 => 0
      case x if x >= lastPage => lastPage
      case _ => currentPage
    }

    val section = formTemplate.sections(curr)

    Page(Math.max(0, curr - 1), curr, Math.min(lastPage, curr + 1), section, formTemplate, repeatService, envelope)
  }
}
