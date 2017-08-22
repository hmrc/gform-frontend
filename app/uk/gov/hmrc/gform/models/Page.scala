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
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.prepop.PrepopService
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class PageForRender(
  formId: FormId,
  sectionNumber: SectionNumber,
  sectionTitle: String,
  sectionDescription: Option[String],
  hiddenFieldsSnippets: List[Html],
  snippets: List[Html],
  javascripts: String,
  envelopeId: EnvelopeId,
  formMaxAttachmentSizeMB: Int
) //TODO maybe pass full section object into page for render to get access to all information

object PageForRender {
  def apply(
    formId: FormId,
    sectionNumber: SectionNumber,
    fieldData: Map[FieldId, Seq[String]],
    formTemplate: FormTemplate,
    f: Option[FieldValue => Option[FormFieldValidationResult]],
    repeatService: RepeatingComponentService,
    envelope: Envelope,
    envelopeId: EnvelopeId,
    prepopService: PrepopService,
    dynamicSections: List[Section],
    formMaxAttachmentSizeMB: Int
  )(implicit retrievals: Retrievals, hc: HeaderCarrier): Future[PageForRender] = new PageShader(formId, sectionNumber, fieldData, formTemplate, f, repeatService, envelope, envelopeId, prepopService, dynamicSections, formMaxAttachmentSizeMB).render()

}

case class Page(formId: FormId, sectionNumber: SectionNumber, formTemplate: FormTemplate, repeatService: RepeatingComponentService, envelope: Envelope, envelopeId: EnvelopeId, prepopService: PrepopService) {
  def pageForRender(fieldData: Map[FieldId, Seq[String]], f: Option[FieldValue => Option[FormFieldValidationResult]], dynamicSections: List[Section], formMaxAttachmentSizeMB: Int)(implicit retrievals: Retrievals, hc: HeaderCarrier): Future[PageForRender] =
    PageForRender(formId, sectionNumber, fieldData, formTemplate, f, repeatService, envelope, envelopeId, prepopService, dynamicSections, formMaxAttachmentSizeMB)

  def renderPage(fieldData: Map[FieldId, Seq[String]], formId: FormId, f: Option[FieldValue => Option[FormFieldValidationResult]], dynamicSections: List[Section], formMaxAttachmentSizeMB: Int)(implicit request: Request[_], messages: Messages, retrievals: Retrievals, hc: HeaderCarrier): Future[Result] = {
    pageForRender(fieldData, f, dynamicSections, formMaxAttachmentSizeMB).map(page => Ok(uk.gov.hmrc.gform.views.html.form(formTemplate, page, formId)))
  }
}
