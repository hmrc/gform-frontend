/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json._
import uk.gov.hmrc.gform.graph.Data
import uk.gov.hmrc.gform.sharedmodel.formtemplate

case class ExpandedFormTemplate(expandedSection: List[ExpandedSection]) {
  val allFCs: List[FormComponent] = expandedSection.flatMap(_.expandedFCs.flatMap(_.expandedFC))
  val allFcIds: List[FormComponentId] = expandedSection.flatMap(_.expandedFCs.flatMap(_.allIds))
  def fcsLookup(data: Data): Map[FormComponentId, FormComponent] =
    allFCs.flatMap(fc => fc.expandFormComponent(data).allIds.map(_ -> fc)).toMap
  def fcsLookupFull: Map[FormComponentId, FormComponent] =
    allFCs.flatMap(fc => fc.expandFormComponentFull.allIds.map(_ -> fc)).toMap
  val allIncludeIfs: List[(List[ExpandedFormComponent], IncludeIf, Int)] = expandedSection.zipWithIndex.collect {
    case (ExpandedSection(expandedFCs, Some(includeIf)), index) => (expandedFCs, includeIf, index)
  }
}

case class FormTemplate(
  _id: FormTemplateId,
  formName: String,
  description: String,
  developmentPhase: Option[DevelopmentPhase] = Some(ResearchBanner),
  formCategory: Option[FormCategory],
  draftRetrievalMethod: Option[DraftRetrievalMethod] = Some(OnePerUser),
  submissionReference: Option[TextExpression],
  dmsSubmission: DmsSubmission,
  authConfig: formtemplate.AuthConfig,
  emailTemplateId: String,
  submitSuccessUrl: String,
  submitErrorUrl: String,
  sections: List[Section],
  acknowledgementSection: AcknowledgementSection,
  declarationSection: DeclarationSection,
  GFC579Ready: Option[String] = Some("false")
) {
  def expandFormTemplate(data: Data): ExpandedFormTemplate = ExpandedFormTemplate(sections.map(_.expandSection(data)))
  val expandFormTemplateFull: ExpandedFormTemplate = ExpandedFormTemplate(sections.map(_.expandSectionFull))
}

object FormTemplate {

  implicit val format: OFormat[FormTemplate] = Json.format[FormTemplate]
}
