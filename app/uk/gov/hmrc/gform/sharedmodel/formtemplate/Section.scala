/*
 * Copyright 2019 HM Revenue & Customs
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

import cats.data.NonEmptyList
import play.api.libs.json._
import uk.gov.hmrc.gform.graph.Data
import uk.gov.hmrc.gform.sharedmodel.LocalisedString
import uk.gov.hmrc.gform.sharedmodel.form.FormField

import scala.collection.immutable.List

sealed trait BaseSection {
  def title: LocalisedString
  def shortName: Option[LocalisedString]
  def fields: List[FormComponent]
}

case class ExpandedSection(expandedFormComponents: List[ExpandedFormComponent], includeIf: Option[IncludeIf]) {
  def toExpandedFormTemplate: ExpandedFormTemplate = ExpandedFormTemplate(this :: Nil)
  def allFCs: List[FormComponent] = toExpandedFormTemplate.allFormComponents
}

case class Section(
  title: LocalisedString,
  description: Option[LocalisedString],
  shortName: Option[LocalisedString],
  progressIndicator: Option[LocalisedString] = None,
  includeIf: Option[IncludeIf],
  repeatsMax: Option[TextExpression],
  repeatsMin: Option[TextExpression],
  validators: Option[Validator], //TODO List instead of Option
  fields: List[FormComponent],
  continueLabel: Option[LocalisedString],
  continueIf: Option[ContinueIf]
) extends BaseSection {
  def expandSection(data: Data): ExpandedSection =
    ExpandedSection(fields.map(_.expandFormComponent(data)), includeIf) // TODO expand sections

  def expandSectionRc(data: Data): ExpandedSection =
    ExpandedSection(fields.map(_.expandFormComponentRc(data)), includeIf) // TODO expand sections

  val expandSectionFull: ExpandedSection =
    ExpandedSection(fields.map(_.expandFormComponentFull), includeIf) // TODO expand sections

  val expandSectionFullWithCtx: List[FormComponentWithCtx] = fields.flatMap(_.expandFormComponentFullWithCtx)

  def isRepeating: Boolean = repeatsMax.isDefined && repeatsMin.isDefined
}

object Section {
  implicit val format: OFormat[Section] = Json.format[Section]
}

case class DeclarationSection(
  title: LocalisedString,
  description: Option[LocalisedString],
  shortName: Option[LocalisedString],
  fields: List[FormComponent]
) extends BaseSection

object DeclarationSection {
  implicit val format: OFormat[DeclarationSection] = Json.format[DeclarationSection]
}

case class AcknowledgementSection(
  title: LocalisedString,
  description: Option[LocalisedString],
  shortName: Option[LocalisedString],
  fields: List[FormComponent]
) extends BaseSection

object AcknowledgementSection {
  implicit val format: OFormat[AcknowledgementSection] = Json.format[AcknowledgementSection]
}

case class EnrolmentSection(
  title: LocalisedString,
  shortName: Option[LocalisedString],
  fields: List[FormComponent],
  identifiers: NonEmptyList[IdentifierRecipe],
  verifiers: List[VerifierRecipe]
) extends BaseSection

object EnrolmentSection {
  import JsonUtils._
  implicit val format: OFormat[EnrolmentSection] = Json.format[EnrolmentSection]
}

case class SectionFormField(
  title: String,
  fields: List[(List[FormField], FormComponent)]
)

case class IdentifierRecipe(key: String, value: FormCtx)
object IdentifierRecipe {
  implicit val format: OFormat[IdentifierRecipe] = Json.format[IdentifierRecipe]
}

case class VerifierRecipe(key: String, value: FormCtx)
object VerifierRecipe {
  implicit val format: OFormat[VerifierRecipe] = Json.format[VerifierRecipe]
}
