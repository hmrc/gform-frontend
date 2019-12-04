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
import uk.gov.hmrc.gform.models.javascript.JsFormComponentModel
import uk.gov.hmrc.gform.sharedmodel.{ LocalisedString, SmartString, VariadicFormData }

import scala.collection.immutable.List

sealed trait BaseSection {
  def title: SmartString
  def shortName: Option[SmartString]
  def fields: List[FormComponent]
}

case class ExpandedSection(expandedFormComponents: List[ExpandedFormComponent], includeIf: Option[IncludeIf]) {
  def toExpandedFormTemplate: ExpandedFormTemplate = ExpandedFormTemplate(this :: Nil)
  def allFCs: List[FormComponent] = toExpandedFormTemplate.allFormComponents
}

case class Section(
  title: SmartString,
  description: Option[SmartString],
  shortName: Option[SmartString],
  progressIndicator: Option[SmartString] = None,
  includeIf: Option[IncludeIf],
  repeatsMax: Option[TextExpression],
  repeatsMin: Option[TextExpression],
  validators: Option[Validator], //TODO List instead of Option
  fields: List[FormComponent],
  continueLabel: Option[SmartString],
  continueIf: Option[ContinueIf]
) extends BaseSection {
  def expandSection(data: VariadicFormData): ExpandedSection =
    ExpandedSection(fields.map(_.expandFormComponent(data)), includeIf) // TODO expand sections

  def expandSectionRc(data: VariadicFormData): ExpandedSection =
    ExpandedSection(fields.map(_.expandFormComponentRc(data)), includeIf) // TODO expand sections

  val expandSectionFull: ExpandedSection =
    ExpandedSection(fields.map(_.expandFormComponentFull), includeIf) // TODO expand sections

  val jsFormComponentModels: List[JsFormComponentModel] = fields.flatMap(_.jsFormComponentModels)

  def isRepeating: Boolean = repeatsMax.isDefined && repeatsMin.isDefined
}

object Section {
  implicit val format: OFormat[Section] = Json.format[Section]
}

case class DeclarationSection(
  title: SmartString,
  description: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent]
) extends BaseSection

object DeclarationSection {
  implicit val format: OFormat[DeclarationSection] = Json.format[DeclarationSection]
}

case class AcknowledgementSection(
  title: SmartString,
  description: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent]
) extends BaseSection

object AcknowledgementSection {
  implicit val format: OFormat[AcknowledgementSection] = Json.format[AcknowledgementSection]
}

case class EnrolmentSection(
  title: SmartString,
  shortName: Option[SmartString],
  fields: List[FormComponent],
  identifiers: NonEmptyList[IdentifierRecipe],
  verifiers: List[VerifierRecipe]
) extends BaseSection

object EnrolmentSection {
  import JsonUtils._
  implicit val format: OFormat[EnrolmentSection] = Json.format[EnrolmentSection]
}

case class IdentifierRecipe(key: String, value: FormCtx)
object IdentifierRecipe {
  implicit val format: OFormat[IdentifierRecipe] = Json.format[IdentifierRecipe]
}

case class VerifierRecipe(key: String, value: FormCtx)
object VerifierRecipe {
  implicit val format: OFormat[VerifierRecipe] = Json.format[VerifierRecipe]
}
