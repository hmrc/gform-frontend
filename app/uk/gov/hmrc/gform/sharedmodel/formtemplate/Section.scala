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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json._
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.collection.immutable.List

sealed trait BaseSection {
  def title: String
  def description: Option[String]
  def shortName: Option[String]
  def fields: List[FieldValue]
}

case class Section(
  title: String,
  description: Option[String],
  shortName: Option[String],
  includeIf: Option[IncludeIf],
  repeatsMax: Option[TextExpression],
  repeatsMin: Option[TextExpression],
  fields: List[FieldValue]
) extends BaseSection

object Section {
  implicit val format = Json.format[Section]
}

case class DeclarationSection(
  title: String,
  description: Option[String],
  shortName: Option[String],
  fields: List[FieldValue]
) extends BaseSection

object DeclarationSection {
  implicit val format = Json.format[DeclarationSection]
}

case class SectionFormField(
  title: String,
  fields: List[(List[FormField], FieldValue)]
)
