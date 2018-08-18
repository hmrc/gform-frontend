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
import uk.gov.hmrc.gform.sharedmodel.LabelHelper

case class ExpandedFormComponent(expandedFC: List[FormComponent]) extends AnyVal {
  def allIds: List[FormComponentId] =
    expandedFC.map {
      case fc @ IsDate(_)       => Date.fields(fc.id)
      case fc @ IsAddress(_)    => Address.fields(fc.id)
      case fc @ IsUkSortCode(_) => UkSortCode.fields(fc.id)
      case fc                   => List(fc.id)
    }.flatten
}

case class FormComponent(
  id: FormComponentId,
  `type`: ComponentType,
  label: String,
  helpText: Option[String],
  shortName: Option[String],
  validIf: Option[ValidIf],
  mandatory: Boolean,
  editable: Boolean,
  submissible: Boolean,
  derived: Boolean,
  onlyShowOnSummary: Boolean = false,
  errorMessage: Option[String],
  presentationHint: Option[List[PresentationHint]] = None
) {

  private def updateField(i: Int, fc: FormComponent): FormComponent =
    fc.copy(
      label = LabelHelper.buildRepeatingLabel(fc, i),
      shortName = LabelHelper.buildRepeatingLabel(fc.shortName, i))

  private def loop(fc: FormComponent): List[FormComponent] =
    fc.`type` match {
      case Group(fields, _, max, _, _, _) =>
        val expandedFields: List[FormComponent] =
          (0 until max.getOrElse(1)).toList.flatMap { i =>
            fields.map { field =>
              val fieldToUpdate = if (i == 0) field else field.copy(id = FormComponentId(i + "_" + field.id.value))
              updateField(i + 1, fieldToUpdate)
            }
          }

        expandedFields.flatMap(loop) // for case when there is group inside group (Note: it does not work, we would need to handle prefix)

      case _ => fc :: Nil
    }

  val expandFormComponent: ExpandedFormComponent = ExpandedFormComponent(loop(this))

}

object FormComponent {
  implicit val format = Json.format[FormComponent]
}

object IsGroup {
  def unapply(fc: FormComponent): Option[Group] =
    fc.`type` match {
      case g @ Group(_, _, _, _, _, _) => Some(g)
      case _                           => None
    }
}

object IsDate {
  def unapply(fc: FormComponent): Option[Date] =
    fc.`type` match {
      case d @ Date(_, _, _) => Some(d)
      case _                 => None
    }
}

object IsChoice {
  def unapply(fc: FormComponent): Option[Choice] =
    fc.`type` match {
      case c @ Choice(_, _, _, _, _) => Some(c)
      case _                         => None
    }
}

object IsAddress {
  def unapply(fc: FormComponent): Option[Address] =
    fc.`type` match {
      case a @ Address(_) => Some(a)
      case _              => None
    }
}

object IsUkSortCode {
  def unapply(fc: FormComponent): Option[UkSortCode] =
    fc.`type` match {
      case u @ UkSortCode(_) => Some(u)
      case _                 => None
    }
}

object IsInformationMessage {
  def unapply(fc: FormComponent): Option[InformationMessage] =
    fc.`type` match {
      case i @ InformationMessage(_, _) => Some(i)
      case _                            => None
    }
}

object IsFileUpload {
  def unapply(fc: FormComponent): Boolean =
    fc.`type` match {
      case i @ FileUpload() => true
      case _                => false
    }
}
