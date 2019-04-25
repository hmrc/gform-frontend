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

import play.api.libs.json._
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.graph.Data
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.sharedmodel.LabelHelper

sealed trait FormComponentWithCtx {
  def id: FormComponentId = this match {
    case FormComponentWithGroup(fc, _) => fc.id
    case FormComponentSimple(fc)       => fc.id
  }
}

case class FormComponentWithGroup(fc: FormComponent, parent: FormComponent) extends FormComponentWithCtx
case class FormComponentSimple(fc: FormComponent) extends FormComponentWithCtx

case class ExpandedFormComponent(expandedFormComponents: List[FormComponent]) extends AnyVal {
  def allIds: List[FormComponentId] = {
    def recurse(formComponent: FormComponent): List[FormComponentId] =
      formComponent match {
        case IsMultiField(mf)      => mf.fields(formComponent.id).toList
        case IsRevealingChoice(rc) => rc.options.toList.flatMap(_.revealingFields.flatMap(recurse))
        case _                     => List(formComponent.id)
      }

    expandedFormComponents.flatMap(recurse)
  }
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

  private def addFieldIndex(field: FormComponent, index: Int) = {
    val fieldToUpdate = if (index == 0) field else field.copy(id = FormComponentId(index + "_" + field.id.value))
    val i = index + 1
    fieldToUpdate.copy(
      label = LabelHelper.buildRepeatingLabel(field, i),
      shortName = LabelHelper.buildRepeatingLabel(field.shortName, i))
  }

  import cats.implicits._
  private def expandByData(fc: FormComponent, data: Data): List[FormComponent] =
    expand(
      fc,
      (group, index) => {
        val ids: List[FormComponentId] = groupIndex(index + 1, group)
        val toExpand: Boolean = ids.forall(data.contains)
        if (index == 0 || toExpand) group.fields.map(addFieldIndex(_, index))
        else Nil
      },
      revealingChoice =>
        data
          .get(fc.id)
          .toSeq
          .flatten
          .headOption
          .filterNot(_.isEmpty)
          .map(_.toLong)
          .flatMap(i => revealingChoice.options.get(i).map(_.revealingFields))
          .getOrElse(Nil)
    )

  private def expandAll(fc: FormComponent): List[FormComponent] =
    expand(
      fc,
      (group, index) => group.fields.map(addFieldIndex(_, index)),
      revealingChoice => revealingChoice.options.toList.flatMap(_.revealingFields)
    )

  private def expand(
    fc: FormComponent,
    groupExpander: (Group, Int) => List[FormComponent],
    revealingChoiceExpander: RevealingChoice => List[FormComponent]): List[FormComponent] =
    fc.`type` match {
      case group: Group =>
        val expandedFields: List[FormComponent] =
          (0 until group.repeatsMax.getOrElse(1)).toList.flatMap(groupExpander(group, _))
        expandedFields.flatMap(expand(_, groupExpander, revealingChoiceExpander)) // for case when there is group inside group (Note: it does not work, we would need to handle prefix)
      case revealingChoice: RevealingChoice => fc :: revealingChoiceExpander(revealingChoice)
      case _                                => fc :: Nil
    }

  private def expandWithCtx(fc: FormComponent): List[FormComponentWithCtx] =
    fc.`type` match {
      case Group(fields, _, max, _, _, _) =>
        (0 until max.getOrElse(1)).toList.flatMap(index =>
          fields.map(field => FormComponentWithGroup(addFieldIndex(field, index), fc)))
      case _ => FormComponentSimple(fc) :: Nil
    }

  def expandFormComponent(data: Data): ExpandedFormComponent = ExpandedFormComponent(expandByData(this, data))

  val expandFormComponentFull: ExpandedFormComponent = ExpandedFormComponent(expandAll(this))

  val expandFormComponentFullWithCtx: List[FormComponentWithCtx] = expandWithCtx(this)

}

object FormComponent {
  implicit val format: OFormat[FormComponent] = Json.format[FormComponent]
}

object IsText {
  def unapply(fc: FormComponent): Option[Text] =
    fc.`type` match {
      case t @ Text(_, _, _, _) => Some(t)
      case _                    => None
    }
}

object IsCapitalised {
  def unapply(fc: FormComponent): Boolean =
    fc.`type` match {
      case t @ Text(_, _, _, IsUpperCase) => true
      case _                              => false
    }
}

object IsTextArea {
  def unapply(fc: FormComponent): Option[TextArea] =
    fc.`type` match {
      case t @ TextArea(_, _, _) => Some(t)
      case _                     => None
    }
}

object IsGroup {
  def unapply(fc: FormComponent): Option[Group] =
    fc.`type` match {
      case g @ Group(_, _, _, _, _, _) => Some(g)
      case _                           => None
    }
}

object IsMultiField {
  def unapply(fc: FormComponent): Option[MultiField] =
    fc.`type` match {
      case d: MultiField => Some(d)
      case _             => None
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

object IsRevealingChoice {
  def unapply(fc: FormComponent): Option[RevealingChoice] = fc.`type`.cast[RevealingChoice]
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

object IsHmrcTaxPeriod {
  def unapply(fc: FormComponent): Option[HmrcTaxPeriod] =
    fc.`type` match {
      case i @ HmrcTaxPeriod(_, _, _) => Some(i)
      case _                          => None
    }
}

object IsFileUpload {
  def unapply(fc: FormComponent): Boolean =
    fc.`type` match {
      case i @ FileUpload() => true
      case _                => false
    }
}
