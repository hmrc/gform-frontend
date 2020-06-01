/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.instances.list._
import cats.instances.int._
import cats.syntax.eq._
import cats.syntax.foldable._
import play.api.libs.json._
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.gform.FormComponentUpdater
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.models.email.{ EmailFieldId, VerificationCodeFieldId, emailFieldId, verificationCodeFieldId }
import uk.gov.hmrc.gform.models.javascript.{ FormComponentSimple, FormComponentWithGroup, JsFormComponentModel, JsFormComponentWithCtx, JsRevealingChoiceModel }
import uk.gov.hmrc.gform.sharedmodel.{ LabelHelper, SmartString, VariadicFormData }

case class ExpandedFormComponent(formComponents: List[FormComponent]) extends AnyVal {
  // ToDo Lance - Shouldn't we be recursing into Group and RevealingChoice
  def allIds: List[FormComponentId] = {
    def recurse(formComponent: FormComponent): List[FormComponentId] =
      formComponent match {
        case IsMultiField(mf) => mf.fields(formComponent.id).toList
        case _                => List(formComponent.id)
      }

    formComponents.flatMap(recurse)
  }
}

case class FormComponent(
  id: FormComponentId,
  `type`: ComponentType,
  label: SmartString,
  helpText: Option[SmartString],
  shortName: Option[SmartString],
  validIf: Option[ValidIf],
  mandatory: Boolean,
  editable: Boolean,
  submissible: Boolean,
  derived: Boolean,
  onlyShowOnSummary: Boolean = false,
  errorMessage: Option[SmartString],
  presentationHint: Option[List[PresentationHint]] = None,
  validators: List[FormComponentValidator] = Nil
) {

  def hideOnSummary: Boolean =
    presentationHint.fold(false)(x => x.contains(InvisibleInSummary)) || IsInformationMessage.unapply(this).isDefined

  private def updateField(i: Int, fc: FormComponent): FormComponent =
    fc.copy(
      label = LabelHelper.buildRepeatingLabel(fc.label, i),
      shortName = LabelHelper.buildRepeatingLabel(fc.shortName, i))

  private def loop(fc: FormComponent): List[FormComponent] =
    fc.`type` match {
      case Group(fields, max, _, _, _) =>
        val expandedFields =
          for {
            field <- fields
            res <- updateField(1, field) :: (1 until max.getOrElse(1))
                    .map(i => updateField(i + 1, field.copy(id = FormComponentId(i + "_" + field.id.value))))
                    .toList
          } yield res
        expandedFields.flatMap(loop) // for case when there is group inside group (Note: it does not work, we would need to handle prefix)
      case RevealingChoice(options, _) => fc :: options.toList.foldMap(_.revealingFields.map(loop)).flatten
      case _                           => fc :: Nil
    }

  lazy val expandedFormComponents: List[FormComponent] = loop(this)

  private def addFieldIndex(field: FormComponent, index: Int, group: Group) = {
    val fieldToUpdate = if (index === 0) field else field.copy(id = FormComponentId(index + "_" + field.id.value))
    val i = index + 1
    FormComponentUpdater(
      fieldToUpdate.copy(
        label = LabelHelper.buildRepeatingLabel(field.label, i),
        shortName = LabelHelper.buildRepeatingLabel(field.shortName, i)
      ),
      index,
      group
    ).updated
  }

  private val expandGroup: VariadicFormData => Group => Int => List[FormComponent] = data =>
    group =>
      index => {
        val ids: List[FormComponentId] = groupIndex(index + 1, group)
        val toExpand: Boolean = ids.forall(data.contains)
        if (index === 0 || toExpand) {
          group.fields.map(addFieldIndex(_, index, group))
        } else Nil
  }

  private val expandRevealingChoice: RevealingChoice => List[FormComponent] =
    _.options.toList.flatMap(_.revealingFields)

  private def expandByDataRc(fc: FormComponent, data: VariadicFormData): List[FormComponent] =
    expand(fc, expandGroup(data), RevealingChoice.slice(fc.id)(data))

  private def expandByData(fc: FormComponent, data: VariadicFormData): List[FormComponent] =
    expand(fc, expandGroup(data), expandRevealingChoice)

  private def expandAll(fc: FormComponent): List[FormComponent] =
    expand(fc, group => index => group.fields.map(addFieldIndex(_, index, group)), expandRevealingChoice)

  private def expand(
    fc: FormComponent,
    expandGroup: Group => Int => List[FormComponent],
    expandRc: RevealingChoice => List[FormComponent]
  ): List[FormComponent] =
    fc.`type` match {
      case g @ Group(fields, max, _, _, _) => (0 until max.getOrElse(1)).toList.flatMap(expandGroup(g))
      case rc: RevealingChoice             => fc :: expandRc(rc)
      case _                               => fc :: Nil
    }

  private def mkJsFormComponentModels(fc: FormComponent): List[JsFormComponentModel] =
    fc.`type` match {
      case RevealingChoice(options, _) =>
        options.toList.flatMap { option =>
          option.revealingFields.map(rf => JsRevealingChoiceModel(fc.id, rf))
        }
      case group @ Group(fields, max, _, _, _) =>
        (0 until max.getOrElse(1)).toList.flatMap(index =>
          fields.map(field => JsFormComponentWithCtx(FormComponentWithGroup(addFieldIndex(field, index, group), fc))))
      case _ => JsFormComponentWithCtx(FormComponentSimple(fc)) :: Nil
    }

  def expandFormComponent(data: VariadicFormData): ExpandedFormComponent =
    ExpandedFormComponent(expandByData(this, data))
  def expandFormComponentRc(data: VariadicFormData): ExpandedFormComponent =
    ExpandedFormComponent(expandByDataRc(this, data))

  val expandFormComponentFull: ExpandedFormComponent = ExpandedFormComponent(expandAll(this))

  val jsFormComponentModels: List[JsFormComponentModel] = mkJsFormComponentModels(this)

}

object FormComponent {
  implicit val format: OFormat[FormComponent] = Json.format[FormComponent]
}

object IsText {
  def unapply(fc: FormComponent): Option[Text] = fc.`type`.cast[Text]
}

object IsCapitalised {
  def unapply(fc: FormComponent): Boolean =
    fc.`type` match {
      case t @ Text(_, _, _, IsUpperCase) => true
      case _                              => false
    }
}

object IsTextArea {
  def unapply(fc: FormComponent): Option[TextArea] = fc.`type`.cast[TextArea]
}

object IsGroup {
  def unapply[S](fc: FormComponent): Option[Group] = fc.`type`.cast[Group]
}

object IsMultiField {
  def unapply(fc: FormComponent): Option[MultiField] = fc.`type`.cast[MultiField]
}

object IsDate {
  def unapply(fc: FormComponent): Option[Date] = fc.`type`.cast[Date]
}

object IsChoice {
  def unapply(fc: FormComponent): Option[Choice] = fc.`type`.cast[Choice]
}

object IsRevealingChoice {
  def unapply[S](fc: FormComponent): Option[RevealingChoice] = fc.`type`.cast[RevealingChoice]
}

object IsAddress {
  def unapply(fc: FormComponent): Option[Address] = fc.`type`.cast[Address]
}

object IsUkSortCode {
  def unapply(fc: FormComponent): Option[UkSortCode] = fc.`type`.cast[UkSortCode]
}

object IsInformationMessage {
  def unapply(fc: FormComponent): Option[InformationMessage] = fc.`type`.cast[InformationMessage]
}

object IsHmrcTaxPeriod {
  def unapply(fc: FormComponent): Option[HmrcTaxPeriod] = fc.`type`.cast[HmrcTaxPeriod]
}

object IsFileUpload {
  def unapply(fc: FormComponent): Boolean = fc.`type`.cast[FileUpload].isDefined
}

object IsEmailVerifier {
  def unapply(formComponent: FormComponent): Option[(EmailFieldId, EmailVerifiedBy)] =
    formComponent.`type` match {
      case Text(evb @ EmailVerifiedBy(_, _), _, _, _) =>
        Some((emailFieldId(formComponent.id), evb))
      case _ => None
    }
}
