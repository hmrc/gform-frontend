/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.gform.eval.{ ExprType, StaticTypeData }
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId, MultiValueId }
import uk.gov.hmrc.gform.models.email.{ EmailFieldId, emailFieldId }
import uk.gov.hmrc.gform.sharedmodel.SmartString

case class FormComponent(
  id: FormComponentId,
  `type`: ComponentType,
  label: SmartString,
  helpText: Option[SmartString],
  shortName: Option[SmartString],
  includeIf: Option[IncludeIf],
  validIf: Option[ValidIf],
  mandatory: Boolean,
  editable: Boolean,
  submissible: Boolean,
  derived: Boolean,
  onlyShowOnSummary: Boolean = false,
  errorMessage: Option[SmartString],
  presentationHint: Option[List[PresentationHint]] = None,
  validators: List[FormComponentValidator] = Nil,
  instruction: Option[Instruction] = None
) {

  val modelComponentId: ModelComponentId = id.modelComponentId

  val baseComponentId: BaseComponentId = id.baseComponentId

  def atomicFormComponentId(atom: Atom): ModelComponentId.Atomic = id.toAtomicFormComponentId(atom)

  def childrenFormComponents: List[FormComponent] = `type` match {
    case t: RevealingChoice => t.options.toList.flatMap(_.revealingFields)
    case t: Group           => t.fields
    case _                  => Nil
  }

  def lookupFor: Map[ModelComponentId, FormComponent] = multiValueId.lookupFor(this)

  val multiValueId: MultiValueId = modelComponentId.fold(
    pure =>
      this match {
        case IsMultiField(multifield) => MultiValueId.multiValue(pure, multifield.fields(pure.indexedComponentId))
        case _                        => MultiValueId.pure(pure)
    }
  )(
    atomic => throw new IllegalArgumentException(s"$atomic cannot be broken into multiValues")
  )

  def firstAtomModelComponentId: ModelComponentId.Atomic = multiValueId.firstAtomModelComponentId

  private val exprType: ExprType = this match {
    case IsText(Text(Sterling(_, _), _, _, _, _, _))             => ExprType.number
    case IsText(Text(Number(_, _, _, _), _, _, _, _, _))         => ExprType.number
    case IsText(Text(PositiveNumber(_, _, _, _), _, _, _, _, _)) => ExprType.number
    case IsChoice(_)                                             => ExprType.ChoiceSelection
    case IsRevealingChoice(_)                                    => ExprType.ChoiceSelection
    case IsDate(_)                                               => ExprType.DateString
    case _                                                       => ExprType.String
  }

  private val textConstraint: Option[TextConstraint] = this match {
    case IsText(Text(tc @ Sterling(_, _), _, _, _, _, _))             => Some(tc)
    case IsText(Text(tc @ Number(_, _, _, _), _, _, _, _, _))         => Some(tc)
    case IsText(Text(tc @ PositiveNumber(_, _, _, _), _, _, _, _, _)) => Some(tc)
    case _                                                            => None
  }

  val staticTypeData: StaticTypeData = StaticTypeData(exprType, textConstraint)

  def hideOnSummary: Boolean =
    presentationHint.fold(false)(x => x.contains(InvisibleInSummary)) || IsInformationMessage.unapply(this).isDefined
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
      case t @ Text(_, _, _, IsUpperCase, _, _) => true
      case _                                    => false
    }
}

object IsTextArea {
  def unapply(fc: FormComponent): Option[TextArea] = fc.`type`.cast[TextArea]
}

object IsGroup {
  def unapply(fc: FormComponent): Option[Group] = fc.`type`.cast[Group]
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
  def unapply(fc: FormComponent): Option[RevealingChoice] = fc.`type`.cast[RevealingChoice]
}

object IsAddress {
  def unapply(fc: FormComponent): Option[Address] = fc.`type`.cast[Address]
}

object IsOverseasAddress {
  def unapply(fc: FormComponent): Option[OverseasAddress] = fc.`type`.cast[OverseasAddress]
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

object IsTime {
  def unapply(fc: FormComponent): Option[Time] = fc.`type`.cast[Time]
}

object IsTelephone {
  def unapply(fc: FormComponent): Boolean =
    fc.`type` match {
      case Text(TelephoneNumber, _, _, _, _, _) => true
      case _                                    => false
    }
}

object IsEmailVerifier {
  def unapply(formComponent: FormComponent): Option[(EmailFieldId, EmailVerifiedBy)] =
    formComponent.`type` match {
      case Text(evb @ EmailVerifiedBy(_, _), _, _, _, _, _) =>
        Some((emailFieldId(formComponent.id), evb))
      case _ => None
    }
}

object AllValidIfs {
  def unapply(fc: FormComponent): Option[List[ValidIf]] = (fc.validIf, fc.validators.map(_.validIf)) match {
    case (None, Nil)     => None
    case (None, xs2)     => Some(xs2)
    case (Some(xs), xs2) => Some(xs :: xs2)
  }
}
