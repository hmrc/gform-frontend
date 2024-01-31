/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.gform.ops.FormComponentOps

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
  instruction: Option[Instruction] = None,
  labelSize: Option[LabelSize] = None,
  errorShortName: Option[SmartString] = None,
  errorShortNameStart: Option[SmartString] = None,
  errorExample: Option[SmartString] = None,
  extraLetterSpacing: Option[Boolean] = None
) {

  val modelComponentId: ModelComponentId = id.modelComponentId

  val baseComponentId: BaseComponentId = id.baseComponentId

  def atomicFormComponentId(atom: Atom): ModelComponentId.Atomic = id.toAtomicFormComponentId(atom)

  val errorExampleWithCommaOrBlank =
    errorExample.map(_.transform(v => s", $v", v => s", $v")).getOrElse(SmartString.blank)

  val message = this match {
    case IsTelephone()                        => Some("telephoneNumber")
    case fc if fc.isUkSortCode                => Some("ukSortCode")
    case fc if fc.isCtUtr                     => Some("ctUtr")
    case fc if fc.isSaUtr                     => Some("saUtr")
    case fc if fc.isNino                      => Some("nino")
    case fc if fc.isPayeReference             => Some("payeReference")
    case fc if fc.isChildBenefitNumber        => Some("childBenefitNumber")
    case fc if fc.isEORI                      => Some("EORI")
    case fc if fc.isUkEORI                    => Some("UkEORI")
    case fc if fc.isUkVrn                     => Some("ukVrn")
    case fc if fc.isCompanyRegistrationNumber => Some("companyRegistrationNumber")
    case _                                    => None
  }

  def childrenFormComponents: List[FormComponent] = `type` match {
    case t: RevealingChoice => t.options.toList.flatMap(_.revealingFields)
    case t: Group           => t.fields
    case _                  => Nil
  }

  def lookupFor: Map[ModelComponentId, FormComponent] = multiValueId.lookupFor(this)

  val multiValueId: MultiValueId = modelComponentId.fold(pure =>
    this match {
      case IsMultiField(multifield) => MultiValueId.multiValue(pure, multifield.fields(pure.indexedComponentId))
      case _                        => MultiValueId.pure(pure)
    }
  )(atomic => throw new IllegalArgumentException(s"$atomic cannot be broken into multiValues"))

  def firstAtomModelComponentId: ModelComponentId.Atomic = multiValueId.firstAtomModelComponentId

  private val exprType: ExprType = this match {
    case IsText(Text(Sterling(_, _), _, _, _, _, _))             => ExprType.number
    case IsText(Text(WholeSterling(_), _, _, _, _, _))           => ExprType.number
    case IsText(Text(Number(_, _, _, _), _, _, _, _, _))         => ExprType.number
    case IsText(Text(PositiveNumber(_, _, _, _), _, _, _, _, _)) => ExprType.number
    case IsText(Text(YearFormat, _, _, _, _, _))                 => ExprType.number
    case IsChoice(_)                                             => ExprType.ChoiceSelection
    case IsRevealingChoice(_)                                    => ExprType.ChoiceSelection
    case IsDate(_)                                               => ExprType.DateString
    case IsAddress(_) | IsOverseasAddress(_)                     => ExprType.AddressString
    case IsTaxPeriodDate()                                       => ExprType.TaxPeriod
    case _                                                       => ExprType.String
  }

  private val textConstraint: Option[TextConstraint] = this match {
    case IsText(Text(tc @ Sterling(_, _), _, _, _, _, _))             => Some(tc)
    case IsText(Text(tc @ WholeSterling(_), _, _, _, _, _))           => Some(tc)
    case IsText(Text(tc @ Number(_, _, _, _), _, _, _, _, _))         => Some(tc)
    case IsText(Text(tc @ PositiveNumber(_, _, _, _), _, _, _, _, _)) => Some(tc)
    case IsText(Text(UkSortCodeFormat, _, _, _, _, _))                => Some(UkSortCodeFormat)
    case _                                                            => None
  }

  val staticTypeData: StaticTypeData = StaticTypeData(exprType, textConstraint)

  def hideOnSummary: Boolean =
    presentationHint.fold(false)(x => x.contains(InvisibleInSummary)) ||
      IsInformationMessage.unapply(this).isDefined

  def withIndex(index: Int) = copy(id = id.withIndex(index))

  val errorPlaceholder = errorShortName orElse shortName
}

object FormComponent {
  implicit val format: OFormat[FormComponent] = Json.format[FormComponent]
}

object IsText {
  def unapply(fc: FormComponent): Option[Text] = fc.`type`.cast[Text]
}

object IsEmail {
  def unapply(fc: FormComponent): Boolean =
    fc.`type` match {
      case Text(Email, _, _, _, _, _) => true
      case _                          => false
    }
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

object IsCalendarDate {
  def unapply(fc: FormComponent): Boolean = fc.`type` match {
    case CalendarDate => true
    case _            => false
  }
}

object IsPostcodeLookup {
  def unapply(fc: FormComponent): Option[PostcodeLookup] = fc.`type`.cast[PostcodeLookup]
}

object IsTaxPeriodDate {
  def unapply(fc: FormComponent): Boolean = fc.`type` match {
    case TaxPeriodDate => true
    case _             => false
  }
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

object IsInformationMessage {
  def unapply(fc: FormComponent): Option[InformationMessage] = fc.`type`.cast[InformationMessage]
}

object IsMiniSummaryList {
  def unapply(fc: FormComponent): Option[MiniSummaryList] = fc.`type`.cast[MiniSummaryList]
}

object IsTableComp {
  def unapply(fc: FormComponent): Option[TableComp] = fc.`type`.cast[TableComp]
}

object IsSummarySection {
  def unapply(fc: FormComponent): Option[SummarySection] = fc.`type`.cast[SummarySection]
}

object IsDeclarationSection {
  def unapply(fc: FormComponent): Option[DeclarationSection] = fc.`type`.cast[DeclarationSection]
}

object IsHmrcTaxPeriod {
  def unapply(fc: FormComponent): Option[HmrcTaxPeriod] = fc.`type`.cast[HmrcTaxPeriod]
}

object IsFileUpload {
  def unapply(fc: FormComponent): Option[FileUpload] = fc.`type`.cast[FileUpload]
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

object HasDynamicChoice {
  def unapply(fc: FormComponent): Option[(FormComponentId, Set[BaseComponentId])] =
    fc.`type` match {
      case c: Choice =>
        val baseComponentIds = c.options.toList
          .map(_.dynamic)
          .collect { case Some(Dynamic.ATLBased(formComponentId)) =>
            formComponentId.baseComponentId
          }
          .toSet

        if (baseComponentIds.isEmpty) {
          None
        } else {
          Some(fc.id -> baseComponentIds)
        }

      case _ => None
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

object AllChoiceIncludeIfs {
  def unapply(fc: FormComponent): Option[List[IncludeIf]] = fc match {
    case IsChoice(c) =>
      Some(c.options.toList.flatMap {
        case OptionData.IndexBased(_, _, includeIf, _)    => includeIf
        case OptionData.ValueBased(_, _, includeIf, _, _) => includeIf
      })
    case IsRevealingChoice(rc) =>
      Some(rc.options.map(_.choice).flatMap {
        case OptionData.IndexBased(_, _, includeIf, _)    => includeIf
        case OptionData.ValueBased(_, _, includeIf, _, _) => includeIf
      })
    case _ => None
  }
}

object AllMiniSummaryListIncludeIfs {
  def unapply(fc: FormComponent): Option[List[IncludeIf]] = fc match {
    case IsMiniSummaryList(c) =>
      Some(c.rows.flatMap {
        case MiniSummaryRow.ValueRow(_, _, includeIf, _)       => includeIf
        case MiniSummaryRow.SmartStringRow(_, _, includeIf, _) => includeIf
        case MiniSummaryRow.HeaderRow(_)                       => None
        case MiniSummaryRow.ATLRow(_, includeIf, _)            => includeIf
      })
    case _ => None
  }
}

object SummarySectionIncludeIf {
  def unapply(fc: FormComponent): Option[IncludeIf] = fc match {
    case IsSummarySection(ss) => ss.includeIf
    case _                    => None
  }
}

object DeclarationSectionIncludeIf {
  def unapply(fc: FormComponent): Option[IncludeIf] = fc match {
    case IsDeclarationSection(ss) => ss.includeIf
    case _                        => None
  }
}

object IsUpscanInitiateFileUpload {
  def unapply(formComponent: FormComponent): Option[FormComponent] =
    formComponent.`type` match {
      case FileUpload(FileUploadProvider.Upscan(_), _, _) => Some(formComponent)
      case _                                              => None
    }
}
