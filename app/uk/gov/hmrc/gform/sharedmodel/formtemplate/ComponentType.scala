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

import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit.MINUTES
import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.foldable._
import julienrf.json.derived
import play.api.i18n.Messages
import play.api.libs.json._
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.{ IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ LocalisedString, SmartString, SourceOrigin, ValueClassFormat, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayWidth.DisplayWidth
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ FieldName, RoboticsXml, StructuredFormDataFieldNamePurpose }
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, HtmlFieldId }

import scala.annotation.tailrec
import scala.collection.immutable.List

sealed trait MultiField {

  def fields(indexedComponentId: IndexedComponentId): NonEmptyList[ModelComponentId.Atomic]

  def alternateNamesFor(atom: Atom): Map[StructuredFormDataFieldNamePurpose, FieldName] = Map.empty

}

sealed trait ComponentType {
  def showType: String = this match {
    case _: Text               => "text"
    case _: TextArea           => "textArea"
    case _: Date               => "date"
    case _: CalendarDate.type  => "calendarDate"
    case _: TaxPeriodDate.type => "taxPeriodDate"
    case _: PostcodeLookup     => "postcodeLookup"
    case _: Time               => "time"
    case _: Address            => "address"
    case _: OverseasAddress    => "overseasAddress"
    case _: Choice             => "choice"
    case _: RevealingChoice    => "revealingChoice"
    case _: HmrcTaxPeriod      => "hmrcTaxPeriod"
    case _: Group              => "group"
    case _: InformationMessage => "informationMessage"
    case _: FileUpload         => "fileUpload"
    case _: MiniSummaryList    => "miniSummaryList"
    case _: TableComp          => "table"
  }
}

case class Text(
  constraint: TextConstraint,
  value: Expr,
  displayWidth: DisplayWidth = DisplayWidth.DEFAULT,
  toUpperCase: UpperCaseBoolean = IsNotUpperCase,
  prefix: Option[SmartString] = None,
  suffix: Option[SmartString] = None
) extends ComponentType

sealed trait UpperCaseBoolean

case object IsUpperCase extends UpperCaseBoolean
case object IsNotUpperCase extends UpperCaseBoolean

object UpperCaseBoolean {
  implicit val format: OFormat[UpperCaseBoolean] = derived.oformat()
}

case class TextArea(
  constraint: TextConstraint,
  value: Expr,
  displayWidth: DisplayWidth = DisplayWidth.DEFAULT,
  rows: Int = TextArea.defaultRows,
  displayCharCount: Boolean = TextArea.defaultDisplayCharCount,
  dataThreshold: Option[Int]
) extends ComponentType

object TextArea {
  val defaultRows = 5
  val defaultDisplayCharCount = true
}

case class Date(
  constraintType: DateConstraintType,
  offset: Offset,
  value: Option[DateValue]
) extends ComponentType with MultiField {
  override def fields(indexedComponentId: IndexedComponentId): NonEmptyList[ModelComponentId.Atomic] =
    Date.fields(indexedComponentId)
}

object Date {
  val day: Atom = Atom("day")
  val month: Atom = Atom("month")
  val year: Atom = Atom("year")
  val fields: IndexedComponentId => NonEmptyList[ModelComponentId.Atomic] = (indexedComponentId: IndexedComponentId) =>
    NonEmptyList.of(day, month, year).map(ModelComponentId.atomicCurry(indexedComponentId))
}

case object CalendarDate extends ComponentType with MultiField {
  val day: Atom = Atom("day")
  val month: Atom = Atom("month")
  val componentFields: NonEmptyList[Atom] = NonEmptyList.of(day, month)

  override def fields(indexedComponentId: IndexedComponentId): NonEmptyList[ModelComponentId.Atomic] =
    componentFields.map(ModelComponentId.atomicCurry(indexedComponentId))
}

case class PostcodeLookup(chooseAddressLabel: Option[SmartString], confirmAddressLabel: Option[SmartString])
    extends ComponentType with MultiField {
  override def fields(indexedComponentId: IndexedComponentId): NonEmptyList[ModelComponentId.Atomic] =
    PostcodeLookup.fields(indexedComponentId)
}

case object PostcodeLookup {
  val postcode: Atom = Atom("postcode")
  val filter: Atom = Atom("filter")
  val componentFields: NonEmptyList[Atom] = NonEmptyList.of(postcode, filter)

  val fields: IndexedComponentId => NonEmptyList[ModelComponentId.Atomic] = (indexedComponentId: IndexedComponentId) =>
    componentFields.map(ModelComponentId.atomicCurry(indexedComponentId))
}

case object TaxPeriodDate extends ComponentType with MultiField {
  val month: Atom = Atom("month")
  val year: Atom = Atom("year")
  val componentFields: NonEmptyList[Atom] = NonEmptyList.of(month, year)

  override def fields(indexedComponentId: IndexedComponentId): NonEmptyList[ModelComponentId.Atomic] =
    componentFields.map(ModelComponentId.atomicCurry(indexedComponentId))
}

case class Address(
  international: Boolean,
  mandatoryFields: List[Address.Configurable.Mandatory],
  countyDisplayed: Boolean,
  value: Option[Expr]
) extends ComponentType with MultiField {

  val configurableMandatoryAtoms: Set[Atom] = mandatoryFields.map(_.toAtom).toSet

  override def fields(indexedComponentId: IndexedComponentId): NonEmptyList[ModelComponentId.Atomic] =
    Address.fields(indexedComponentId)

  override def alternateNamesFor(atom: Atom): Map[StructuredFormDataFieldNamePurpose, FieldName] =
    Map(RoboticsXml -> FieldName(atom.value.replace("street", "line")))

  private val allOptionalAtoms: Set[Atom] =
    Set(Address.street3).filterNot(configurableMandatoryAtoms)

  def isOptional(atom: Atom): Boolean = allOptionalAtoms(atom)

}

object Address {
  val street1: Atom = Atom("street1")
  val street2: Atom = Atom("street2")
  val street3: Atom = Atom("street3")
  val street4: Atom = Atom("street4")
  val uk: Atom = Atom("uk")
  val postcode: Atom = Atom("postcode")
  val country: Atom = Atom("country")

  val fields: IndexedComponentId => NonEmptyList[ModelComponentId.Atomic] = indexedComponentId =>
    NonEmptyList
      .of(street1, street2, street3, street4, uk, postcode, country)
      .map(ModelComponentId.atomicCurry(indexedComponentId))

  val summaryPageFields: IndexedComponentId => NonEmptyList[ModelComponentId.Atomic] = indexedComponentId =>
    NonEmptyList
      .of(street1, street2, street3, street4, postcode, country)
      .map(ModelComponentId.atomicCurry(indexedComponentId))

  def renderToString(formComponent: FormComponent, formFieldValidationResult: FormFieldValidationResult): List[String] =
    summaryPageFields(formComponent.modelComponentId.indexedComponentId)
      .map(modelComponentId => formFieldValidationResult.getCurrentValue(HtmlFieldId.pure(modelComponentId)))
      .filter(_.trim.nonEmpty)

  object Configurable {
    sealed trait Mandatory {
      def toAtom: Atom = this match {
        case Mandatory.City => street3
      }
    }
    object Mandatory {
      case object City extends Mandatory
      implicit val format: OFormat[Mandatory] = derived.oformat()
    }
  }
}

case class OverseasAddress(
  mandatoryFields: List[OverseasAddress.Configurable.Mandatory],
  optionalFields: List[OverseasAddress.Configurable.Optional],
  countryLookup: Boolean,
  value: Option[Expr],
  countryDisplayed: Boolean
) extends ComponentType with MultiField {
  override def fields(indexedComponentId: IndexedComponentId): NonEmptyList[ModelComponentId.Atomic] =
    if (countryDisplayed)
      OverseasAddress.fields(indexedComponentId)
    else OverseasAddress.fieldsWithoutCountry(indexedComponentId)

  val configurableMandatoryAtoms: Set[Atom] = mandatoryFields.map(_.toAtom).toSet
  val configurableOptionalAtoms: Set[Atom] = optionalFields.map(_.toAtom).toSet

  val allOptionalAtoms: Set[Atom] =
    configurableOptionalAtoms ++
      Set(OverseasAddress.line2, OverseasAddress.postcode).filterNot(configurableMandatoryAtoms) +
      OverseasAddress.line3 // Line3 is always optional

  def isOptional(atom: Atom): Boolean = allOptionalAtoms(atom)
}

object OverseasAddress {

  val line1: Atom = Atom("line1")
  val line2: Atom = Atom("line2")
  val line3: Atom = Atom("line3")
  val city: Atom = Atom("city")
  val postcode: Atom = Atom("postcode")
  val country: Atom = Atom("country")

  val allAtoms: NonEmptyList[Atom] = NonEmptyList.of(line1, line2, line3, city, postcode, country)

  object Configurable {
    sealed trait Mandatory {
      def toAtom: Atom = this match {
        case Mandatory.Line2    => line2
        case Mandatory.Postcode => postcode
      }
    }
    object Mandatory {
      case object Line2 extends Mandatory
      case object Postcode extends Mandatory
      implicit val format: OFormat[Mandatory] = derived.oformat()
    }

    sealed trait Optional {
      def toAtom: Atom = this match {
        case Optional.City => city
      }
    }
    object Optional {
      case object City extends Optional
      implicit val format: OFormat[Optional] = derived.oformat()
    }
  }

  implicit val format: OFormat[OverseasAddress] = derived.oformat()

  val fields: IndexedComponentId => NonEmptyList[ModelComponentId.Atomic] =
    indexedComponentId => {
      NonEmptyList
        .of(line1, line2, line3, city, postcode, country)
        .map(ModelComponentId.atomicCurry(indexedComponentId))
    }

  val fieldsWithoutCountry: IndexedComponentId => NonEmptyList[ModelComponentId.Atomic] =
    indexedComponentId => {
      NonEmptyList
        .of(line1, line2, line3, city, postcode)
        .map(ModelComponentId.atomicCurry(indexedComponentId))
    }

  private val summaryPageFields: IndexedComponentId => NonEmptyList[ModelComponentId.Atomic] = indexedComponentId =>
    allAtoms.map(ModelComponentId.atomicCurry(indexedComponentId))

  def renderToString(formComponent: FormComponent, formFieldValidationResult: FormFieldValidationResult): List[String] =
    summaryPageFields(formComponent.modelComponentId.indexedComponentId)
      .map(modelComponentId => formFieldValidationResult.getCurrentValue(HtmlFieldId.pure(modelComponentId)))
      .filter(_.trim.nonEmpty)

}

object DisplayWidth extends Enumeration {
  type DisplayWidth = Value
  val XS, S, M, L, XL, XXL, DEFAULT = Value

  implicit val displayWidthReads: Reads[DisplayWidth] = Reads.enumNameReads(DisplayWidth)
  implicit val displayWidthWrites: Writes[DisplayWidth] = Writes.enumNameWrites
}

object SummaryDisplayWidth extends Enumeration {
  type SummaryDisplayWidth = Value
  val M, L, XL = Value

  implicit val displayWidthReads: Reads[SummaryDisplayWidth] = Reads.enumNameReads(SummaryDisplayWidth)
  implicit val displayWidthWrites: Writes[SummaryDisplayWidth] = Writes.enumNameWrites
}

object TaskListDisplayWidth extends Enumeration {
  type TaskListDisplayWidth = Value
  val M, L, XL = Value

  implicit val displayWidthReads: Reads[TaskListDisplayWidth] = Reads.enumNameReads(TaskListDisplayWidth)
  implicit val displayWidthWrites: Writes[TaskListDisplayWidth] = Writes.enumNameWrites
}

sealed trait Dynamic extends Product with Serializable

object Dynamic {

  final case class ATLBased(formComponentId: FormComponentId) extends Dynamic
  final case class DataRetrieveBased(indexOfDataRetrieveCtx: IndexOfDataRetrieveCtx) extends Dynamic

  implicit val dataRetrieveCtx: OFormat[DataRetrieveCtx] = derived.oformat()
  implicit val indexOfDataRetrieveCtx: OFormat[IndexOfDataRetrieveCtx] = derived.oformat()
  implicit val format: OFormat[Dynamic] = derived.oformat()
}

sealed trait OptionData extends Product with Serializable {
  def label: SmartString
  def hint: Option[SmartString]
  def dynamic: Option[Dynamic]

  def value(index: Int): String = this match {
    case o @ OptionData.ValueBased(_, _, _, _, OptionDataValue.StringBased(value)) => value
    case _                                                                         => index.toString
  }
  def getValue[D <: DataOrigin](index: Int, formModelVisibilityOptics: FormModelVisibilityOptics[D])(implicit
    m: Messages
  ): String =
    this match {
      case o: OptionData.IndexBased                                                  => index.toString
      case o @ OptionData.ValueBased(_, _, _, _, OptionDataValue.StringBased(value)) => value
      case o @ OptionData.ValueBased(_, _, _, _, OptionDataValue.ExprBased(prefix, expr)) =>
        prefix + formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr).stringRepresentation
      case o @ OptionData.ValueBased(_, _, _, _, OptionDataValue.FormCtxBased(formCtx)) =>
        formModelVisibilityOptics.evalAndApplyTypeInfoFirst(formCtx).stringRepresentation
    }
}

object OptionData {

  case class IndexBased(
    label: SmartString,
    hint: Option[SmartString],
    includeIf: Option[IncludeIf],
    dynamic: Option[Dynamic]
  ) extends OptionData

  case class ValueBased(
    label: SmartString,
    hint: Option[SmartString],
    includeIf: Option[IncludeIf],
    dynamic: Option[Dynamic],
    value: OptionDataValue
  ) extends OptionData

  implicit val format: OFormat[OptionData] = derived.oformat()
}
sealed trait NoneChoice extends Product with Serializable {
  def selection: String = this match {
    case o: NoneChoice.IndexBased => (o.index - 1).toString
    case o: NoneChoice.ValueBased => o.value
  }
}

object NoneChoice {

  case class IndexBased(index: Int) extends NoneChoice
  case class ValueBased(value: String) extends NoneChoice

  implicit val format: OFormat[NoneChoice] = derived.oformat()
}

sealed trait DividerPosition extends Product with Serializable

object DividerPosition {
  final case class Number(pos: Int) extends DividerPosition
  final case class Value(value: String) extends DividerPosition
  implicit val format: OFormat[DividerPosition] = derived.oformat()
}
case class Choice(
  `type`: ChoiceType,
  options: NonEmptyList[OptionData],
  orientation: Orientation,
  selections: List[Int],
  hints: Option[NonEmptyList[SmartString]],
  optionHelpText: Option[NonEmptyList[SmartString]],
  dividerPosition: Option[DividerPosition],
  dividerText: LocalisedString,
  noneChoice: Option[NoneChoice],
  noneChoiceError: Option[LocalisedString]
) extends ComponentType {
  def renderToString[D <: DataOrigin](
    formComponent: FormComponent,
    formFieldValidationResult: FormFieldValidationResult,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    evaluator: SmartStringEvaluator,
    m: Messages
  ): List[String] =
    options.toList.zipWithIndex
      .map { case (option, index) =>
        formFieldValidationResult
          .getOptionalCurrentValue(
            HtmlFieldId.indexed(formComponent.id, option.getValue(index, formModelVisibilityOptics))
          )
          .map(_ => option.label.value())
      }
      .collect { case Some(selection) => selection }
}

sealed trait ChoiceType
final case object Radio extends ChoiceType
final case object Checkbox extends ChoiceType
final case object YesNo extends ChoiceType

object ChoiceType {
  implicit val format: OFormat[ChoiceType] = derived.oformat()
  implicit val equal: Eq[ChoiceType] = Eq.fromUniversalEquals

}

case class RevealingChoiceElement(
  choice: OptionData,
  revealingFields: List[FormComponent],
  hint: Option[SmartString],
  selected: Boolean
)
object RevealingChoiceElement {
  implicit val format: OFormat[RevealingChoiceElement] = derived.oformat()
}
// options is NonEmptyList on the backend, but it needs to support 'emptiness' here due to need of Visibility model
// ie. it needs to be able to represent that no option has been selected by a user
case class RevealingChoice(options: List[RevealingChoiceElement], multiValue: Boolean) extends ComponentType
object RevealingChoice {
  implicit val format: OFormat[RevealingChoice] = derived.oformat()

  def slice[S <: SourceOrigin](fcId: FormComponentId): VariadicFormData[S] => RevealingChoice => RevealingChoice =
    data =>
      revealingChoice => {
        val indices: List[String] =
          data.many(fcId.modelComponentId).toList.flatten.sorted

        val rcElements: List[RevealingChoiceElement] =
          revealingChoice.options.zipWithIndex.collect {
            case (rcElement, index) if indices.contains(rcElement.choice.value(index)) =>
              rcElement
          }

        revealingChoice.copy(options = rcElements)
      }
}

case class IdType(value: String) extends AnyVal
case class RegimeType(value: String) extends AnyVal

object IdType {
  implicit val format: OFormat[IdType] = ValueClassFormat.oformat("idType", IdType.apply, _.value)
}
object RegimeType {
  implicit val format: OFormat[RegimeType] = ValueClassFormat.oformat("regimeType", RegimeType.apply, _.value)
}

case class HmrcTaxPeriod(idType: IdType, idNumber: Expr, regimeType: RegimeType) extends ComponentType

object HmrcTaxPeriod {
  implicit val catsEq: Eq[HmrcTaxPeriod] = Eq.fromUniversalEquals
  implicit val format: OFormat[HmrcTaxPeriod] = derived.oformat()
}

sealed trait Orientation
case object Vertical extends Orientation
case object Horizontal extends Orientation
object Orientation {
  implicit val catsEq: Eq[Orientation] = Eq.fromUniversalEquals
  implicit val format: OFormat[Orientation] = derived.oformat()
}

sealed trait InfoType
case object StandardInfo extends InfoType

case object LongInfo extends InfoType

case object ImportantInfo extends InfoType

case object BannerInfo extends InfoType

case object NoFormat extends InfoType

object InfoType {
  implicit val format: OFormat[InfoType] = derived.oformat()
}

case class Group(
  fields: List[FormComponent],
  repeatsMax: Option[Int] = None,
  repeatsMin: Option[Int] = None,
  repeatLabel: Option[SmartString] = None,
  repeatAddAnotherText: Option[SmartString] = None
) extends ComponentType

case class InformationMessage(infoType: InfoType, infoText: SmartString) extends ComponentType

sealed trait FileUploadProvider

object FileUploadProvider {
  final case class Upscan(compression: Boolean) extends FileUploadProvider
  case object FileUploadFrontend extends FileUploadProvider

  implicit val format: OFormat[FileUploadProvider] = derived.oformat()
}

case class FileUpload(
  fileUploadProvider: FileUploadProvider,
  fileSizeLimit: Option[Int],
  allowedFileTypes: Option[AllowedFileTypes]
) extends ComponentType

case class StartTime(time: LocalTime) extends AnyVal

object StartTime {
  implicit val format: OFormat[StartTime] = derived.oformat()
}

case class EndTime(time: LocalTime) extends AnyVal

object EndTime {
  implicit val format: OFormat[EndTime] = derived.oformat()
}

case class Range(startTime: StartTime, endTime: EndTime)

object Range {
  implicit val format: OFormat[Range] = derived.oformat()

  def stringToLocalTime(formatter: DateTimeFormatter, time: String): LocalTime =
    LocalTime.parse(time, formatter)

  val twelveHoursFormat = DateTimeFormatter.ofPattern("hh:mm a")

  @tailrec
  def getTimeSlots(sTime: LocalTime, eTime: LocalTime, iMins: Int, acc: List[LocalTime]): List[LocalTime] = {
    val t = sTime.plusMinutes(iMins.toLong)
    if (t.isAfter(eTime) || (0 until iMins contains MINUTES.between(LocalTime.parse("00:00"), t).toInt))
      acc
    else
      getTimeSlots(t, eTime, iMins, acc :+ t)
  }

  def timeSlots(time: Time): List[String] =
    time.ranges
      .flatMap(t =>
        getTimeSlots(t.startTime.time, t.endTime.time, time.intervalMins.intervalMins, List(t.startTime.time))
      )
      .distinct
      .map(_.format(twelveHoursFormat))
}

case class IntervalMins(intervalMins: Int) extends AnyVal

object IntervalMins {
  implicit val format: OFormat[IntervalMins] = derived.oformat()
}

case class Time(ranges: List[Range], intervalMins: IntervalMins) extends ComponentType

object Time {
  implicit val format: OFormat[Time] = derived.oformat()
}

sealed trait MiniSummaryRow extends Product with Serializable
object MiniSummaryRow {
  case class ValueRow(
    key: Option[SmartString],
    value: MiniSummaryListValue,
    includeIf: Option[IncludeIf]
  ) extends MiniSummaryRow

  case class SmartStringRow(
    key: Option[SmartString],
    value: SmartString,
    includeIf: Option[IncludeIf]
  ) extends MiniSummaryRow

  case class HeaderRow(
    header: SmartString
  ) extends MiniSummaryRow

  case class ATLRow(
    atlId: FormComponentId,
    includeIf: Option[IncludeIf],
    rows: List[MiniSummaryRow]
  ) extends MiniSummaryRow

  implicit val format: Format[MiniSummaryRow] = derived.oformat()
}

case class MiniSummaryList(rows: List[MiniSummaryRow]) extends ComponentType
object MiniSummaryList {
  implicit val format: Format[MiniSummaryList] = derived.oformat()
}

case class TableValue(value: SmartString, cssClass: Option[String], colspan: Option[Int], rowspan: Option[Int]) {
  def decrementRowSpan =
    if (rowspan.exists(_ > 1))
      TableValue(value, cssClass, colspan, rowspan.map(_ - 1))
    else TableValue(value, cssClass, colspan, None)
}

object TableValue {
  implicit val format: Format[TableValue] = derived.oformat()
}

case class TableValueRow(
  values: List[TableValue],
  includeIf: Option[IncludeIf],
  dynamic: Option[Dynamic]
)

object TableValueRow {

  implicit val format: Format[TableValueRow] = derived.oformat()
}

case class TableComp(
  header: List[SmartString],
  rows: List[TableValueRow],
  summaryValue: SmartString,
  caption: Option[String] = None,
  captionClasses: String = "",
  classes: String = "",
  firstCellIsHeader: Boolean = false
) extends ComponentType

object TableComp {
  implicit val format: Format[TableComp] = derived.oformat()
}

object ComponentType {

  implicit def readsNonEmptyList[T: Reads]: Reads[NonEmptyList[T]] = Reads[NonEmptyList[T]] { json =>
    Json.fromJson[List[T]](json).flatMap {
      case Nil     => JsError(JsonValidationError(s"Required at least one element. Got: $json"))
      case x :: xs => JsSuccess(NonEmptyList(x, xs))
    }
  }

  implicit def writesNonEmptyList[T: Writes]: Writes[NonEmptyList[T]] = Writes[NonEmptyList[T]] { v =>
    JsArray((v.head :: v.tail).map(Json.toJson(_)))
  }

  implicit val format: OFormat[ComponentType] = derived.oformat()
}
