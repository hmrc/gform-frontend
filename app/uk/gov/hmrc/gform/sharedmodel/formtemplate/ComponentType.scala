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

import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit.MINUTES

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.foldable._
import julienrf.json.derived
import play.api.libs.json._

import scala.util.Try
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.{ IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.{ SmartString, SourceOrigin, ValueClassFormat, VariadicFormData }
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
    case _: Time               => "time"
    case _: Address            => "address"
    case _: OverseasAddress    => "overseasAddress"
    case _: Choice             => "choice"
    case _: RevealingChoice    => "revealingChoice"
    case _: HmrcTaxPeriod      => "hmrcTaxPeriod"
    case _: Group              => "group"
    case _: InformationMessage => "informationMessage"
    case _: FileUpload         => "fileUpload"
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
  displayCharCount: Boolean = TextArea.defaultDisplayCharCount
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

case class Address(international: Boolean) extends ComponentType with MultiField {
  override def fields(indexedComponentId: IndexedComponentId): NonEmptyList[ModelComponentId.Atomic] =
    Address.fields(indexedComponentId)

  override def alternateNamesFor(atom: Atom): Map[StructuredFormDataFieldNamePurpose, FieldName] =
    Map(RoboticsXml -> FieldName(atom.value.replace("street", "line")))

}

object Address {
  val street1: Atom = Atom("street1")
  val street2: Atom = Atom("street2")
  val street3: Atom = Atom("street3")
  val street4: Atom = Atom("street4")
  val uk: Atom = Atom("uk")
  val postcode: Atom = Atom("postcode")
  val country: Atom = Atom("country")
  val mandatoryFields: IndexedComponentId => NonEmptyList[ModelComponentId.Atomic] = indexedComponentId =>
    NonEmptyList.one(street1).map(ModelComponentId.atomicCurry(indexedComponentId))
  val optionalFields: IndexedComponentId => NonEmptyList[ModelComponentId.Atomic] = indexedComponentId =>
    NonEmptyList
      .of(street2, street3, street4, uk, postcode, country)
      .map(ModelComponentId.atomicCurry(indexedComponentId))
  val fields: IndexedComponentId => NonEmptyList[ModelComponentId.Atomic] = indexedComponentId =>
    mandatoryFields(indexedComponentId).concatNel(optionalFields(indexedComponentId))

  private val summaryPageFields: IndexedComponentId => NonEmptyList[ModelComponentId.Atomic] = indexedComponentId =>
    NonEmptyList
      .of(street1, street2, street3, street4, postcode, country)
      .map(ModelComponentId.atomicCurry(indexedComponentId))

  def renderToString(formComponent: FormComponent, formFieldValidationResult: FormFieldValidationResult): List[String] =
    summaryPageFields(formComponent.modelComponentId.indexedComponentId)
      .map(modelComponentId => formFieldValidationResult.getCurrentValue(HtmlFieldId.pure(modelComponentId)))
      .filter(_.trim.nonEmpty)
}

case class OverseasAddress(
  mandatoryFields: List[OverseasAddress.Configurable.Mandatory],
  optionalFields: List[OverseasAddress.Configurable.Optional],
  value: Option[OverseasAddress.Value],
  countryLookup: Boolean
) extends ComponentType with MultiField {
  override def fields(indexedComponentId: IndexedComponentId): NonEmptyList[ModelComponentId.Atomic] =
    OverseasAddress.fields(indexedComponentId)

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

  case class Value(
    line1: SmartString,
    line2: SmartString,
    line3: SmartString,
    city: SmartString,
    postcode: SmartString,
    country: SmartString
  ) {
    def getPrepopValue(atom: Atom)(implicit
      sse: SmartStringEvaluator
    ): String = atom match {
      case OverseasAddress.line1    => line1.value
      case OverseasAddress.line2    => line2.value
      case OverseasAddress.line3    => line3.value
      case OverseasAddress.city     => city.value
      case OverseasAddress.postcode => postcode.value
      case OverseasAddress.country  => country.value
      case _                        => ""
    }
  }

  object Value {
    implicit val format: OFormat[Value] = derived.oformat()
  }

  implicit val format: OFormat[OverseasAddress] = derived.oformat()

  val fields: IndexedComponentId => NonEmptyList[ModelComponentId.Atomic] =
    indexedComponentId => {
      NonEmptyList
        .of(line1, line2, line3, city, postcode, country)
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

case class Choice(
  `type`: ChoiceType,
  options: NonEmptyList[SmartString],
  orientation: Orientation,
  selections: List[Int],
  hints: Option[NonEmptyList[SmartString]],
  optionHelpText: Option[NonEmptyList[SmartString]]
) extends ComponentType {
  def renderToString(formComponent: FormComponent, formFieldValidationResult: FormFieldValidationResult)(implicit
    evaluator: SmartStringEvaluator
  ): List[String] =
    options.toList.zipWithIndex
      .map { case (option, index) =>
        formFieldValidationResult
          .getOptionalCurrentValue(HtmlFieldId.indexed(formComponent.id, index))
          .map(_ => option.value)
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
  choice: SmartString,
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
        val indices: List[Int] =
          data.many(fcId.modelComponentId).toList.flatten.flatMap(index => Try(index.toInt).toOption.toList).sorted

        val rcElements: List[RevealingChoiceElement] = indices.map(revealingChoice.options)

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
  case object Upscan extends FileUploadProvider
  case object FileUploadFrontend extends FileUploadProvider

  implicit val format: OFormat[FileUploadProvider] = derived.oformat()
}

case class FileUpload(
  fileUploadProvider: FileUploadProvider
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
    if (
      t.isAfter(eTime) || (0 until iMins contains MINUTES
        .between(LocalTime.parse("00:00"), t))
    )
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
