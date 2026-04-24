/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.recalculation

import cats.Eq
import cats.syntax.all._
import java.time.{ LocalDate, Period }
import java.time.format.TextStyle
import java.util.Locale
import play.api.i18n.Messages
import scala.util.Try
import scala.util.matching.Regex
import uk.gov.hmrc.gform.commons.BigDecimalUtil.toBigDecimalSafe
import uk.gov.hmrc.gform.commons.NumberSetScale
import uk.gov.hmrc.gform.eval.{ StaticTypeData, TypeInfo }
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Address, FormComponentId, Number, PositiveNumber, Sterling, TextConstraint, WholeSterling }
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.views.summary.TextFormatter

sealed trait EvaluationStatus extends Product with Serializable {

  import EvaluationStatus._

  private def invalidAdd(left: EvaluationStatus, right: EvaluationStatus): EvaluationStatus =
    Invalid(s"Unsupported operation, cannot add $left and $right")

  private def invalidSubstract(left: EvaluationStatus, right: EvaluationStatus): EvaluationStatus =
    Invalid(s"Unsupported operation, cannot substract $left and $right")

  private def invalidMult(left: EvaluationStatus, right: EvaluationStatus): EvaluationStatus =
    Invalid(s"Unsupported operation, cannot multiply $left and $right")

  private def invalidDiv(left: EvaluationStatus, right: EvaluationStatus): EvaluationStatus =
    Invalid(s"Unsupported operation, cannot divide $left and $right")

  private def errorInCalculation[T](op: String): T =
    throw new Exception(s"Dirty cannot be part of evaluation $op. This: $this")

  private def errorInCalculation[T](op: String, that: EvaluationStatus): T =
    throw new Exception(s"Dirty cannot be part of evaluation $op. This: $this, that: $that")

  def >(that: EvaluationStatus): List[Boolean] = this match {
    case Hidden          => false :: Nil
    case Empty           => false :: Nil
    case t: NumberResult => that.ifNumberResult(t.value > _) :: Nil
    case t: StringResult => that.ifStringResult(t.value > _) :: Nil
    case t: OptionResult =>
      (that.ifOptionResult(t.value.min > _.min) ||
        that.ifNumberResult { numberResult =>
          compareNumberWithOption(numberResult, t, _ > _)
        }) :: Nil
    case t: DateResult => false :: Nil
    case t: ListResult =>
      that match {
        case t2: ListResult => t.list.zipAll(t2.list, Empty, Empty).flatMap { case (l, r) => l > r }
        case otherwise      => t.list.flatMap(_ > otherwise)
      }

    case t: PeriodResult  => that.ifPeriodResult(t.value.toTotalMonths > _.toTotalMonths) :: Nil
    case t: AddressResult => false :: Nil
    case t: Invalid       => false :: Nil
    case Dirty            => errorInCalculation(">", that)
  }

  def <(that: EvaluationStatus): List[Boolean] = this match {
    case Hidden          => false :: Nil
    case Empty           => false :: Nil
    case t: NumberResult => that.ifNumberResult(t.value < _) :: Nil
    case t: StringResult => that.ifStringResult(t.value < _) :: Nil
    case t: OptionResult =>
      (that.ifOptionResult(t.value.min < _.min) ||
        that.ifNumberResult { numberResult =>
          compareNumberWithOption(numberResult, t, _ < _)
        }) :: Nil
    case t: DateResult => false :: Nil
    case t: ListResult =>
      that match {
        case t2: ListResult => t.list.zipAll(t2.list, Empty, Empty).flatMap { case (l, r) => l < r }
        case otherwise      => t.list.flatMap(_ < otherwise)
      }
    case t: PeriodResult  => that.ifPeriodResult(t.value.toTotalMonths < _.toTotalMonths) :: Nil
    case t: AddressResult => false :: Nil
    case t: Invalid       => false :: Nil
    case Dirty            => errorInCalculation("<", that)
  }

  def <=(that: EvaluationStatus): List[Boolean] = this match {
    case Hidden          => false :: Nil
    case Empty           => false :: Nil
    case t: NumberResult => that.ifNumberResult(t.value <= _) :: Nil
    case t: StringResult => that.ifStringResult(t.value <= _) :: Nil
    case t: OptionResult =>
      (that.ifOptionResult(t.value.min <= _.min) ||
        that.ifNumberResult { numberResult =>
          compareNumberWithOption(numberResult, t, _ <= _)
        }) :: Nil
    case t: DateResult => false :: Nil
    case t: ListResult =>
      that match {
        case t2: ListResult => t.list.zipAll(t2.list, Empty, Empty).flatMap { case (l, r) => l <= r }
        case otherwise      => t.list.flatMap(_ <= otherwise)
      }
    case t: PeriodResult  => that.ifPeriodResult(t.value.toTotalMonths <= _.toTotalMonths) :: Nil
    case t: AddressResult => false :: Nil
    case t: Invalid       => false :: Nil
    case Dirty            => errorInCalculation("<=", that)
  }

  def >=(that: EvaluationStatus): List[Boolean] = this match {
    case Hidden          => false :: Nil
    case Empty           => false :: Nil
    case t: NumberResult => that.ifNumberResult(t.value >= _) :: Nil
    case t: StringResult => that.ifStringResult(t.value >= _) :: Nil
    case t: OptionResult =>
      (that.ifOptionResult(t.value.min >= _.min) ||
        that.ifNumberResult { numberResult =>
          compareNumberWithOption(numberResult, t, _ >= _)
        }) :: Nil
    case t: DateResult => false :: Nil
    case t: ListResult =>
      that match {
        case t2: ListResult => t.list.zipAll(t2.list, Empty, Empty).flatMap { case (l, r) => l >= r }
        case otherwise      => t.list.flatMap(_ >= otherwise)
      }
    case t: PeriodResult  => that.ifPeriodResult(t.value.toTotalMonths >= _.toTotalMonths) :: Nil
    case t: AddressResult => false :: Nil
    case t: Invalid       => false :: Nil
    case Dirty            => errorInCalculation(">=", that)
  }

  def +(that: EvaluationStatus)(implicit messages: Messages): EvaluationStatus = that match {
    case Hidden | Empty => this
    case r: NumberResult =>
      this match {
        case Hidden | Empty  => r
        case l: NumberResult => l + r
        case l: StringResult => StringResult(l.value + r.value.toString)
        case l: DateResult   => StringResult(l.asString + r.value)
        case l: ListResult   => l.map(_ + r)
        case _               => invalidAdd(this, r)
      }
    case r: StringResult =>
      this match {
        case Hidden | Empty  => r
        case l: NumberResult => StringResult(l.value.toString + r.value)
        case l: StringResult => l + r
        case l: DateResult   => StringResult(l.asString + r.value)
        case l: ListResult   => l.map(_ + r)
        case l: PeriodResult => StringResult(l.asString + r.value)
        case _               => invalidAdd(this, r)
      }
    case r: DateResult =>
      this match {
        case Hidden | Empty  => r
        case l: StringResult => l + StringResult(r.asString)
        case l: DateResult   => StringResult(l.asString + r.asString)
        case l: ListResult   => l.map(_ + r)
        case _               => invalidAdd(this, r)
      }
    case r: ListResult =>
      this match {
        case Hidden | Empty  => r
        case l: NumberResult => r.map(_ + l)
        case l: StringResult => r.map(_ + l)
        case l: ListResult   => l.zipOther(r, _ + _)
        case _               => invalidAdd(this, r)
      }
    case r: PeriodResult =>
      this match {
        case Hidden | Empty  => r
        case l: StringResult => l + StringResult(r.asString)
        case l: PeriodResult => l + r
        case _               => invalidAdd(this, r)
      }
    case _ => invalidAdd(this, that)
  }

  def -(that: EvaluationStatus): EvaluationStatus = that match {
    case Hidden | Empty => this
    case r: NumberResult =>
      this match {
        case Hidden | Empty  => NumberResult(-r.value)
        case l: NumberResult => l - r
        case l: ListResult   => l.map(_ - r)
        case _               => invalidSubstract(this, r)
      }
    case r: ListResult =>
      this match {
        case Hidden | Empty  => r
        case l: NumberResult => r.map(l - _)
        case l: ListResult   => l.zipOther(r, _ - _)
        case _               => invalidSubstract(this, r)
      }
    case r: PeriodResult =>
      this match {
        case Hidden | Empty  => r
        case l: PeriodResult => l - r
        case _               => invalidSubstract(this, r)
      }
    case _ => invalidSubstract(this, that)
  }

  def *(that: EvaluationStatus): EvaluationStatus = that match {
    case Hidden | Empty => NumberResult(0)
    case r: NumberResult =>
      this match {
        case Hidden | Empty  => NumberResult(0)
        case l: NumberResult => l * r
        case l: ListResult   => l.map(_ * r)
        case _               => invalidMult(this, r)
      }
    case r: ListResult =>
      this match {
        case Hidden | Empty  => NumberResult(0)
        case l: NumberResult => r.map(l * _)
        case l: ListResult   => l.zipOther(r, _ * _)
        case _               => invalidMult(this, r)
      }
    case _ => invalidMult(this, that)
  }

  def /(that: EvaluationStatus): EvaluationStatus = that match {
    case Hidden | Empty => NumberResult(0)
    case r: NumberResult =>
      this match {
        case Hidden | Empty  => NumberResult(0)
        case l: NumberResult => l / r
        case l: ListResult   => l.map(_ / r)
        case _               => invalidDiv(this, r)
      }
    case r: ListResult =>
      this match {
        case Hidden | Empty  => NumberResult(0)
        case l: NumberResult => r.map(l / _)
        case l: ListResult   => l.zipOther(r, _ / _)
        case _               => invalidDiv(this, r)
      }
    case _ => invalidDiv(this, that)
  }

  def contains(that: EvaluationStatus, behaviour: Behaviour): List[Boolean] = this match {
    case t: Hidden.type  => false :: Nil
    case t: Empty.type   => false :: Nil
    case t: NumberResult => false :: Nil
    case t: StringResult => t.identical(that)
    case t: OptionResult =>
      that.fold[Boolean](_ => false)(_ => false)(n => t.contains(n.value))(n => t.contains(n.value))(
        _.value.forall(p => t.value.contains(p))
      )(_ => false)(_ => false)(_ => false)(_ => false)(_ => false) :: Nil
    case t: DateResult => false :: Nil
    case t: ListResult =>
      behaviour match {
        case Behaviour.Default => t.list.exists(_.contains(that, behaviour).exists(identity)) :: Nil
        case _                 => t.list.flatMap(_.contains(that, behaviour))
      }

    case t: PeriodResult  => false :: Nil
    case t: AddressResult => false :: Nil
    case t: Invalid       => false :: Nil
    case Dirty            => errorInCalculation("+", that)
  }

  def identical(that: EvaluationStatus): List[Boolean] = this match {
    case t: Hidden.type => that.isEmpty :: Nil
    case t: Empty.type  => that.isEmpty :: Nil
    case t: NumberResult =>
      that match {
        case NumberResult(bd) => (bd === t.value) :: Nil
        case StringResult(value) =>
          (value === t.value.toString ||
            value === t.value.underlying.stripTrailingZeros.toPlainString) :: Nil
        case ListResult(_) => that.identical(t)
        case _             => false :: Nil
      }
    case t: StringResult =>
      that match {
        case StringResult(value) => (value === t.value) :: Nil
        case NumberResult(bd)    => that.identical(t)
        case ListResult(_)       => that.identical(t)
        case OptionResult(_)     => that.identical(t)
        case _                   => (that.isEmpty && this.isEmpty) :: Nil
      }
    case t: OptionResult =>
      (that.ifOptionResult(_.toSet.diff(t.value.toSet).isEmpty) ||
        that.ifStringResult(sr => t.value.length === 1 && t.value.headOption.fold(false)(or => sr === or))) :: Nil
    case t: DateResult => that.ifDateResult(d => d.isEqual(t.value))
    case t: ListResult =>
      that match {
        case ListResult(list) =>
          t.list.zipAll(list, Empty, Empty).flatMap { case (l, r) => l.identical(r) }
        case _ =>
          val addresses: List[AddressResult] = t.list.collect { case a: AddressResult => a }
          if (addresses.isEmpty) {
            t.list.flatMap(_.identical(that))
          } else {
            t.list.forall(_.identical(that).forall(_ == true)) :: Nil
          }
      }
    case t: PeriodResult  => that.ifPeriodResult(t.value.toTotalMonths == _.toTotalMonths) :: Nil
    case t: AddressResult => (that.isEmpty && this.isEmpty) :: Nil
    case t: Invalid       => false :: Nil
    case Dirty            => errorInCalculation("identical", that)
  }

  def before(that: EvaluationStatus): List[Boolean] = this match {
    case t: Hidden.type   => false :: Nil
    case t: Empty.type    => false :: Nil
    case t: NumberResult  => false :: Nil
    case t: StringResult  => false :: Nil
    case t: OptionResult  => false :: Nil
    case t: DateResult    => that.ifDateResult(t.value.isBefore(_))
    case t: ListResult    => t.list.flatMap(_.before(that))
    case t: PeriodResult  => false :: Nil
    case t: AddressResult => false :: Nil
    case t: Invalid       => false :: Nil
    case Dirty            => errorInCalculation("before", that)
  }

  def after(that: EvaluationStatus): List[Boolean] = this match {
    case t: Hidden.type   => false :: Nil
    case t: Empty.type    => false :: Nil
    case t: NumberResult  => false :: Nil
    case t: StringResult  => false :: Nil
    case t: OptionResult  => false :: Nil
    case t: DateResult    => that.ifDateResult(t.value.isAfter(_))
    case t: ListResult    => t.list.flatMap(_.after(that))
    case t: PeriodResult  => false :: Nil
    case t: AddressResult => false :: Nil
    case t: Invalid       => false :: Nil
    case Dirty            => errorInCalculation("after", that)
  }

  def matchRegex(regex: Regex): List[Boolean] = this match {
    case t: Hidden.type   => false :: Nil
    case t: Empty.type    => false :: Nil
    case t: NumberResult  => regex.findFirstIn(t.value.toString).isDefined :: Nil
    case t: StringResult  => regex.findFirstIn(t.value.toString).isDefined :: Nil
    case t: OptionResult  => false :: Nil
    case t: DateResult    => false :: Nil
    case t: ListResult    => t.list.flatMap(_.matchRegex(regex))
    case t: PeriodResult  => false :: Nil
    case t: AddressResult => false :: Nil
    case t: Invalid       => false :: Nil
    case Dirty            => errorInCalculation("matchRegex")
  }

  private def compareNumberWithOption(
    numberResult: BigDecimal,
    optionResult: OptionResult,
    f: (BigDecimal, BigDecimal) => Boolean
  ): Boolean = {
    val bds = optionResult.value.flatMap(toBigDecimalSafe)
    if (bds.isEmpty) {
      false
    } else {
      f(bds.min, numberResult)
    }
  }

  def orElse(er: EvaluationStatus): EvaluationStatus = if (this.isEmpty) er else this

  def withNumberResult(f: BigDecimal => BigDecimal): EvaluationStatus =
    fold[EvaluationStatus](identity)(identity)(r => NumberResult(f(r.value)))(identity)(identity)(identity)(lr =>
      ListResult(lr.list.map(_.withNumberResult(f)))
    )(
      identity
    )(
      identity
    )(
      identity
    )

  def applyTypeInfo(typeInfo: TypeInfo): EvaluationStatus = applyStaticTypeData(typeInfo.staticTypeData)

  def applyStaticTypeData(staticTypeData: StaticTypeData): EvaluationStatus =
    staticTypeData.textConstraint.fold(this)(textConstraint => this.applyTextConstraint(textConstraint))

  private def applyTextConstraint(textConstraint: TextConstraint): EvaluationStatus = textConstraint match {
    // format: off
    case Sterling(rm, _)                 => withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, 2, rm))
    case WholeSterling(_, rm)            => withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, 0, rm))
    case Number(_, maxFD, rm, _)         => withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, maxFD, rm))
    case PositiveNumber(_, maxFD, rm, _) => withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, maxFD, rm))
    case _                               => this
    // format: on
  }

  def stringRepresentation(staticTypeData: StaticTypeData, messages: Messages): String =
    fold(_ => staticTypeData.defaultValue)(_ => staticTypeData.defaultValue)(_.value.toString)(_.value)(
      _.value.mkString(",")
    )(
      _.asString(messages)
    )(
      _.list
        .map(_.stringRepresentation(staticTypeData, messages))
        .map(res =>
          staticTypeData.textConstraint.fold(res)(tc => TextFormatter.componentTextReadonly(res, tc)(LangADT.En))
        )
        .filterNot(_ === "")
        .mkString(", ")
    )(
      _.value.toString
    )(_.lines.mkString(", "))(_ => "")

  def handlebarRepresentation(staticTypeData: StaticTypeData, messages: Messages): String =
    fold(_ => staticTypeData.defaultValue)(_ => staticTypeData.defaultValue)(_.value.toString)(_.value)(
      _.value.mkString(",")
    )(
      _.asString(messages)
    )(
      _.list
        .map(_.handlebarRepresentation(staticTypeData, messages))
        .map(res => staticTypeData.textConstraint.fold(res)(tc => TextFormatter.handlebarText(res, tc)(LangADT.En)))
        .filterNot(_ === "")
        .mkString(", ")
    )(
      _.value.toString
    )(_.lines.mkString(", "))(_ => "")

  def numberRepresentation: Option[BigDecimal] =
    fold[Option[BigDecimal]](_ => None)(_ => None)(bd => Some(bd.value))(_ => None)(_ => None)(_ => None)(_ => None)(
      _ => None
    )(_ => None)(_ => None)

  def optionRepresentation: Option[Seq[String]] =
    fold[Option[Seq[String]]](_ => None)(_ => None)(_ => None)(_ => None)(o => Some(o.value))(_ => None)(_ => None)(_ =>
      None
    )(_ => None)(_ => None)

  def dateRepresentation: Option[LocalDate] =
    fold[Option[LocalDate]](_ => None)(_ => None)(_ => None)(_ => None)(_ => None)(d => Some(d.value))(_ => None)(_ =>
      None
    )(_ => None)(_ => None)

  private def isEmpty: Boolean =
    fold[Boolean](_ => true)(_ => true)(_ => false)(_.value.trim.isEmpty)(_ => false)(_ => false)(
      _.list.forall(_.isEmpty)
    )(_ => false)(_.lines.isEmpty)(_ => false)

  private def ifNumberResult(f: BigDecimal => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(r => f(r.value))(_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(
      _ => false
    )(_ => false)

  private def ifStringResult(f: String => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(r => f(r.value))(_ => false)(_ => false)(_ => false)(_ => false)(
      _ => false
    )(_ => false)

  private def ifOptionResult(f: Seq[String] => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(r => f(r.value))(_ => false)(_ => false)(_ => false)(
      _ => false
    )(_ => false)

  private def ifDateResult(f: LocalDate => Boolean): List[Boolean] = this match {
    case r: DateResult => f(r.value) :: Nil
    case l: ListResult => l.list.flatMap(_.ifDateResult(f))
    case _             => false :: Nil
  }

  private def ifPeriodResult(f: Period => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(r => f(r.value))(
      _ => false
    )(_ => false)

  def stripFileNamePrefix(evaluationContext: EvaluationContext, modelComponentId: ModelComponentId): EvaluationStatus =
    this match {
      case StringResult(value) =>
        val fileIdPrefix: String =
          evaluationContext.componentIdToFileId
            .findSingle(modelComponentId)
            .fold(modelComponentId.toMongoIdentifier)(_.value)

        EvaluationStatus.StringResult(value.replace(fileIdPrefix + "_", ""))

      case otherwise => otherwise
    }

  def mapDateResult(f: LocalDate => EvaluationStatus): EvaluationStatus =
    fold[EvaluationStatus](identity)(identity)(identity)(identity)(identity)(r => f(r.value))(listResult =>
      ListResult(listResult.list.map(_.mapDateResult(f)))
    )(
      identity
    )(
      identity
    )(
      identity
    )

  def fold[B](
    a: Hidden.type => B
  )(
    b: Empty.type => B
  )(
    c: NumberResult => B
  )(
    d: StringResult => B
  )(
    e: OptionResult => B
  )(
    f: DateResult => B
  )(
    g: ListResult => B
  )(
    h: PeriodResult => B
  )(
    i: AddressResult => B
  )(
    j: Invalid => B
  ): B = this match {
    case t: Hidden.type   => a(t)
    case t: Empty.type    => b(t)
    case t: NumberResult  => c(t)
    case t: StringResult  => d(t)
    case t: OptionResult  => e(t)
    case t: DateResult    => f(t)
    case t: ListResult    => g(t)
    case t: PeriodResult  => h(t)
    case t: AddressResult => i(t)
    case t: Invalid       => j(t)
    case t: Dirty.type    => errorInCalculation("fold", t)
  }
}

object EvaluationStatus {
  implicit val equal: Eq[EvaluationStatus] = Eq.fromUniversalEquals

  case object Dirty extends EvaluationStatus
  case object Empty extends EvaluationStatus
  case object Hidden extends EvaluationStatus
  final case class Invalid(explanation: String) extends EvaluationStatus
  final case class StringResult(value: String) extends EvaluationStatus {
    def +(sr: StringResult): StringResult = StringResult(value + sr.value)
  }
  case class NumberResult(value: BigDecimal) extends EvaluationStatus {
    def +(nr: NumberResult): NumberResult = NumberResult(value + nr.value)
    def -(nr: NumberResult): NumberResult = NumberResult(value - nr.value)
    def *(nr: NumberResult): NumberResult = NumberResult(value * nr.value)
    def /(nr: NumberResult): NumberResult = NumberResult(value / nr.value)
  }
  final case class OptionResult(value: Seq[String]) extends EvaluationStatus {
    def contains(v: String): Boolean = value.contains(v)
    def contains(bd: BigDecimal): Boolean = Try(value.map(_.toInt)).fold(_ => false, _.contains(bd.toInt))
  }
  final case class DateResult(value: LocalDate, flag: DateResultFlag) extends EvaluationStatus {
    def asString(implicit messages: Messages) = {
      val monthStr = value.getMonth.getDisplayName(TextStyle.FULL, Locale.UK)
      flag match {
        case DateResultFlag.Date =>
          val day = value.getDayOfMonth
          val month = messages(s"date.$monthStr")
          val year = value.getYear
          s"$day $month $year"
        case DateResultFlag.CalendarDate =>
          val day = value.getDayOfMonth
          val month = messages(s"date.$monthStr")
          s"$day $month"
        case DateResultFlag.TaxPeriodDate =>
          val month = messages(s"date.$monthStr")
          val year = value.getYear
          s"$month $year"
      }
    }
  }
  object DateResult {
    def mkDate(value: LocalDate): DateResult = DateResult(value, DateResultFlag.Date)
    def mkCalendarDate(value: LocalDate): DateResult = DateResult(value, DateResultFlag.CalendarDate)
    def mkTaxPeriodDate(value: LocalDate): DateResult = DateResult(value, DateResultFlag.TaxPeriodDate)
  }

  final case class PeriodResult(value: Period) extends EvaluationStatus {
    def +(pr: PeriodResult): PeriodResult = PeriodResult(value.plus(pr.value).normalized())
    def -(pr: PeriodResult): PeriodResult = PeriodResult(value.minus(pr.value).normalized())
    def asString = value.toString
  }

  final case class AddressResult(address: List[(Atom, String)]) extends EvaluationStatus {
    val lines: List[String] = address.map(_._2).filter(_.trim.nonEmpty)
  }

  final case class ListResult(list: List[EvaluationStatus]) extends EvaluationStatus {
    def map(f: EvaluationStatus => EvaluationStatus): ListResult = ListResult(list.map(f))

    def zipOther(other: ListResult, f: (EvaluationStatus, EvaluationStatus) => EvaluationStatus): ListResult =
      ListResult(list.zipAll(other.list, Empty, Empty).map { case (l, r) => f(l, r) })
  }

  def evalDate(
    day: EvaluationStatus,
    month: EvaluationStatus,
    year: EvaluationStatus,
    modelComponentId: ModelComponentId
  ): EvaluationStatus =
    (year, month, day) match {
      case (NumberResult(y), NumberResult(m), NumberResult(d)) =>
        val maybeDate = Try(LocalDate.of(y.toInt, m.toInt, d.toInt)).toOption
        maybeDate.fold[EvaluationStatus](Empty)(date => DateResult.mkDate(date))
      case (Hidden, Hidden, Hidden) => Hidden
      case _                        => Empty

    }

  def evalCalendarDate(
    day: EvaluationStatus,
    month: EvaluationStatus,
    modelComponentId: ModelComponentId
  ): EvaluationStatus =
    (day, month) match {
      case (NumberResult(d), NumberResult(m)) =>
        val date = LocalDate.of(2020, m.toInt, d.toInt)
        DateResult.mkCalendarDate(date)
      case (Empty, Empty)   => Empty
      case (Hidden, Hidden) => Hidden
      case unknown =>
        throw new Exception(
          s"Error when evaluating CalendarDate component $modelComponentId. Expected two NumberResults, got $unknown"
        )
    }

  def evalTaxPeriodDate(
    month: EvaluationStatus,
    year: EvaluationStatus,
    modelComponentId: ModelComponentId
  ): EvaluationStatus =
    (month, year) match {
      case (NumberResult(m), NumberResult(y)) =>
        val date = LocalDate.of(y.toInt, m.toInt, 1)
        DateResult.mkTaxPeriodDate(date)
      case (Empty, Empty)   => Empty
      case (Hidden, Hidden) => Hidden
      case unknown =>
        throw new Exception(
          s"Error when evaluating TaxPeriodDate component $modelComponentId. Expected two NumberResults, got $unknown"
        )
    }

  def evalPostcodeLookup(
    formComponentId: FormComponentId,
    evaluationStatus: EvaluationStatus,
    evaluationContext: EvaluationContext
  ): EvaluationStatus =
    evaluationStatus match {
      case EvaluationStatus.StringResult(postcode) =>
        val addressAtoms: List[(Atom, String)] = evaluationContext.thirdPartyData.addressLines(formComponentId)

        if (addressAtoms.isEmpty) {
          AddressResult(List(Address.postcode -> postcode))
        } else {
          AddressResult(addressAtoms)
        }
      case otherwise => otherwise

    }

  def evalMultiFile(modelComponentId: ModelComponentId, evaluationContext: EvaluationContext): EvaluationStatus = {
    val value: String = evaluationContext.multiFilesData.get(modelComponentId).fold("") { xs =>
      xs.map { case (fileComponentId, variadicValue) =>
        val fileIdPrefix: String = evaluationContext.componentIdToFileId.fileIdFor(fileComponentId).value
        variadicValue.value.replace(fileIdPrefix + "_", "")
      }.mkString(", ")
    }

    EvaluationStatus.StringResult(value)
  }

}

sealed trait DateResultFlag extends Product with Serializable {
  val isDate: Boolean = this match {
    case DateResultFlag.Date => true
    case _                   => false
  }
  val isCalendarDate: Boolean = this match {
    case DateResultFlag.CalendarDate => true
    case _                           => false
  }
  val isTaxPeriodDate: Boolean = this match {
    case DateResultFlag.TaxPeriodDate => true
    case _                            => false
  }
}

object DateResultFlag {
  case object Date extends DateResultFlag
  case object CalendarDate extends DateResultFlag
  case object TaxPeriodDate extends DateResultFlag
}
