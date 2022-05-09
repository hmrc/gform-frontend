/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.eval

import cats.Eq
import cats.instances.bigDecimal._
import cats.instances.string._
import cats.syntax.eq._
import play.api.i18n.Messages
import scala.util.Try

import scala.util.matching.Regex
import uk.gov.hmrc.gform.commons.NumberSetScale
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Number, PositiveNumber, RoundingMode, Sterling, TextConstraint, WholeSterling }

import java.time.{ LocalDate, Period }
import java.time.format.TextStyle
import java.util.Locale

sealed trait ExpressionResult extends Product with Serializable {

  import ExpressionResult._

  private def invalidAdd(er: ExpressionResult): ExpressionResult =
    Invalid(s"Unsupported operation, cannot add $this and $er")

  private def invalidSubstract(er: ExpressionResult): ExpressionResult =
    Invalid(s"Unsupported operation, cannot substract $this and $er")

  private def invalidMult(er: ExpressionResult): ExpressionResult =
    Invalid(s"Unsupported operation, cannot multiply $this and $er")

  private def invalidDivide(er: ExpressionResult): ExpressionResult =
    Invalid(s"Unsupported operation, cannot divide $this and $er")

  def +(er: ExpressionResult)(implicit messages: Messages): ExpressionResult = er match {
    // format: off
    case t: Invalid     => t // TODO we are loosing 'this' which can potentionally be invalid as well
    case t: Hidden.type => this
    case t: Empty.type  => this
    case t: NumberResult =>
      fold[ExpressionResult](identity)(_ => t)(_ => t)(_ + t)(s => StringResult(s.value + t.value.toString))(invalidAdd)(invalidAdd)(invalidAdd)(invalidAdd)(l => ListResult(l.list.map(_ + t)))
    case t: StringResult =>
      fold[ExpressionResult](identity)(_ => t)(_ => t)(n => StringResult(n.value.toString + t.value))(_ + t)(invalidAdd)(d => StringResult(d.asString + t.value))(invalidAdd)(p => StringResult(p.asString + t.value))(invalidAdd)
    case t: OptionResult  => Invalid(s"Unsupported operation, cannot add OptionResult and $t")
    case t: DateResult    =>
      fold[ExpressionResult](identity)(_ => t)(_ => t)(invalidAdd)(_ + StringResult(t.asString))(invalidAdd)(d => StringResult(d.asString + t.asString))(invalidAdd)(invalidAdd)(invalidAdd)
    case t: PeriodResult  =>
      fold[ExpressionResult](identity)(_ => t)(_ => t)(invalidAdd)(_ + StringResult(t.asString))(invalidAdd)(invalidAdd)(invalidAdd)(_ + t)(invalidAdd)
    case t: AddressResult => Invalid(s"Unsupported operation, cannot add AddressResult and $t")
    case t: ListResult => fold[ExpressionResult](identity)(_ => t)(identity)(e => ListResult(t.list.map(_ + e)))(invalidMult)(invalidMult)(invalidMult)(invalidMult)(invalidMult)(l => ListResult(l.list.zip(t.list).map{case (a,b) => a + b }))
    // format: on
  }

  def -(er: ExpressionResult): ExpressionResult = er match {
    // format: off
    case t: Invalid      => t
    case t: Hidden.type  => this
    case t: Empty.type   => this
    case t: NumberResult => fold[ExpressionResult](identity)(_ => t)(identity)(_ - t)(invalidSubstract)(invalidSubstract)(invalidSubstract)(invalidSubstract)(invalidSubstract)(l => ListResult(l.list.map(_ - t)))
    case t: StringResult => Invalid("Unsupported operation, cannot substract strings")
    case t: OptionResult => Invalid("Unsupported operation, cannot substract options")
    case t: DateResult => Invalid(s"Unsupported operation, cannot substract DateResult$t")
    case t: PeriodResult => Invalid(s"Unsupported operation, cannot substract PeriodResult$t")
    case t: AddressResult => Invalid(s"Unsupported operation, cannot substract AddressResult$t")
    case t: ListResult => fold[ExpressionResult](identity)(_ => t)(identity)(e => ListResult(t.list.map(e - _)))(invalidMult)(invalidMult)(invalidMult)(invalidMult)(invalidMult)(l => ListResult(l.list.zip(t.list).map{case (a,b) => a - b }))
    // format: on
  }

  def *(er: ExpressionResult): ExpressionResult = er match {
    // format: off
    case t: Invalid      => t
    case t: Hidden.type  => fold[ExpressionResult](identity)(identity)(identity)(_ => NumberResult(0))(invalidMult)(invalidMult)(invalidMult)(invalidMult)(invalidMult)(l => ListResult(l.list.map(_ => NumberResult(0))))
    case t: Empty.type   => this
    case t: NumberResult => fold[ExpressionResult](identity)(_ => NumberResult(0))(identity)(_ * t)(invalidMult)(invalidMult)(invalidMult)(invalidMult)(invalidMult)(l => ListResult(l.list.map(_ * t)))
    case t: StringResult => Invalid("Unsupported operation, cannot multiply strings")
    case t: OptionResult => Invalid("Unsupported operation, cannot multiply options")
    case t: DateResult => Invalid("Unsupported operation, cannot multiply DateResult")
    case t: PeriodResult => Invalid("Unsupported operation, cannot multiply PeriodResult")
    case t: AddressResult => Invalid("Unsupported operation, cannot multiply AddressResult")
    case t: ListResult => fold[ExpressionResult](identity)(_ => ListResult(t.list.map(_ => NumberResult(0))))(identity)(e => ListResult(t.list.map(_ * e)))(invalidMult)(invalidMult)(invalidMult)(invalidMult)(invalidMult)(l => ListResult(l.list.zip(t.list).map{case (a,b) => a * b }))
    // format: on
  }

  def /(er: ExpressionResult): ExpressionResult = er match {
    // format: off
    case t: Invalid      => t
    case t: Hidden.type  => fold[ExpressionResult](identity)(identity)(identity)(_ => NumberResult(0))(invalidDivide)(invalidDivide)(invalidDivide)(invalidDivide)(invalidDivide)(l => ListResult(l.list.map(_ => NumberResult(0))))
    case t: Empty.type   => this
    case t: NumberResult => fold[ExpressionResult](identity)(_ => NumberResult(0))(identity)(_ / t)(invalidDivide)(invalidDivide)(invalidDivide)(invalidDivide)(invalidDivide)(l => ListResult(l.list.map( _ / t)))
    case t: StringResult => Invalid("Unsupported operation, cannot divide strings")
    case t: OptionResult => Invalid("Unsupported operation, cannot divide options")
    case t: DateResult => Invalid("Unsupported operation, cannot divide DateResult")
    case t: PeriodResult => Invalid("Unsupported operation, cannot divide PeriodResult")
    case t: AddressResult => Invalid("Unsupported operation, cannot divide AddressResult")
    case t: ListResult => fold[ExpressionResult](identity)(_ => ListResult(t.list.map(_ => NumberResult(0))))(identity)(e => ListResult(t.list.map(e / _)))(invalidMult)(invalidMult)(invalidMult)(invalidMult)(invalidMult)(l => ListResult(l.list.zip(t.list).map{case (a,b) => a / b }))
    // format: on
  }

  def orElse(er: ExpressionResult): ExpressionResult =
    fold[ExpressionResult](identity)(_ => er)(_ => er)(identity)(identity)(identity)(identity)(identity)(identity)(
      identity
    )

  def contains(er: ExpressionResult): Boolean = this match {
    case t: Invalid      => false
    case t: Hidden.type  => false
    case t: Empty.type   => false
    case t: NumberResult => false
    case t: StringResult => false
    case t: OptionResult =>
      er.fold[Boolean](_ => false)(_ => false)(_ => false)(n => t.contains(n.value))(n => t.contains(n.value))(_ =>
        false
      )(_ => false)(_ => false)(_ => false)(_ => false)
    case t: DateResult    => false
    case t: PeriodResult  => false
    case t: AddressResult => false
    case t: ListResult    => t.list.exists(_.contains(er))
  }

  def identical(er: ExpressionResult): Boolean = this match {
    case t: Invalid     => false
    case t: Hidden.type => er === Empty
    case t: Empty.type  => er.isEmpty
    case t: NumberResult =>
      er.ifNumberResult(_ === t.value) ||
        er.ifStringResult(_ === t.value.toString) ||
        er.ifStringResult(_ === t.value.underlying.stripTrailingZeros.toString)
    case t: StringResult =>
      er.ifStringResult(_ === t.value) ||
        er.ifNumberResult(_.toString === t.value) ||
        er.ifNumberResult(_.underlying.stripTrailingZeros.toString === t.value)
    case t: OptionResult  => er.ifOptionResult(_.toSet.diff(t.value.toSet).isEmpty)
    case t: DateResult    => false
    case t: PeriodResult  => er.ifPeriodResult(t.value.toTotalMonths == _.toTotalMonths)
    case t: AddressResult => false
    case t: ListResult    => er.ifListResult(_ === t.list) || t.list.exists(_ identical er)
  }

  def >(er: ExpressionResult): Boolean = this match {
    case t: Invalid       => false
    case t: Hidden.type   => false
    case t: Empty.type    => false
    case t: NumberResult  => er.ifNumberResult(t.value > _)
    case t: StringResult  => er.ifStringResult(t.value > _)
    case t: OptionResult  => er.ifOptionResult(t.value.min > _.min)
    case t: DateResult    => false
    case t: PeriodResult  => er.ifPeriodResult(t.value.toTotalMonths > _.toTotalMonths)
    case t: AddressResult => false
    case t: ListResult    => t.list.exists(_ > er)
  }

  def >=(er: ExpressionResult): Boolean = this match {
    case t: Invalid       => false
    case t: Hidden.type   => false
    case t: Empty.type    => false
    case t: NumberResult  => er.ifNumberResult(t.value >= _)
    case t: StringResult  => er.ifStringResult(t.value >= _)
    case t: OptionResult  => er.ifOptionResult(t.value.min >= _.min)
    case t: DateResult    => false
    case t: PeriodResult  => er.ifPeriodResult(t.value.toTotalMonths >= _.toTotalMonths)
    case t: AddressResult => false
    case t: ListResult    => t.list.exists(_ >= er)
  }

  def <(er: ExpressionResult): Boolean = this match {
    case t: Invalid       => false
    case t: Hidden.type   => false
    case t: Empty.type    => false
    case t: NumberResult  => er.ifNumberResult(t.value < _)
    case t: StringResult  => er.ifStringResult(t.value < _)
    case t: OptionResult  => er.ifOptionResult(t.value.min < _.min)
    case t: DateResult    => false
    case t: PeriodResult  => er.ifPeriodResult(t.value.toTotalMonths < _.toTotalMonths)
    case t: AddressResult => false
    case t: ListResult    => t.list.exists(_ < er)
  }

  def <=(er: ExpressionResult): Boolean = this match {
    case t: Invalid       => false
    case t: Hidden.type   => false
    case t: Empty.type    => false
    case t: NumberResult  => er.ifNumberResult(t.value <= _)
    case t: StringResult  => er.ifStringResult(t.value <= _)
    case t: OptionResult  => er.ifOptionResult(t.value.min <= _.min)
    case t: DateResult    => false
    case t: PeriodResult  => er.ifPeriodResult(t.value.toTotalMonths <= _.toTotalMonths)
    case t: AddressResult => false
    case t: ListResult    => t.list.exists(_ <= er)
  }

  def before(er: ExpressionResult): Boolean = this match {
    case t: Invalid       => false
    case t: Hidden.type   => false
    case t: Empty.type    => false
    case t: NumberResult  => false
    case t: StringResult  => false
    case t: OptionResult  => false
    case t: DateResult    => er.ifDateResult(t.value.isBefore(_))
    case t: PeriodResult  => false
    case t: AddressResult => false
    case t: ListResult    => t.list.exists(_ before er)
  }

  def after(er: ExpressionResult): Boolean = this match {
    case t: Invalid       => false
    case t: Hidden.type   => false
    case t: Empty.type    => false
    case t: NumberResult  => false
    case t: StringResult  => false
    case t: OptionResult  => false
    case t: DateResult    => er.ifDateResult(t.value.isAfter(_))
    case t: PeriodResult  => false
    case t: AddressResult => false
    case t: ListResult    => t.list.exists(_ after er)
  }

  private def isEmpty: Boolean =
    fold[Boolean](_ => false)(_ => true)(_ => true)(_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(_ =>
      false
    )(_ => false)
  private def ifNumberResult(f: BigDecimal => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(r => f(r.value))(_ => false)(_ => false)(_ => false)(_ => false)(
      _ => false
    )(_ => false)
  private def ifStringResult(f: String => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(r => f(r.value))(_ => false)(_ => false)(_ => false)(
      _ => false
    )(_ => false)
  private def ifOptionResult(f: Seq[String] => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(r => f(r.value))(_ => false)(_ => false)(
      _ => false
    )(_ => false)
  private def ifDateResult(f: LocalDate => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(r => f(r.value))(_ => false)(
      _ => false
    )(_ => false)
  private def ifPeriodResult(f: Period => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(r => false)(_ => false)(r =>
      f(r.value)
    )(_ => false)
  private def ifListResult(f: List[ExpressionResult] => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(r => false)(_ => false)(_ =>
      false
    )(r => f(r.list))

  def withNumberResult(f: BigDecimal => BigDecimal): ExpressionResult =
    fold[ExpressionResult](identity)(identity)(identity)(r => NumberResult(f(r.value)))(identity)(identity)(identity)(
      identity
    )(
      identity
    )(identity)

  def withStringResult[B](noString: B)(f: String => B): B =
    fold[B](_ => noString)(_ => noString)(_ => noString)(_ => noString)(r => f(r.value))(_ => noString)(_ => noString)(
      _ => noString
    )(_ => noString)(_ => noString)

  def convertNumberToString: ExpressionResult =
    fold[ExpressionResult](identity)(identity)(identity)(r => StringResult(r.value.toString))(identity)(identity)(
      identity
    )(identity)(identity)(identity)

  private def applyTextConstraint(textConstraint: TextConstraint): ExpressionResult = textConstraint match {
    // format: off
    case Sterling(rm, _)                 => withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, 2, rm))
    case WholeSterling(_)                => withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, 0, RoundingMode.defaultRoundingMode))
    case Number(_, maxFD, rm, _)         => withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, maxFD, rm))
    case PositiveNumber(_, maxFD, rm, _) => withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, maxFD, rm))
    case _                               => this
    // format: on
  }

  def applyTypeInfo(typeInfo: TypeInfo): ExpressionResult =
    typeInfo.staticTypeData.textConstraint.fold(this)(applyTextConstraint)

  def stringRepresentation(typeInfo: TypeInfo, messages: Messages): String =
    fold(_ => "")(_ => typeInfo.defaultValue)(_ => "")(_.value.toString)(_.value)(_.value.mkString(","))(
      _.asString(messages)
    )(
      _.address.mkString(", ")
    )(_.value.toString)(_.list.map(_.stringRepresentation(typeInfo, messages)).filterNot(_ === "").mkString(", "))

  def addressRepresentation(typeInfo: TypeInfo): List[String] =
    fold[List[String]](_ => Nil)(_ => Nil)(_ => Nil)(_ => Nil)(_ => Nil)(_ => Nil)(_ => Nil)(
      _.address
    )(_ => Nil)(_ => Nil)

  def dateRepresentation(typeInfo: TypeInfo): Option[LocalDate] =
    fold[Option[LocalDate]](_ => None)(_ => None)(_ => None)(_ => None)(_ => None)(_ => None)(d => Some(d.value))(_ =>
      None
    )(_ => None)(_ => None)

  def numberRepresentation: Option[BigDecimal] =
    fold[Option[BigDecimal]](_ => None)(_ => None)(_ => None)(bd => Some(bd.value))(_ => None)(_ => None)(_ => None)(
      _ => None
    )(_ => None)(_ => None)

  def optionRepresentation: Option[Seq[String]] =
    fold[Option[Seq[String]]](_ => None)(_ => None)(_ => None)(_ => None)(_ => None)(o => Some(o.value))(_ => None)(_ =>
      None
    )(_ => None)(_ => None)

  def matchRegex(regex: Regex): Boolean =
    fold { _ =>
      false
    } { _ =>
      false
    } { _ =>
      false
    } { number =>
      regex.findFirstIn(number.value.toString).isDefined
    } { string =>
      regex.findFirstIn(string.value).isDefined
    } { _ =>
      false
    } { _ =>
      false
    } { _ =>
      false
    } { _ =>
      false
    } { _ =>
      false
    }

  def fold[B](
    a: Invalid => B
  )(
    b: Hidden.type => B
  )(
    c: Empty.type => B
  )(
    d: NumberResult => B
  )(
    e: StringResult => B
  )(
    f: OptionResult => B
  )(
    g: DateResult => B
  )(
    h: AddressResult => B
  )(
    i: PeriodResult => B
  )(
    j: ListResult => B
  ): B = this match {
    case t: Invalid       => a(t)
    case t: Hidden.type   => b(t)
    case t: Empty.type    => c(t)
    case t: NumberResult  => d(t)
    case t: StringResult  => e(t)
    case t: OptionResult  => f(t)
    case t: DateResult    => g(t)
    case t: AddressResult => h(t)
    case t: PeriodResult  => i(t)
    case t: ListResult    => j(t)
  }
}

object ExpressionResult {
  implicit val equal: Eq[ExpressionResult] = Eq.fromUniversalEquals

  val empty: ExpressionResult = Empty
  def invalid(explanation: String): ExpressionResult = Invalid(explanation)

  case object Empty extends ExpressionResult
  case object Hidden extends ExpressionResult
  case class Invalid(explanation: String) extends ExpressionResult
  case class NumberResult(value: BigDecimal) extends ExpressionResult {
    def +(nr: NumberResult): NumberResult = NumberResult(value + nr.value)
    def -(nr: NumberResult): NumberResult = NumberResult(value - nr.value)
    def *(nr: NumberResult): NumberResult = NumberResult(value * nr.value)
    def /(nr: NumberResult): NumberResult = NumberResult(value / nr.value)
  }
  case class StringResult(value: String) extends ExpressionResult {
    def +(sr: StringResult): StringResult = StringResult(value + sr.value)
  }
  case class DateResult(value: LocalDate) extends ExpressionResult {
    def asString(implicit messages: Messages) = {
      val day = value.getDayOfMonth
      val month = messages(s"date.${value.getMonth.getDisplayName(TextStyle.FULL, Locale.UK)}")
      val year = value.getYear
      s"$day $month $year"
    }
  }
  case class PeriodResult(value: Period) extends ExpressionResult {
    def +(pr: PeriodResult): PeriodResult = PeriodResult(pr.value.plus(value).normalized())
    def asString = value.toString
  }
  case class OptionResult(value: Seq[String]) extends ExpressionResult {
    def contains(v: String): Boolean = value.contains(v)
    def contains(bd: BigDecimal): Boolean = Try(value.map(_.toInt)).fold(_ => false, _.contains(bd.toInt))
  }
  case class ListResult(list: List[ExpressionResult]) extends ExpressionResult
  case class AddressResult(address: List[String]) extends ExpressionResult
}
