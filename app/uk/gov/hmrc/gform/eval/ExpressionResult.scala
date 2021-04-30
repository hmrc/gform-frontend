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

package uk.gov.hmrc.gform.eval

import cats.Eq
import cats.instances.bigDecimal._
import cats.instances.string._
import cats.syntax.eq._
import scala.util.matching.Regex
import uk.gov.hmrc.gform.commons.NumberSetScale
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Number, PositiveNumber, Sterling, TextConstraint }

import java.time.LocalDate
import java.time.format.DateTimeFormatter

sealed trait ExpressionResult extends Product with Serializable {

  import ExpressionResult._

  private def invalidAdd(er: ExpressionResult): ExpressionResult =
    Invalid(s"Unsupported operation, cannot add $this and $er")

  private def invalidSubstract(er: ExpressionResult): ExpressionResult =
    Invalid(s"Unsupported operation, cannot substract $this and $er")

  private def invalidMult(er: ExpressionResult): ExpressionResult =
    Invalid(s"Unsupported operation, cannot multiply $this and $er")

  def +(er: ExpressionResult): ExpressionResult = er match {
    case t: Invalid     => t // TODO we are loosing 'this' which can potentionally be invalid as well
    case t: Hidden.type => this
    case t: Empty.type  => this
    case t: NumberResult =>
      fold[ExpressionResult](identity)(_ => t)(_ => t)(_ + t)(s => StringResult(s.value + t.value.toString))(
        invalidAdd
      )(invalidAdd)
    case t: StringResult =>
      fold[ExpressionResult](identity)(_ => t)(_ => t)(n => StringResult(n.value.toString + t.value))(_ + t)(
        invalidAdd
      )(invalidAdd)
    case t: OptionResult => Invalid(s"Unsupported operation, cannot add options a OptionResult $t")
    case t: DateResult   => Invalid(s"Unsupported operation, cannot add options a OptionResult $t")
  }

  def -(er: ExpressionResult): ExpressionResult = er match {
    // format: off
    case t: Invalid      => t
    case t: Hidden.type  => this
    case t: Empty.type   => this
    case t: NumberResult => fold[ExpressionResult](identity)(_ => t)(identity)(_ - t)(invalidSubstract)(invalidSubstract)(invalidSubstract)
    case t: StringResult => Invalid("Unsupported operation, cannot substract strings")
    case t: OptionResult => Invalid("Unsupported operation, cannot substract options")
    case t: DateResult => Invalid(s"Unsupported operation, cannot substract DateResult$t")
    // format: on
  }

  def *(er: ExpressionResult): ExpressionResult = er match {
    // format: off
    case t: Invalid      => t
    case t: Hidden.type  => fold[ExpressionResult](identity)(identity)(identity)(_ => NumberResult(0))(invalidMult)(invalidMult)(invalidMult)
    case t: Empty.type   => this
    case t: NumberResult => fold[ExpressionResult](identity)(_ => NumberResult(0))(identity)(_ * t)(invalidMult)(invalidMult)(invalidMult)
    case t: StringResult => Invalid("Unsupported operation, cannot multiply strings")
    case t: OptionResult => Invalid("Unsupported operation, cannot multiply options")
    case t: DateResult => Invalid("Unsupported operation, cannot multiply DateResult")
    // format: on
  }

  def orElse(er: ExpressionResult): ExpressionResult =
    fold[ExpressionResult](identity)(_ => er)(_ => er)(identity)(identity)(identity)(identity)

  def contains(er: ExpressionResult): Boolean = this match {
    case t: Invalid      => false
    case t: Hidden.type  => false
    case t: Empty.type   => false
    case t: NumberResult => false
    case t: StringResult => false
    case t: OptionResult =>
      er.fold[Boolean](_ => false)(_ => false)(_ => false)(n => t.contains(n.value))(_ => false)(_ => false)(_ => false)
    case t: DateResult => false
  }

  def identical(er: ExpressionResult): Boolean = (this, er) match {
    case (t: Invalid, _) =>
      false
    case (t: Hidden.type, _) =>
      false
    case (t: Empty.type, _) =>
      er.isEmpty
    case (t: NumberResult, _) =>
      er.ifNumberResult(_ === t.value) ||
        er.ifStringResult(_ === t.value.toString)
    case (t: StringResult, _) =>
      er.ifStringResult(_ === t.value) ||
        er.ifNumberResult(_.toString === t.value)
    case (t: OptionResult, _: NumberResult) =>
      er.ifNumberResult(v => t.value.headOption.fold(false)(v === BigDecimal(_)))
    case (t: OptionResult, _) =>
      er.ifOptionResult(_.toSet.diff(t.value.toSet).isEmpty)
    case (t: DateResult, _) =>
      false
  }

  def >(er: ExpressionResult): Boolean = this match {
    case t: Invalid      => false
    case t: Hidden.type  => false
    case t: Empty.type   => false
    case t: NumberResult => er.ifNumberResult(t.value > _)
    case t: StringResult => er.ifStringResult(t.value > _)
    case t: OptionResult => er.ifOptionResult(t.value.min > _.min)
    case t: DateResult   => false
  }

  def >=(er: ExpressionResult): Boolean = this match {
    case t: Invalid      => false
    case t: Hidden.type  => false
    case t: Empty.type   => false
    case t: NumberResult => er.ifNumberResult(t.value >= _)
    case t: StringResult => er.ifStringResult(t.value >= _)
    case t: OptionResult => er.ifOptionResult(t.value.min >= _.min)
    case t: DateResult   => false
  }

  def <(er: ExpressionResult): Boolean = this match {
    case t: Invalid      => false
    case t: Hidden.type  => false
    case t: Empty.type   => false
    case t: NumberResult => er.ifNumberResult(t.value < _)
    case t: StringResult => er.ifStringResult(t.value < _)
    case t: OptionResult => er.ifOptionResult(t.value.min < _.min)
    case t: DateResult   => false
  }

  def <=(er: ExpressionResult): Boolean = this match {
    case t: Invalid      => false
    case t: Hidden.type  => false
    case t: Empty.type   => false
    case t: NumberResult => er.ifNumberResult(t.value <= _)
    case t: StringResult => er.ifStringResult(t.value <= _)
    case t: OptionResult => er.ifOptionResult(t.value.min <= _.min)
    case t: DateResult   => false
  }

  def before(er: ExpressionResult): Boolean = this match {
    case t: Invalid      => false
    case t: Hidden.type  => false
    case t: Empty.type   => false
    case t: NumberResult => false
    case t: StringResult => false
    case t: OptionResult => false
    case t: DateResult   => er.ifDateResult(t.value.isBefore(_))
  }

  def after(er: ExpressionResult): Boolean = this match {
    case t: Invalid      => false
    case t: Hidden.type  => false
    case t: Empty.type   => false
    case t: NumberResult => false
    case t: StringResult => false
    case t: OptionResult => false
    case t: DateResult   => er.ifDateResult(t.value.isAfter(_))
  }

  private def isEmpty: Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => true)(_ => false)(_ => false)(_ => false)(_ => false)
  private def ifNumberResult(f: BigDecimal => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(r => f(r.value))(_ => false)(_ => false)(_ => false)
  private def ifStringResult(f: String => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(r => f(r.value))(_ => false)(_ => false)
  private def ifOptionResult(f: Seq[Int] => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(r => f(r.value))(_ => false)
  private def ifDateResult(f: LocalDate => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(r => f(r.value))

  def withNumberResult(f: BigDecimal => BigDecimal): ExpressionResult =
    fold[ExpressionResult](identity)(identity)(identity)(r => NumberResult(f(r.value)))(identity)(identity)(identity)

  def withStringResult[B](noString: B)(f: String => B): B =
    fold[B](_ => noString)(_ => noString)(_ => noString)(_ => noString)(r => f(r.value))(_ => noString)(_ => noString)

  def convertNumberToString: ExpressionResult =
    fold[ExpressionResult](identity)(identity)(identity)(r => StringResult(r.value.toString))(identity)(identity)(
      identity
    )

  def applyTextConstraint(textConstraint: TextConstraint): ExpressionResult = textConstraint match {
    // format: off
    case Sterling(rm, _)                 => withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, 2, rm))
    case Number(_, maxFD, rm, _)         => withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, maxFD, rm))
    case PositiveNumber(_, maxFD, rm, _) => withNumberResult(bigDecimal => NumberSetScale.setScale(bigDecimal, maxFD, rm))
    case _                               => this
    // format: on
  }

  def applyTypeInfo(typeInfo: TypeInfo): ExpressionResult =
    typeInfo.staticTypeData.textConstraint.fold(this)(applyTextConstraint)

  def stringRepresentation(typeInfo: TypeInfo) =
    fold(_ => "")(_ => typeInfo.defaultValue)(_ => "")(_.value.toString)(_.value)(_.value.mkString(","))(_.asString)

  def numberRepresentation: Option[BigDecimal] =
    fold[Option[BigDecimal]](_ => None)(_ => None)(_ => None)(bd => Some(bd.value))(_ => None)(_ => None)(_ => None)

  def optionRepresentation: Option[Seq[Int]] =
    fold[Option[Seq[Int]]](_ => None)(_ => None)(_ => None)(_ => None)(_ => None)(o => Some(o.value))(_ => None)

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
  ): B = this match {
    case t: Invalid      => a(t)
    case t: Hidden.type  => b(t)
    case t: Empty.type   => c(t)
    case t: NumberResult => d(t)
    case t: StringResult => e(t)
    case t: OptionResult => f(t)
    case t: DateResult   => g(t)
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
  }
  case class StringResult(value: String) extends ExpressionResult {
    def +(sr: StringResult): StringResult = StringResult(value + sr.value)
  }
  case class DateResult(value: LocalDate) extends ExpressionResult {
    val DATE_DISPLAY_FORMAT: DateTimeFormatter = DateTimeFormatter.ofPattern("dd MMMM yyyy")
    def asString = value.format(DATE_DISPLAY_FORMAT)
  }
  case class OptionResult(value: Seq[Int]) extends ExpressionResult {
    def contains(bd: BigDecimal): Boolean = value.contains(bd.toInt)
  }
}
