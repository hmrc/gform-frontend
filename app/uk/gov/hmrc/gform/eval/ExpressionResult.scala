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

package uk.gov.hmrc.gform.eval

import cats.Eq
import cats.instances.bigDecimal._
import cats.instances.int._
import cats.instances.string._
import cats.syntax.eq._

sealed trait ExpressionResult extends Product with Serializable {

  import ExpressionResult._

  private def invalidMult(er: ExpressionResult): ExpressionResult =
    Invalid(s"Unsupported operation, cannot multiply $this and $er")

  def +(er: ExpressionResult): ExpressionResult = er match {
    case t: Invalid     => t // TODO we are loosing 'this' which can potentionally be invalid as well
    case t: Hidden.type => this
    case t: Empty.type  => this
    case t: NumberResult =>
      fold[ExpressionResult](identity)(_ => t)(_ => t)(_ + t)(_ + t)(_ => ???)(_ =>
        Invalid(s"Unsupported operation, cannot add options to a NumberResult $t"))
    case t: IntResult =>
      fold[ExpressionResult](identity)(_ => t)(_ => t)(_ => ???)(_ + t)(_ + t)(_ =>
        Invalid(s"Unsupported operation, cannot add options to a IntResult $t"))
    case t: StringResult =>
      fold[ExpressionResult](identity)(_ => t)(_ => t)(_ + t)(_ + t)(_ + t)(_ =>
        Invalid(s"Unsupported operation, cannot add options to a StringResult $t"))
    case t: OptionResult => Invalid(s"Unsupported operation, cannot add options a OptionResult $t")
  }

  def -(er: ExpressionResult): ExpressionResult = er match {
    case t: Invalid      => t
    case t: Hidden.type  => this
    case t: Empty.type   => this
    case t: NumberResult => fold[ExpressionResult](identity)(_ => t)(identity)(_ - t)(_ - t)(_ => ???)(_ => ???)
    case t: IntResult    => fold[ExpressionResult](identity)(_ => t)(identity)(_ => ???)(_ - t)(_ => ???)(_ => ???)
    case t: StringResult => Invalid("Unsupported operation, cannot substract strings")
    case t: OptionResult => Invalid("Unsupported operation, cannot substract options")
  }

  def *(er: ExpressionResult): ExpressionResult = er match {
    // format: off
    case t: Invalid      => t
    case t: Hidden.type  => fold[ExpressionResult](identity)(identity)(identity)(_ => NumberResult(0))(_ => IntResult(0))(invalidMult)(invalidMult)
    case t: Empty.type   => this
    case t: NumberResult => fold[ExpressionResult](identity)(_ => NumberResult(0))(identity)(_ * t)(_ * t)(invalidMult)(invalidMult)
    case t: IntResult    => fold[ExpressionResult](identity)(_ => IntResult(0))(identity)(n => NumberResult(n.value * t.value))(_ * t)(invalidMult)(invalidMult)
    case t: StringResult => Invalid("Unsupported operation, cannot multiply strings")
    case t: OptionResult => Invalid("Unsupported operation, cannot multiply options")
    // format: on
  }

  def orElse(er: ExpressionResult): ExpressionResult =
    fold[ExpressionResult](identity)(_ => er)(_ => er)(identity)(identity)(identity)(_ => ???)

  def contains(er: ExpressionResult): Boolean = this match {
    case t: Invalid      => false
    case t: Hidden.type  => false
    case t: Empty.type   => false
    case t: NumberResult => false
    case t: IntResult    => false
    case t: StringResult => false
    case t: OptionResult =>
      er.fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(n => t.value.contains(n.value))(_ => false)(_ =>
        false)
  }

  def identical(er: ExpressionResult): Boolean = this match {
    case t: Invalid     => false
    case t: Hidden.type => false
    case t: Empty.type  => false
    case t: NumberResult =>
      er.ifNumberResult(_ === t.value) ||
        er.ifStringResult(_ === t.value.toString)
    case t: IntResult =>
      er.ifIntResult(_ === t.value) ||
        er.ifStringResult(_ === t.value.toString)
    case t: StringResult =>
      er.ifStringResult(_ === t.value) ||
        er.ifIntResult(_.toString === t.value) ||
        er.ifNumberResult(_.toString === t.value)
    case t: OptionResult => er.ifOptionResult(_.toSet.diff(t.value.toSet).isEmpty)
  }

  def >(er: ExpressionResult): Boolean = this match {
    case t: Invalid      => false
    case t: Hidden.type  => false
    case t: Empty.type   => false
    case t: NumberResult => er.ifNumberResult(t.value > _)
    case t: IntResult    => er.ifIntResult(t.value > _)
    case t: StringResult => er.ifStringResult(t.value > _)
    case t: OptionResult => er.ifOptionResult(t.value.min > _.min)
  }

  def >=(er: ExpressionResult): Boolean = this match {
    case t: Invalid      => false
    case t: Hidden.type  => false
    case t: Empty.type   => false
    case t: NumberResult => er.ifNumberResult(t.value >= _)
    case t: IntResult    => er.ifIntResult(t.value >= _)
    case t: StringResult => er.ifStringResult(t.value >= _)
    case t: OptionResult => er.ifOptionResult(t.value.min >= _.min)
  }

  def <(er: ExpressionResult): Boolean = this match {
    case t: Invalid      => false
    case t: Hidden.type  => false
    case t: Empty.type   => false
    case t: NumberResult => er.ifNumberResult(t.value < _)
    case t: IntResult    => er.ifIntResult(t.value < _)
    case t: StringResult => er.ifStringResult(t.value < _)
    case t: OptionResult => er.ifOptionResult(t.value.min < _.min)
  }

  def <=(er: ExpressionResult): Boolean = this match {
    case t: Invalid      => false
    case t: Hidden.type  => false
    case t: Empty.type   => false
    case t: NumberResult => er.ifNumberResult(t.value <= _)
    case t: IntResult    => er.ifIntResult(t.value <= _)
    case t: StringResult => er.ifStringResult(t.value <= _)
    case t: OptionResult => er.ifOptionResult(t.value.min <= _.min)
  }

  private def ifNumberResult(f: BigDecimal => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(r => f(r.value))(r => f(r.value))(_ => false)(_ => false)
  private def ifIntResult(f: Int => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(r => f(r.value))(_ => false)(_ => false)
  private def ifStringResult(f: String => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(r => f(r.value))(_ => false)
  private def ifOptionResult(f: Seq[Int] => Boolean): Boolean =
    fold[Boolean](_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(_ => false)(r => f(r.value))

  def withStringResult[B](noString: B)(f: String => B): B =
    fold[B](_ => noString)(_ => noString)(_ => noString)(_ => noString)(_ => noString)(r => f(r.value))(_ => noString)

  def fold[B](
    a: Invalid => B
  )(
    b: Hidden.type => B
  )(
    c: Empty.type => B
  )(
    d: NumberResult => B
  )(
    e: IntResult => B
  )(
    f: StringResult => B
  )(
    g: OptionResult => B
  ): B = this match {
    case t: Invalid      => a(t)
    case t: Hidden.type  => b(t)
    case t: Empty.type   => c(t)
    case t: NumberResult => d(t)
    case t: IntResult    => e(t)
    case t: StringResult => f(t)
    case t: OptionResult => g(t)
  }

  def stringRepresentation =
    fold(_ => "")(_ => "")(_ => "")(_.value.toString)(_.value.toString)(_.value)(_.value.mkString(","))
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
  case class IntResult(value: Int) extends ExpressionResult {
    def +(sr: IntResult): IntResult = IntResult(value + sr.value)
    def -(sr: IntResult): IntResult = IntResult(value - sr.value)
    def *(sr: IntResult): IntResult = IntResult(value * sr.value)
  }
  case class StringResult(value: String) extends ExpressionResult {
    def +(sr: StringResult): StringResult = StringResult(value + sr.value)
  }
  case class OptionResult(value: Seq[Int]) extends ExpressionResult
}
