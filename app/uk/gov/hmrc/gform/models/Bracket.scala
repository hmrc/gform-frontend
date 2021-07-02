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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import cats.syntax.eq._
import uk.gov.hmrc.gform.sharedmodel.form.VisitIndex
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, Section, SectionNumber }

sealed trait Bracket[A <: PageMode] extends Product with Serializable {
  def toPlainBracket: BracketPlain[A]
  def hasSectionNumber(sectionNumber: SectionNumber): Boolean
  def map[B <: PageMode](f: Singleton[A] => Singleton[B], g: Repeater[A] => Repeater[B]): Bracket[B]

  def fold[B](
    f: Bracket.NonRepeatingPage[A] => B
  )(
    g: Bracket.RepeatingPage[A] => B
  )(
    h: Bracket.AddToList[A] => B
  ): B = this match {
    case b: Bracket.NonRepeatingPage[A] => f(b)
    case b: Bracket.RepeatingPage[A]    => g(b)
    case b: Bracket.AddToList[A]        => h(b)
  }

  def isToListById(addToListId: AddToListId): Boolean = this match {
    case Bracket.AddToList(_, source) => source.id === addToListId
    case _                            => false
  }

  def toPageModelWithNumber: NonEmptyList[(PageModel[A], SectionNumber)] =
    fold[NonEmptyList[(PageModel[A], SectionNumber)]](a => NonEmptyList.one((a.singleton, a.sectionNumber)))(
      _.singletons.map(_.toPageModelWithNumber)
    )(_.iterations.flatMap(_.toPageModelWithNumber))

  def toPageModel: NonEmptyList[PageModel[A]] =
    fold[NonEmptyList[PageModel[A]]](a => NonEmptyList.one(a.singleton))(_.singletons.map(_.singleton))(
      _.iterations.flatMap(_.toPageModel)
    )

  def filter(predicate: PageModel[A] => Boolean): Option[Bracket[A]] = this match {
    case Bracket.AddToList(iterations, source) =>
      val filtered: Option[NonEmptyList[Bracket.AddToListIteration[A]]] = iterations.traverse(_.filter(predicate))
      filtered.map(Bracket.AddToList(_, source))
    case Bracket.RepeatingPage(singletons, source) =>
      val filtered = singletons.filter(singletonWithNumber => predicate(singletonWithNumber.singleton))
      NonEmptyList.fromList(filtered).map(Bracket.RepeatingPage(_, source))
    case b @ Bracket.NonRepeatingPage(singleton, _, _) => if (predicate(singleton)) Some(b) else None
  }

  def withAddToListBracket[B](f: Bracket.AddToList[A] => B): B =
    fold { nonRepeating =>
      throw new IllegalArgumentException(s"Expected AddToList bracket, got: $nonRepeating")
    } { repeating =>
      throw new IllegalArgumentException(s"Expected AddToList bracket, got: $repeating")
    } {
      f
    }
}

object Bracket {
  case class AddToListIteration[A <: PageMode](
    singletons: NonEmptyList[SingletonWithNumber[A]], // There must be at least one page in Add-to-list iteration
    repeater: RepeaterWithNumber[A]
  ) {

    def toPageModelWithNumber: NonEmptyList[(PageModel[A], SectionNumber)] =
      singletons.map(_.toPageModelWithNumber) ::: NonEmptyList.one(repeater.repeater -> repeater.sectionNumber)

    def toPlainBracket: BracketPlain.AddToListIteration[A] =
      BracketPlain.AddToListIteration(singletons.map(_.singleton), repeater.repeater)
    def hasSectionNumber(sectionNumber: SectionNumber): Boolean =
      repeater.sectionNumber === sectionNumber || singletons.exists(_.sectionNumber === sectionNumber)
    def singleton(sectionNumber: SectionNumber): Singleton[A] =
      singletons.toList
        .collectFirst {
          case singletonWithNumber if singletonWithNumber.sectionNumber === sectionNumber =>
            singletonWithNumber.singleton
        }
        .getOrElse(throw new IllegalArgumentException(s"Invalid sectionNumber: $sectionNumber for AddToListIteration"))

    def toPageModel: NonEmptyList[PageModel[A]] = singletons.map(_.singleton) ::: NonEmptyList.one(repeater.repeater)

    def map[B <: PageMode](f: Singleton[A] => Singleton[B], g: Repeater[A] => Repeater[B]): AddToListIteration[B] =
      AddToListIteration(singletons.map(_.map(f)), repeater.map(g))

    def filter(predicate: PageModel[A] => Boolean): Option[AddToListIteration[A]] = {
      val filtered = singletons.filter(s => predicate(s.singleton))
      NonEmptyList.fromList(filtered).map(AddToListIteration(_, repeater))
    }

    def firstSectionNumber: SectionNumber = singletons.head.sectionNumber

    def secondSectionNumber: SectionNumber = singletons.tail.head.sectionNumber

    def isCommited(visitsIndex: VisitIndex): Boolean =
      singletons.forall { singletonWithNumber =>
        visitsIndex.contains(singletonWithNumber.sectionNumber.value)
      }
  }

  case class NonRepeatingPage[A <: PageMode](
    singleton: Singleton[A],
    sectionNumber: SectionNumber,
    source: Section.NonRepeatingPage
  ) extends Bracket[A] {
    def map[B <: PageMode](f: Singleton[A] => Singleton[B], g: Repeater[A] => Repeater[B]): NonRepeatingPage[B] =
      NonRepeatingPage(
        f(singleton),
        sectionNumber,
        source
      )

    def toPlainBracket: BracketPlain[A] = BracketPlain.NonRepeatingPage(singleton, source)

    def hasSectionNumber(sectionNumber: SectionNumber): Boolean = this.sectionNumber === sectionNumber
  }

  case class RepeatingPage[A <: PageMode](
    singletons: NonEmptyList[SingletonWithNumber[A]], // There must be at at least one page in repeating page bracket
    source: Section.RepeatingPage
  ) extends Bracket[A] {
    def map[B <: PageMode](f: Singleton[A] => Singleton[B], g: Repeater[A] => Repeater[B]): RepeatingPage[B] =
      RepeatingPage(
        singletons.map(_.map(f)),
        source
      )

    def toPlainBracket: BracketPlain[A] = BracketPlain.RepeatingPage(singletons.map(_.singleton), source)

    def hasSectionNumber(sectionNumber: SectionNumber): Boolean = singletons.exists(_.sectionNumber === sectionNumber)

    def singletonForSectionNumber(sectionNumber: SectionNumber): Singleton[A] =
      singletons.toList
        .collectFirst {
          case singletonWithNumber if singletonWithNumber.sectionNumber === sectionNumber =>
            singletonWithNumber.singleton
        }
        .getOrElse(
          throw new IllegalArgumentException(s"Invalid sectionNumber: $sectionNumber for Bracket.RepeatingPage")
        )

  }

  case class AddToList[A <: PageMode](
    iterations: NonEmptyList[
      AddToListIteration[A]
    ], // There must be at least one iteration for Add-to-list to make sense
    source: Section.AddToList
  ) extends Bracket[A] {

    def map[B <: PageMode](f: Singleton[A] => Singleton[B], g: Repeater[A] => Repeater[B]): AddToList[B] = AddToList(
      iterations.map(_.map(f, g)),
      source
    )

    def toPlainBracket: BracketPlain[A] = BracketPlain.AddToList(iterations.map(_.toPlainBracket), source)

    def hasSectionNumber(sectionNumber: SectionNumber): Boolean = iterations.exists { iteration =>
      iteration.repeater.sectionNumber === sectionNumber || iteration.singletons.exists(
        _.sectionNumber === sectionNumber
      )
    }

    def iterationForSectionNumber(sectionNumber: SectionNumber): Bracket.AddToListIteration[A] =
      iterations.toList
        .collectFirst {
          case iteration if iteration.hasSectionNumber(sectionNumber) => iteration
        }
        .getOrElse(
          throw new IllegalArgumentException(s"Invalid sectionNumber: $sectionNumber for Bracket.AddToListIteration")
        )

    def repeaters: NonEmptyList[Repeater[A]] = iterations.map(_.repeater.repeater)

    def lastSectionNumber: SectionNumber = iterations.last.repeater.sectionNumber
  }

  def fromBrackets[A <: PageMode](brackets: NonEmptyList[BracketPlain[A]]): NonEmptyList[Bracket[A]] = {
    val iterator = Stream.from(0).map(SectionNumber(_)).iterator
    brackets.map {
      case BracketPlain.AddToList(iterations, source) =>
        Bracket.AddToList(
          iterations.map { it =>
            Bracket
              .AddToListIteration(
                it.singletons.map(singleton => SingletonWithNumber(singleton, iterator.next)),
                RepeaterWithNumber(it.repeater, iterator.next)
              )
          },
          source
        )
      case BracketPlain.RepeatingPage(singletons, source) =>
        Bracket.RepeatingPage(singletons.map(singleton => SingletonWithNumber(singleton, iterator.next)), source)
      case BracketPlain.NonRepeatingPage(singleton, source) =>
        Bracket.NonRepeatingPage(singleton, iterator.next, source)
    }
  }
}
