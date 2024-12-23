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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import cats.syntax.eq._
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.form.VisitIndex
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, Expr, Section, SectionNumber }
import uk.gov.hmrc.gform.eval.AllPageModelExpressionsGetter

sealed trait Bracket[A <: PageMode] extends Product with Serializable {

  def allExprs(formModel: FormModel[DataExpanded]): List[Expr] =
    AllPageModelExpressionsGetter.allExprs(formModel)(this)

  def hasSectionNumber(sectionNumber: SectionNumber): Boolean

  def map[B <: PageMode](
    e: Singleton[A] => Singleton[B],
    f: CheckYourAnswers[A] => CheckYourAnswers[B],
    g: Repeater[A] => Repeater[B]
  ): Bracket[B]

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
    fold[NonEmptyList[(PageModel[A], SectionNumber)]](a =>
      NonEmptyList.one((a.singleton.singleton, a.singleton.sectionNumber))
    )(
      _.singletons.map(_.toPageModelWithNumber)
    )(
      _.iterations.flatMap(_.toPageModelWithNumber)
    )

  def toPageModel: NonEmptyList[PageModel[A]] =
    fold[NonEmptyList[PageModel[A]]](a => NonEmptyList.one(a.singleton.singleton))(_.singletons.map(_.singleton))(
      _.iterations.flatMap(_.toPageModel)
    )

  def filter(predicate: PageModel[A] => Boolean): Option[Bracket[A]] = this match {
    case Bracket.AddToList(iterations, source) =>
      val filtered: Option[NonEmptyList[Bracket.AddToListIteration[A]]] = iterations.traverse(_.filter(predicate))
      filtered.map(Bracket.AddToList(_, source))
    case Bracket.RepeatingPage(singletons, source) =>
      val filtered = singletons.filter(singletonWithNumber => predicate(singletonWithNumber.singleton))
      NonEmptyList.fromList(filtered).map(Bracket.RepeatingPage(_, source))
    case b @ Bracket.NonRepeatingPage(singleton, _) => if (predicate(singleton.singleton)) Some(b) else None
  }

  def withAddToListBracket[B](f: Bracket.AddToList[A] => B): B =
    fold { nonRepeating =>
      throw new IllegalArgumentException(s"Expected AddToList bracket, got: $nonRepeating")
    } { repeating =>
      throw new IllegalArgumentException(s"Expected AddToList bracket, got: $repeating")
    } {
      f
    }

  def whenAddToListBracket(f: Bracket.AddToList[A] => Boolean): Boolean =
    fold(_ => false)(_ => false)(f)

  def atlIterationToRemove(
    sectionNumber: SectionNumber,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser]
  ): Option[(AddToListId, Int)] = fold[Option[(AddToListId, Int)]](nonRepeating => None)(repeating => None) {
    addToList =>
      addToList.indexToRemove(sectionNumber, formModelVisibilityOptics).map(addToList.source.id -> _)
  }
}

object Bracket {
  case class AddToListIteration[A <: PageMode](
    defaultPage: Option[SingletonWithNumber[A]], // Only first iteration can have default page
    singletons: NonEmptyList[SingletonWithNumber[A]], // There must be at least one page in Add-to-list iteration
    checkYourAnswers: Option[CheckYourAnswersWithNumber[A]],
    repeater: RepeaterWithNumber[A]
  ) {

    def allSingletonSectionNumbers: List[SectionNumber] = singletons.map(_.sectionNumber).toList

    def toPageModelWithNumber: NonEmptyList[(PageModel[A], SectionNumber)] = {
      val pageModels = singletons.map(_.toPageModelWithNumber) ++ checkYourAnswers.toList.map(c =>
        c.checkYourAnswers -> c.sectionNumber
      ) ::: NonEmptyList.one(repeater.repeater -> repeater.sectionNumber)

      defaultPage.map(_.toPageModelWithNumber).fold(pageModels)(_ :: pageModels)
    }

    def hasSectionNumber(sectionNumber: SectionNumber): Boolean =
      defaultPage.exists(_.sectionNumber === sectionNumber) ||
        repeater.sectionNumber === sectionNumber ||
        checkYourAnswers.exists(_.sectionNumber === sectionNumber) ||
        singletons.exists(_.sectionNumber === sectionNumber)

    def singleton(sectionNumber: SectionNumber): Singleton[A] =
      (defaultPage.toList ++ singletons.toList)
        .collectFirst {
          case singletonWithNumber if singletonWithNumber.sectionNumber === sectionNumber =>
            singletonWithNumber.singleton
        }
        .getOrElse(throw new IllegalArgumentException(s"Invalid sectionNumber: $sectionNumber for AddToListIteration"))

    def toPageModel: NonEmptyList[PageModel[A]] = singletons.map(_.singleton) ::: NonEmptyList.one(repeater.repeater)

    def map[B <: PageMode](
      e: Singleton[A] => Singleton[B],
      f: CheckYourAnswers[A] => CheckYourAnswers[B],
      g: Repeater[A] => Repeater[B]
    ): AddToListIteration[B] =
      AddToListIteration(
        defaultPage.map(_.map(e)),
        singletons.map(_.map(e)),
        checkYourAnswers.map(_.map(f)),
        repeater.map(g)
      )

    def filter(predicate: PageModel[A] => Boolean): Option[AddToListIteration[A]] = {
      val filtered = singletons.filter(s => predicate(s.singleton))
      NonEmptyList.fromList(filtered).map(AddToListIteration(defaultPage, _, checkYourAnswers, repeater))
    }

    def defaultPageOrFirstSectionNumber: SectionNumber = defaultPage.map(_.sectionNumber).getOrElse(firstSectionNumber)

    def firstSectionNumber: SectionNumber = singletons.head.sectionNumber

    def lastSectionNumber: SectionNumber = singletons.last.sectionNumber

    def isCommited(visitsIndex: VisitIndex): Boolean =
      singletons.forall { singletonWithNumber =>
        visitsIndex.contains(singletonWithNumber.sectionNumber)
      }
  }

  case class NonRepeatingPage[A <: PageMode](singleton: SingletonWithNumber[A], source: Section.NonRepeatingPage)
      extends Bracket[A] {
    def map[B <: PageMode](
      e: Singleton[A] => Singleton[B],
      f: CheckYourAnswers[A] => CheckYourAnswers[B],
      g: Repeater[A] => Repeater[B]
    ): NonRepeatingPage[B] =
      NonRepeatingPage(
        SingletonWithNumber(e(singleton.singleton), singleton.sectionNumber),
        source
      )

    def hasSectionNumber(sectionNumber: SectionNumber): Boolean = this.singleton.sectionNumber === sectionNumber
  }

  case class RepeatingPage[A <: PageMode](
    singletons: NonEmptyList[SingletonWithNumber[A]], // There must be at at least one page in repeating page bracket
    source: Section.RepeatingPage
  ) extends Bracket[A] {
    def map[B <: PageMode](
      e: Singleton[A] => Singleton[B],
      f: CheckYourAnswers[A] => CheckYourAnswers[B],
      g: Repeater[A] => Repeater[B]
    ): RepeatingPage[B] =
      RepeatingPage(
        singletons.map(_.map(e)),
        source
      )

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

    def indexToRemove(
      sectionNumber: SectionNumber,
      formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser]
    ): Option[Int] = {
      val (iteration, index) = iterationForSectionNumberWithIndex(sectionNumber)
      val pageRemove = iteration.singletons.toList
        .find(_.sectionNumber === sectionNumber)
        .flatMap(s => s.singleton.page.removeItemIf)
        .exists(
          formModelVisibilityOptics.evalRemoveItemIf
        )
      val cyaRemove = iteration.checkYourAnswers match {
        case Some(cya) if cya.sectionNumber === sectionNumber =>
          cya.checkYourAnswers.expandedRemoveItemIf
            .exists(
              formModelVisibilityOptics.evalRemoveItemIf
            )
        case _ => false
      }
      if (pageRemove || cyaRemove) {
        Some(index)
      } else {
        None
      }
    }

    def map[B <: PageMode](
      e: Singleton[A] => Singleton[B],
      f: CheckYourAnswers[A] => CheckYourAnswers[B],
      g: Repeater[A] => Repeater[B]
    ): AddToList[B] = AddToList(
      iterations.map(_.map(e, f, g)),
      source
    )

    def hasSectionNumber(sectionNumber: SectionNumber): Boolean =
      iterations.exists { iteration =>
        iteration.hasSectionNumber(sectionNumber)
      }

    def iterationForSectionNumber(sectionNumber: SectionNumber): Bracket.AddToListIteration[A] =
      iterations.toList
        .collectFirst {
          case iteration if iteration.hasSectionNumber(sectionNumber) => iteration
        }
        .getOrElse(
          throw new IllegalArgumentException(s"Invalid sectionNumber: $sectionNumber for Bracket.AddToListIteration")
        )

    private def iterationForSectionNumberWithIndex(sectionNumber: SectionNumber): (Bracket.AddToListIteration[A], Int) =
      iterations.zipWithIndex.toList
        .collectFirst {
          case (iteration, i) if iteration.hasSectionNumber(sectionNumber) => (iteration, i)
        }
        .getOrElse(
          throw new IllegalArgumentException(s"Invalid sectionNumber: $sectionNumber for Bracket.AddToListIteration")
        )

    def repeaters: NonEmptyList[Repeater[A]] = iterations.map(_.repeater.repeater)

    def lastSectionNumber: SectionNumber = iterations.last.repeater.sectionNumber
  }
}
