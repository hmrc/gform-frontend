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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, SectionNumber }

case class BracketsWithSectionNumber[A <: PageMode](brackets: NonEmptyList[Bracket[A]]) {
  def addToListById(addToListId: AddToListId, idx: Int): Bracket.AddToListIteration[A] =
    addToListBracket(addToListId).iterations.toList(idx)

  def addToListBracket(addToListId: AddToListId): Bracket.AddToList[A] =
    brackets.toList
      .collectFirst {
        case b: Bracket.AddToList[A] if b.isToListById(addToListId) => b
      }
      .getOrElse(throw new IllegalArgumentException(s"Invalid addToListId $addToListId"))

  def addToListBrackets: List[Bracket.AddToList[A]] = brackets.collect {
    case x: Bracket.AddToList[A] => x
  }
  def nonRepeatingPageBrackets: List[Bracket.NonRepeatingPage[A]] = brackets.collect {
    case x: Bracket.NonRepeatingPage[A] => x
  }
  def repeatingPageBrackets: List[Bracket.RepeatingPage[A]] = brackets.collect {
    case x: Bracket.RepeatingPage[A] => x
  }

  def withSectionNumber(sectionNumber: SectionNumber): Bracket[A] =
    brackets
      .find(_.hasSectionNumber(sectionNumber))
      .getOrElse(throw new IllegalArgumentException(s"Wrong sectionNumber $sectionNumber"))

  val toBrackets: NonEmptyList[BracketPlain[A]] = brackets.map(_.toPlainBracket)

  def toPageModelWithNumber: NonEmptyList[(PageModel[A], SectionNumber)] = brackets.flatMap(_.toPageModelWithNumber)

}
