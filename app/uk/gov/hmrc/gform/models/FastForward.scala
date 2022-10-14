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

package uk.gov.hmrc.gform.models

import cats.Eq
import uk.gov.hmrc.gform.models.FastForward.StopAt
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber

sealed trait FastForward extends Product with Serializable {

  private def fold[B](
    f: FastForward.Yes.type => B
  )(g: FastForward.StopAt => B)(h: FastForward.CYA => B): B =
    this match {
      case y: FastForward.Yes.type => f(y)
      case s: FastForward.StopAt   => g(s)
      case yy: FastForward.CYA     => h(yy)
    }

  def asString: String =
    fold(_ => FastForward.ffYes)(_.stopAt.value.toString) {
      case FastForward.CYA(from, None)     => FastForward.ffCYA + from.value.toString
      case FastForward.CYA(from, Some(to)) => FastForward.ffCYA + from.value.toString + "." + to.value.toString
    }

  def next(formModel: FormModel[Visibility], sn: SectionNumber): FastForward = fold[FastForward](identity) { st =>
    formModel.availableSectionNumbers
      .find(_ >= st.stopAt)
      .map(availableSectionNumber => StopAt(availableSectionNumber.increment))
      .getOrElse(st)
  } { case cya @ FastForward.CYA(from, to) =>
    formModel
      .nextVisibleSectionNumber(sn)
      .map(s => if (to.map(_ == s).getOrElse(false)) FastForward.Yes else FastForward.CYA(s, to))
      .getOrElse(cya)
  }

  def goOn(sectionNumber: SectionNumber): Boolean =
    fold(_ => true)(_.stopAt > sectionNumber)(_ => true)

}

object FastForward {

  case object Yes extends FastForward
  case class StopAt(stopAt: SectionNumber) extends FastForward
  case class CYA(from: SectionNumber, to: Option[SectionNumber] = None) extends FastForward // {

  val ffYes = "t"
  val ffCYA = "cya"

  implicit val equal: Eq[FastForward] = Eq.fromUniversalEquals

}
