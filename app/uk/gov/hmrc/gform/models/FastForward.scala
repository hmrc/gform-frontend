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

import cats.Eq
import uk.gov.hmrc.gform.models.FastForward.{ BackUntil, StopAt }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber
import uk.gov.hmrc.gform.controllers.Navigator
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionOrSummary

sealed trait FastForward extends Product with Serializable {

  private def fold[B](
    f: FastForward.Yes.type => B
  )(g: FastForward.StopAt => B)(h: FastForward.CYA => B)(i: FastForward.BackUntil => B): B =
    this match {
      case y: FastForward.Yes.type  => f(y)
      case s: FastForward.StopAt    => g(s)
      case yy: FastForward.CYA      => h(yy)
      case b: FastForward.BackUntil => i(b)
    }

  def asString: String =
    fold(_ => FastForward.ffYes)(_.stopAt.value) {
      case FastForward.CYA(SectionOrSummary.FormSummary) => FastForward.ffCYAFormSummary
      case FastForward.CYA(SectionOrSummary.TaskSummary) => FastForward.ffCYATaskSummary
      case FastForward.CYA(SectionOrSummary.Section(to)) =>
        FastForward.ffCYA + to.value
    }("back" + _.until.value)

  def next(formModel: FormModel[Visibility], sn: SectionNumber): FastForward =
    fold[FastForward](identity) { st =>
      formModel.availableSectionNumbers
        .find(s => s >= st.stopAt && s >= sn)
        .map(availableSectionNumber => StopAt(Navigator(availableSectionNumber, formModel).nextSectionNumber))
        .getOrElse(st)
    } { case cya @ FastForward.CYA(to) =>
      formModel
        .nextVisibleSectionNumber(sn)
        .map(s =>
          to match {
            case SectionOrSummary.Section(s) if sn == s => FastForward.Yes
            case _                                      => FastForward.CYA(to)
          }
        )
        .getOrElse(cya)
    } { back =>
      formModel.availableSectionNumbers
        .find(s => s <= back.until && s <= sn)
        .map(availableSectionNumber =>
          BackUntil(Navigator(availableSectionNumber, formModel).previousSectionNumber.getOrElse(back.until))
        )
        .getOrElse(back)
    }

  def goOn(sectionNumber: SectionNumber): Boolean =
    fold(_ => true)(_.stopAt > sectionNumber)(_ => true)(_.until <= sectionNumber)

}

object FastForward {

  case object Yes extends FastForward
  case class StopAt(stopAt: SectionNumber) extends FastForward
  case class CYA(to: SectionOrSummary = SectionOrSummary.FormSummary) extends FastForward
  case class BackUntil(until: SectionNumber) extends FastForward

  val ffYes = "t"
  val ffCYAFormSummary = "cyaf"
  val ffCYATaskSummary = "cyat"
  val ffCYA = "cya"

  implicit val equal: Eq[FastForward] = Eq.fromUniversalEquals

}
