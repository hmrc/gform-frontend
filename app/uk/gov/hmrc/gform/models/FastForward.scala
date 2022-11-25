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
import uk.gov.hmrc.gform.controllers.FastForwardNavigator
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionOrSummary

sealed trait FastForward extends Product with Serializable {

  private def fold[B](
    f: FastForward.NextNew.type => B
  )(g: FastForward.StopAt => B)(h: FastForward.CYA => B): B =
    this match {
      case y: FastForward.NextNew.type => f(y)
      case s: FastForward.StopAt       => g(s)
      case yy: FastForward.CYA         => h(yy)
    }

  def asString: String =
    fold(_ => FastForward.ffYes)(_.stopAt.value.toString) {
      case FastForward.CYA(from, SectionOrSummary.FormSummary) => FastForward.ffCYA + from.value.toString
      case FastForward.CYA(from, SectionOrSummary.TaskSummary) => FastForward.ffCYA + from.value.toString + "n"
      case FastForward.CYA(from, SectionOrSummary.Section(to)) =>
        FastForward.ffCYA + from.value.toString + "." + to.value.toString
    }

  def next(formModel: FormModel[Visibility], sn: SectionNumber): FastForward =
    fold[FastForward](identity) { st =>
      formModel.availableSectionNumbers
        .find(s => s >= st.stopAt && s >= sn)
        .map(availableSectionNumber =>
          StopAt(FastForwardNavigator(formModel).nextSectionNumber(availableSectionNumber))
        )
        .getOrElse(st)
    } { case cya @ FastForward.CYA(from, to) =>
      formModel
        .nextVisibleSectionNumber(sn)
        .map(s =>
          to match {
            case SectionOrSummary.Section(s) if sn == s => FastForward.NextNew
            case _                                      => FastForward.CYA(s, to)
          }
        )
        .getOrElse(cya)
    }

  def goOn(sectionNumber: SectionNumber): Boolean =
    fold(_ => true)(_.stopAt > sectionNumber)(_ => true)

}

object FastForward {

  case object NextNew extends FastForward
  case class StopAt(stopAt: SectionNumber) extends FastForward
  case class CYA(from: SectionNumber, to: SectionOrSummary = SectionOrSummary.FormSummary) extends FastForward

  val ffYes = "t"
  val ffCYA = "cya"

  implicit val equal: Eq[FastForward] = Eq.fromUniversalEquals

}
