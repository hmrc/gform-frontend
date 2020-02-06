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

import cats.Eq
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber

sealed trait FastForward extends Product with Serializable {
  def asString: String = fold(_ => FastForward.ffYes)(_.stopAt.value.toString)

  def next: FastForward = fold[FastForward](identity)(st => FastForward.StopAt(st.stopAt + 1))

  def goOn(sectionNumber: SectionNumber): Boolean = fold(_ => true)(_.stopAt > sectionNumber)

  private def fold[B](f: FastForward.Yes.type => B)(g: FastForward.StopAt => B): B = this match {
    case y: FastForward.Yes.type => f(y)
    case s: FastForward.StopAt   => g(s)
  }
}

object FastForward {

  case object Yes extends FastForward
  case class StopAt(stopAt: SectionNumber) extends FastForward

  val ffYes = "t"

  implicit val equal: Eq[FastForward] = Eq.fromUniversalEquals

}
