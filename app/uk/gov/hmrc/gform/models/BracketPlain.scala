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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Section

sealed trait BracketPlain[A <: PageMode] extends Product with Serializable {
  def fold[B](
    f: BracketPlain.NonRepeatingPage[A] => B
  )(
    g: BracketPlain.RepeatingPage[A] => B
  )(
    h: BracketPlain.AddToList[A] => B
  ): B =
    this match {
      case b: BracketPlain.NonRepeatingPage[A] => f(b)
      case b: BracketPlain.RepeatingPage[A]    => g(b)
      case b: BracketPlain.AddToList[A]        => h(b)
    }
}

object BracketPlain {
  case class AddToListIteration[A <: PageMode](singletons: NonEmptyList[Singleton[A]], repeater: Repeater[A])

  case class NonRepeatingPage[A <: PageMode](singleton: Singleton[A], source: Section.NonRepeatingPage)
      extends BracketPlain[A]
  case class RepeatingPage[A <: PageMode](singletons: NonEmptyList[Singleton[A]], source: Section.RepeatingPage)
      extends BracketPlain[A]
  case class AddToList[A <: PageMode](iterations: NonEmptyList[AddToListIteration[A]], source: Section.AddToList)
      extends BracketPlain[A]

}
