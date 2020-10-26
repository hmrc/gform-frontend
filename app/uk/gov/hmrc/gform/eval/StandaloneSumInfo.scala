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

import cats.syntax.eq._
import uk.gov.hmrc.gform.models.{ PageMode, PageModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormCtx, Sum }

/**
  * This represents ${abc.sum} expression, where this expression
  * is used only in SmartText.
  */
case class StandaloneSumInfo(sums: Set[Sum]) extends AnyVal {
  def dependees(formComponentId: FormComponentId): Option[FormComponentId] =
    sums.collectFirst {
      case Sum(FormCtx(fcId)) if (fcId.baseComponentId === formComponentId.baseComponentId) => formComponentId
    }
}

object StandaloneSumInfo {

  val empty: StandaloneSumInfo = StandaloneSumInfo(Set.empty[Sum])

  def from[A <: PageMode](
    pages: List[PageModel[A]],
    sumInfo: SumInfo
  ): StandaloneSumInfo = {
    val allSums: List[Set[Sum]] = pages.collect {
      case AllPageModelSums(sums) => sums
    }
    val sums = sumInfo.keys
    val standaloneSums: Set[Sum] = allSums.toSet.flatten.filterNot(sums)
    StandaloneSumInfo(standaloneSums)
  }

}
