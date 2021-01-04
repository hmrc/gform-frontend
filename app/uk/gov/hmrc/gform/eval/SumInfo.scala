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

package uk.gov.hmrc.gform.eval

import cats.Monoid
import cats.syntax.eq._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormCtx, Sum }

case class SumInfo(lookup: Map[Sum, Set[FormComponentId]]) extends AnyVal {

  def keys: Set[Sum] = lookup.keySet

  def ++(sumInfo: SumInfo): SumInfo =
    SumInfo(lookup ++ sumInfo.lookup)

  def sums: Set[Sum] = lookup.keys.toSet

  def dependees(formComponentId: FormComponentId): Option[Set[FormComponentId]] = {

    val res: Set[FormComponentId] = lookup.toList
      .collect {
        case (Sum(FormCtx(fcId)), dependeesFcId) if (fcId.baseComponentId === formComponentId.baseComponentId) =>
          dependeesFcId
      }
      .toSet
      .flatten

    if (res.isEmpty) None else Some(res)
  }
}

object SumInfo {
  val empty: SumInfo = SumInfo(Map.empty[Sum, Set[FormComponentId]])

  implicit def monoid: Monoid[SumInfo] = new Monoid[SumInfo] {
    def empty: SumInfo = SumInfo.empty

    def combine(si1: SumInfo, si2: SumInfo): SumInfo = SumInfo {
      (si1.lookup.keys ++ si2.lookup.keys).map { key =>
        key -> (getSet(key, si1) ++ getSet(key, si2))
      }.toMap
    }

    private def getSet(key: Sum, sumInfo: SumInfo): Set[FormComponentId] =
      sumInfo.lookup.get(key).getOrElse(Set.empty[FormComponentId])
  }
}
