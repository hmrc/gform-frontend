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

import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.TextConstraint

case class StaticTypeInfo(lookup: Map[BaseComponentId, StaticTypeData]) extends AnyVal {
  def get(baseComponentId: BaseComponentId): Option[StaticTypeData] = lookup.get(baseComponentId)

  def ++(staticTypeInfo: StaticTypeInfo): StaticTypeInfo =
    StaticTypeInfo(lookup ++ staticTypeInfo.lookup)
}

object StaticTypeInfo {
  val empty: StaticTypeInfo = StaticTypeInfo(Map.empty[BaseComponentId, StaticTypeData])
}

case class StaticTypeData(exprType: ExprType, textConstraint: Option[TextConstraint])

object StaticTypeData {
  val illegal: StaticTypeData = StaticTypeData(ExprType.illegal, None)
}
