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

package uk.gov.hmrc.gform.graph

import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData }

case class RecData[S <: SourceOrigin](
  variadicFormData: VariadicFormData[S]
) extends AnyVal {
  def cleared(modelComponentIds: List[ModelComponentId]): VariadicFormData[S] =
    variadicFormData -- modelComponentIds
}

object RecData {
  val empty = fromData[SourceOrigin.Current](VariadicFormData.empty)
  def fromData[S <: SourceOrigin](variadicFormData: VariadicFormData[S]): RecData[S] =
    RecData(variadicFormData)
}
