/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.recalculation

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.models.PageModel
import uk.gov.hmrc.gform.models.ids.ModelPageId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber

class FormModelMetadata(
  val pageIdSectionNumberMap: Map[ModelPageId, SectionNumber]
)

object FormModelMetadata {
  val notAvailable: FormModelMetadata = new FormModelMetadata(
    Map.empty[ModelPageId, SectionNumber]
  )

  def fromPageModels(pagesWithIndex: NonEmptyList[(PageModel, SectionNumber)]): FormModelMetadata = {
    val pageIdSectionNumberMap: Map[ModelPageId, SectionNumber] = pagesWithIndex.toList.flatMap {
      case (pageModel, number) =>
        pageModel.id.map(id => (id.modelPageId, number))
    }.toMap
    new FormModelMetadata(
      pageIdSectionNumberMap
    )
  }
}
