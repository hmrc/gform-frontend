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

package uk.gov.hmrc.gform.gform

import uk.gov.hmrc.gform.graph.processor.IdentifierExtractor
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.DestinationIncludeIfEval
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.IncludeIfValue
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList

object DestinationIncludeIfEvalBuilder extends IdentifierExtractor {

  def apply[D <: DataOrigin](
    formTemplate: FormTemplate,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ): DestinationIncludeIfEval = {

    val allExpr = formTemplate.destinations match {
      case dl: DestinationList =>
        dl.destinations.toList.flatMap(d =>
          d.includeIf match {
            case IncludeIfValue(includeIf) => Map(d.id -> formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None))
            case _                         => Map.empty[DestinationId, Boolean]
          }
        )
      case _ => List.empty[(DestinationId, Boolean)]
    }
    DestinationIncludeIfEval(allExpr)
  }

}
