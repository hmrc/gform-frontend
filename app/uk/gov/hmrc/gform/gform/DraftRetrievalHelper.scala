/*
 * Copyright 2024 HM Revenue & Customs
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

import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate

object DraftRetrievalHelper {
  def isNotPermitted(formTemplate: FormTemplate, retrievals: MaterialisedRetrievals): Boolean =
    ((formTemplate.draftRetrieval, retrievals.getAffinityGroup) match {
      case (Some(dr), Some(affinityGroup)) => dr.mapping.get(affinityGroup).exists(_.isNotPermitted)
      case _                               => false
    }) || formTemplate.draftRetrievalMethod.isNotPermitted
}
