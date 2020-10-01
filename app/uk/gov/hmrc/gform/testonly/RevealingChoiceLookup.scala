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

package uk.gov.hmrc.gform.testonly

import uk.gov.hmrc.gform.models.{ DataExpanded, FormModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, IsRevealingChoice }

class RevealingChoiceLookup(formComponents: List[FormComponent]) {
  val revealingChoiceChildren: List[FormComponentId] = formComponents.collect {
    case IsRevealingChoice(rc) => rc.options.toList.flatMap(_.revealingFields.map(_.id))
  }.flatten
  def isRevealingChoiceChild(fcId: FormComponentId): Boolean = revealingChoiceChildren.contains(fcId)
}

object RevealingChoiceLookup {
  def apply(formComponents: List[FormComponent]) = new RevealingChoiceLookup(formComponents)
  def apply(formModel: FormModel[DataExpanded]) = new RevealingChoiceLookup(formModel.allFormComponents)
}
