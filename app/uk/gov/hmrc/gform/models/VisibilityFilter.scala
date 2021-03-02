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

package uk.gov.hmrc.gform.models

import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormPhase, Group, IsGroup, IsRevealingChoice, RevealingChoice }

class VisibilityFilter[D <: DataOrigin, P <: PageMode](
  formModelVisibilityOptics: FormModelVisibilityOptics[D],
  phase: Option[FormPhase]
) {

  private def isVisible(fc: FormComponent): Boolean =
    fc.includeIf.fold(true) { includeIf =>
      formModelVisibilityOptics.evalIncludeIfExpr(includeIf, phase)
    }

  private def stripHiddenFormComponentsFromRevealingCoice(rc: RevealingChoice): RevealingChoice =
    rc.copy(
      options = rc.options.map { rcElement =>
        rcElement.copy(revealingFields = rcElement.revealingFields.filter(isVisible))
      }
    )

  private def stripHiddenFormComponentsFromGroup(group: Group): Group =
    group.copy(
      fields = group.fields.filter(isVisible)
    )

  def stripHiddenFormComponents(formModel: FormModel[P]): FormModel[P] =
    formModel.map[P] { singleton =>
      val visibleFields: List[FormComponent] =
        singleton.page.fields
          .map {
            case fc @ IsRevealingChoice(rc) => fc.copy(`type` = stripHiddenFormComponentsFromRevealingCoice(rc))
            case fc @ IsGroup(group)        => fc.copy(`type` = stripHiddenFormComponentsFromGroup(group))
            case i                          => i
          }
          .filter(isVisible)
      singleton.copy(page = singleton.page.copy(fields = visibleFields))
    }(identity)
}

object VisibilityFilter {
  def apply[D <: DataOrigin, P <: PageMode](
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    phase: Option[FormPhase]
  ) = new VisibilityFilter[D, P](formModelVisibilityOptics, phase)
}
