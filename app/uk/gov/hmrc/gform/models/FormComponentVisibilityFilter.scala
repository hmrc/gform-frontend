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

import com.softwaremill.quicklens._
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormPhase, Group, RevealingChoice }

class FormComponentVisibilityFilter[D <: DataOrigin, P <: PageMode](
  formModelVisibilityOptics: FormModelVisibilityOptics[D],
  phase: Option[FormPhase]
) {

  private def isVisible(fc: FormComponent): Boolean =
    fc.includeIf.fold(true) { includeIf =>
      formModelVisibilityOptics.evalIncludeIfExpr(includeIf, phase)
    }

  private def stripHiddenFormComponentsFromRevealingCoice(rc: RevealingChoice): RevealingChoice =
    rc.modify(_.options.each.revealingFields)
      .using(_.filter(isVisible))

  private def stripHiddenFormComponentsFromGroup(group: Group): Group =
    group.modify(_.fields).using(_.filter(isVisible))

  def stripHiddenFormComponents(formModel: FormModel[P]): FormModel[P] =
    formModel.map[P] { singleton =>
      singleton
        .modify(_.page.fields)
        .using(_.filter(isVisible))
        .modify(_.page.fields.each.`type`)
        .using {
          case rc: RevealingChoice => stripHiddenFormComponentsFromRevealingCoice(rc)
          case group: Group        => stripHiddenFormComponentsFromGroup(group)
          case i                   => i
        }
    }(identity)
}

object FormComponentVisibilityFilter {
  def apply[D <: DataOrigin, P <: PageMode](
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    phase: Option[FormPhase]
  ) = new FormComponentVisibilityFilter[D, P](formModelVisibilityOptics, phase)
}
