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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import com.softwaremill.quicklens._
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Choice, FormComponent, FormPhase, Group, OptionData, RevealingChoice }

class FormComponentVisibilityFilter[D <: DataOrigin, P <: PageMode](
  formModelVisibilityOptics: FormModelVisibilityOptics[D],
  phase: Option[FormPhase]
) {

  private def isVisible(fc: FormComponent): Boolean =
    fc.includeIf.fold(true) { includeIf =>
      formModelVisibilityOptics.evalIncludeIfExpr(includeIf, phase)
    }

  private def stripHiddenFormComponentsFromRevealingChoice(rc: RevealingChoice): RevealingChoice =
    rc.modify(_.options)
      .using(_.filter(rce => isVisibleOption(rce.choice)))
      .modify(_.options.each.revealingFields)
      .using(_.filter(isVisible))

  private def stripHiddenFormComponentsFromGroup(group: Group): Group =
    group.modify(_.fields).using(_.filter(isVisible))

  private def isVisibleOption(optionData: OptionData): Boolean = optionData match {
    case OptionData.ValueBased(_, _, includeIf, _, _, _) =>
      includeIf.fold(true)(includeIf => formModelVisibilityOptics.evalIncludeIfExpr(includeIf, phase))
    case OptionData.IndexBased(_, _, includeIf, _, _) =>
      includeIf.fold(true)(includeIf => formModelVisibilityOptics.evalIncludeIfExpr(includeIf, phase))
  }

  private def stripHiddenFormComponentsFromChoice(c: Choice): Choice =
    c.modify(_.options)
      .using(o =>
        NonEmptyList
          .fromList(o.filter(isVisibleOption))
          .getOrElse(throw new IllegalArgumentException("All options of the choice component are invisible"))
      )

  def stripHiddenFormComponents(formModel: FormModel[P]): FormModel[P] =
    formModel.map[P] { singleton =>
      singleton
        .modify(_.page.fields)
        .using(_.filter(isVisible))
        .modify(_.page.fields.each.`type`)
        .using {
          case rc: RevealingChoice => stripHiddenFormComponentsFromRevealingChoice(rc)
          case c: Choice =>
            val isPageVisible = singleton.page.includeIf.fold(true) { includeIf =>
              formModelVisibilityOptics.evalIncludeIfExpr(includeIf, phase)
            }
            if (isPageVisible) stripHiddenFormComponentsFromChoice(c) else c
          case group: Group => stripHiddenFormComponentsFromGroup(group)
          case i            => i
        }
    }(identity)(identity)
}

object FormComponentVisibilityFilter {
  def apply[D <: DataOrigin, P <: PageMode](
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    phase: Option[FormPhase]
  ) = new FormComponentVisibilityFilter[D, P](formModelVisibilityOptics, phase)
}
