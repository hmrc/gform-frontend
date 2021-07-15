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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.Monoid
import play.api.libs.json.{ Json, OFormat }
import cats.syntax.eq._
import uk.gov.hmrc.gform.eval.{ RevealingChoiceData, RevealingChoiceInfo, StaticTypeInfo, SumInfo }
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.gform.RenderUnit
import uk.gov.hmrc.gform.models.{ Basic, PageMode }

case class Page[A <: PageMode](
  title: SmartString,
  documentTitle: Option[SmartString],
  description: Option[SmartString],
  shortName: Option[SmartString],
  progressIndicator: Option[SmartString] = None,
  includeIf: Option[IncludeIf],
  validators: Option[Validator],
  fields: List[FormComponent],
  continueLabel: Option[SmartString],
  continueIf: Option[ContinueIf],
  instruction: Option[Instruction],
  presentationHint: Option[PresentationHint]
) {

  val allIds: List[FormComponentId] = fields.map(_.id) ++ fields.flatMap(_.childrenFormComponents.map(_.id))

  val staticTypeInfo: StaticTypeInfo = StaticTypeInfo {
    (fields ++ fields
      .flatMap(_.childrenFormComponents))
      .map(fc => fc.baseComponentId -> fc.staticTypeData)
      .toMap
  }

  val revealingChoiceInfo: RevealingChoiceInfo = RevealingChoiceInfo {
    fields
      .collect { case fc @ IsRevealingChoice(revealingChoice) =>
        revealingChoice.options.zipWithIndex.flatMap { case (revealingChoiceElement, index) =>
          revealingChoiceElement.revealingFields.map { rf =>
            rf.id.baseComponentId -> RevealingChoiceData(index, fc.id.baseComponentId)
          }
        }.toMap
      }
      .foldLeft(Map.empty[BaseComponentId, RevealingChoiceData])(_ ++ _)
  }

  val sumInfo: SumInfo = implicitly[Monoid[SumInfo]].combineAll(
    (fields ++ fields
      .flatMap(_.childrenFormComponents)).collect {
      case fc @ HasValueExpr(expr) if expr.sums.nonEmpty =>
        SumInfo(expr.sums.map(sum => (sum, Set(fc.id))).toMap)
    }
  )

  def renderUnits: List[RenderUnit] = fields.foldRight(List.empty[RenderUnit]) {
    case (formComponent, (h @ RenderUnit.Group(baseComponentId, groupFormComponents)) :: xs) =>
      formComponent match {
        case IsGroup(group) =>
          if (baseComponentId === formComponent.baseComponentId)
            h.prepend((group, formComponent)) :: xs
          else
            RenderUnit.group(group, formComponent) :: h :: xs
        case otherwise => RenderUnit.pure(formComponent) :: h :: xs
      }
    case (formComponent, acc) =>
      val start = formComponent match {
        case IsGroup(group) => RenderUnit.group(group, formComponent)
        case otherwise      => RenderUnit.pure(formComponent)
      }
      start :: acc
  }
  val isTerminationPage: Boolean = continueIf.contains(Stop)

}

object Page {
  implicit val pageFormat: OFormat[Page[Basic]] = Json.format[Page[Basic]]
}
