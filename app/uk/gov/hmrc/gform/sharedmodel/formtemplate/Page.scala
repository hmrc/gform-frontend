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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.Monoid
import cats.data.NonEmptyList
import play.api.libs.json.OFormat
import cats.syntax.eq._
import julienrf.json.derived
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.eval.{ RevealingChoiceData, RevealingChoiceInfo, StaticTypeInfo, SumInfo }
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, SmartString }
import uk.gov.hmrc.gform.gform.RenderUnit
import uk.gov.hmrc.gform.models.{ Basic, PageMode, SectionHeader }

case class Page[A <: PageMode](
  title: SmartString,
  id: Option[PageId],
  noPIITitle: Option[SmartString],
  description: Option[SmartString],
  shortName: Option[SmartString],
  caption: Option[SmartString] = None,
  includeIf: Option[IncludeIf],
  validator: Option[Validator],
  validators: Option[Validators], //this will be removed by GFORMS-2279
  fields: List[FormComponent],
  continueLabel: Option[SmartString],
  continueIf: Option[ContinueIf],
  instruction: Option[Instruction],
  presentationHint: Option[PresentationHint],
  dataRetrieve: Option[NonEmptyList[DataRetrieve]],
  confirmation: Option[Confirmation],
  redirects: Option[NonEmptyList[RedirectCtx]],
  hideSaveAndComeBackButton: Option[Boolean],
  removeItemIf: Option[RemoveItemIf]
) {

  def sectionHeader()(implicit sse: SmartStringEvaluator) = SectionHeader(
    title.value(),
    description.map(ls => ls.value()),
    caption.map(ls => ls.value())
  )

  val allFields: List[FormComponent] = confirmation.fold(fields)(fields ::: _.question :: Nil)

  val allIds: List[FormComponentId] = allFields.map(_.id) ++ allFields.flatMap(_.childrenFormComponents.map(_.id))

  val allFieldsNested: List[FormComponent] = allFields ++ allFields.flatMap(_.childrenFormComponents)

  val staticTypeInfo: StaticTypeInfo = StaticTypeInfo {
    (allFields ++ allFields
      .flatMap(_.childrenFormComponents))
      .map(fc => fc.baseComponentId -> fc.staticTypeData)
      .toMap
  }

  val revealingChoiceInfo: RevealingChoiceInfo = RevealingChoiceInfo {
    allFields
      .collect { case fc @ IsRevealingChoice(revealingChoice) =>
        revealingChoice.options.zipWithIndex.flatMap { case (revealingChoiceElement, index) =>
          revealingChoiceElement.revealingFields.map { rf =>
            rf.id.baseComponentId -> RevealingChoiceData(
              revealingChoiceElement.choice.value(index),
              fc.id.baseComponentId
            )
          }
        }.toMap
      }
      .foldLeft(Map.empty[BaseComponentId, RevealingChoiceData])(_ ++ _)
  }

  val sumInfo: SumInfo = implicitly[Monoid[SumInfo]].combineAll(
    (allFields ++ allFields
      .flatMap(_.childrenFormComponents)).collect {
      case fc @ HasValueExpr(expr) if expr.sums.nonEmpty =>
        SumInfo(expr.sums.map(sum => (sum, Set(fc.id))).toMap)
    }
  )

  def renderUnits: List[RenderUnit] =
    allFields.foldRight(List.empty[RenderUnit]) {
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
  val isHideSaveAndComeBackButton: Boolean = hideSaveAndComeBackButton.getOrElse(false)

  def dataRetrieves(): List[DataRetrieve] = dataRetrieve.toList.flatMap(_.toList)
}

object Page {
  import JsonUtils._
  implicit val pageFormat: OFormat[Page[Basic]] = derived.oformat()
}
