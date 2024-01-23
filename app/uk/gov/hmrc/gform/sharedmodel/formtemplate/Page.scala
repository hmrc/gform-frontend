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

import cats.data.NonEmptyList
import play.api.libs.json.OFormat
import cats.syntax.eq._
import julienrf.json.derived
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.eval.{ RevealingChoiceData, RevealingChoiceInfo, StaticTypeInfo }
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, SmartString }
import uk.gov.hmrc.gform.gform.RenderUnit
import uk.gov.hmrc.gform.models.{ Basic, PageMode, SectionHeader }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.LayoutDisplayWidth.LayoutDisplayWidth
import com.softwaremill.quicklens._

case class Page[A <: PageMode](
  title: SmartString,
  id: Option[PageId],
  noPIITitle: Option[SmartString],
  description: Option[SmartString],
  shortName: Option[SmartString],
  caption: Option[SmartString] = None,
  includeIf: Option[IncludeIf],
  validators: Option[Validator],
  fields: List[FormComponent],
  continueLabel: Option[SmartString],
  continueIf: Option[ContinueIf],
  instruction: Option[Instruction],
  presentationHint: Option[PresentationHint],
  dataRetrieve: Option[NonEmptyList[DataRetrieve]],
  confirmation: Option[Confirmation],
  redirects: Option[NonEmptyList[RedirectCtx]],
  hideSaveAndComeBackButton: Option[Boolean],
  removeItemIf: Option[RemoveItemIf],
  displayWidth: Option[LayoutDisplayWidth]
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

  def updateExpr(f: Expr => Expr): Page[A] =
    this
      .modify(_.title)
      .using(_.updateExpr(f))
      .modify(_.noPIITitle.each)
      .using(_.updateExpr(f))
      .modify(_.description.each)
      .using(_.updateExpr(f))
      .modify(_.caption.each)
      .using(_.updateExpr(f))
      .modify(_.includeIf.each.booleanExpr)
      .using(_.updateExpr(f))
      .modify(_.validators.each)
      .using(_.updateExpr(f))
      .modify(_.fields.each)
      .using(_.updateExpr(f))
      .modify(_.continueLabel.each)
      .using(_.updateExpr(f))
      .modify(_.instruction.each.name.each)
      .using(_.updateExpr(f))
      .modify(_.confirmation.each.question)
      .using(_.updateExpr(f))
      .modify(_.removeItemIf.each.booleanExpr)
      .using(_.updateExpr(f))
      .copy(dataRetrieve = dataRetrieve.map(_.map(_.updateExpr(f))))
      .copy(redirects = redirects.map(_.map(_.updateExpr(f))))
}

object Page {
  import JsonUtils._
  implicit val pageFormat: OFormat[Page[Basic]] = derived.oformat()
}
