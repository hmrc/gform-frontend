/*
 * Copyright 2022 HM Revenue & Customs
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

import uk.gov.hmrc.gform.models.ExpandUtils
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import SelectionCriteriaValue._

class FormComponentUpdater(formComponent: FormComponent, index: Int, baseIds: List[FormComponentId]) {

  private def expandExpr(expr: Expr): Expr = ExprUpdater(expr, index, baseIds)
  private def expandFormCtx(formCtx: FormCtx): FormCtx = ExprUpdater.formCtx(formCtx, index, baseIds)

  private def expandBooleanExpr(booleanExpr: BooleanExpr): BooleanExpr = BooleanExprUpdater(booleanExpr, index, baseIds)

  private def expandOptionData(optionData: OptionData): OptionData = optionData match {
    case o: OptionData.IndexBased =>
      o.copy(label = expandSmartString(o.label), includeIf = o.includeIf.map(expandIncludeIf))
    case o: OptionData.ValueBased =>
      o.copy(label = expandSmartString(o.label), includeIf = o.includeIf.map(expandIncludeIf))
  }

  private def expandRevealingChoice(revealingChoice: RevealingChoice): RevealingChoice =
    revealingChoice.copy(options = revealingChoice.options.map { revealingChoiceElement =>
      revealingChoiceElement.copy(
        choice = expandOptionData(revealingChoiceElement.choice),
        revealingFields = revealingChoiceElement.revealingFields.map { revealingField =>
          new FormComponentUpdater(revealingField, index, baseIds).updatedWithId
        }
      )
    })

  private def expandGroup(group: Group): Group = group.copy(
    fields = group.fields.map(field => new FormComponentUpdater(field, index, baseIds).updatedWithId),
    repeatLabel = group.repeatLabel.map(expandSmartString),
    repeatAddAnotherText = group.repeatAddAnotherText.map(expandSmartString)
  )

  private def expandChoice(choice: Choice): Choice = choice.copy(
    options = choice.options.map(expandOptionData),
    optionHelpText = choice.optionHelpText.map(_.map(expandSmartString))
  )

  private def expandIncludeIf(includeIf: IncludeIf) = IncludeIf(expandBooleanExpr(includeIf.booleanExpr))
  private def expandValidIf(validIf: ValidIf) = ValidIf(expandBooleanExpr(validIf.booleanExpr))

  private def expandFormComponentValidator(formComponentValidator: FormComponentValidator) =
    formComponentValidator.copy(
      validIf = expandValidIf(formComponentValidator.validIf),
      errorMessage = expandSmartString(formComponentValidator.errorMessage)
    )

  private def expandSmartString(smartString: SmartString) = smartString.expand(index, baseIds)

  private def expandText(text: Text): Text = {
    val scText = text match {
      case txt @ Text(constraint @ Lookup(_, Some(scs)), _, _, _, _, _) =>
        val updatedSelectionCriteria = scs.map {
          case sc @ SelectionCriteria(_, SelectionCriteriaExpr(expr)) =>
            sc.copy(value = SelectionCriteriaExpr(expandFormCtx(expr)))
          case sc @ SelectionCriteria(_, SelectionCriteriaReference(expr, n)) =>
            sc.copy(value = SelectionCriteriaReference(expandFormCtx(expr), n))
          case othewise => othewise
        }
        txt.copy(constraint = constraint.copy(selectionCriteria = Some(updatedSelectionCriteria)))
      case otherwise => otherwise
    }
    scText.copy(value = expandExpr(scText.value))
  }

  private def expandSummaryList(summaryList: MiniSummaryList): MiniSummaryList = {
    val expRows = summaryList.rows
      .map {
        case r @ MiniSummaryRow.ValueRow(_, MiniSummaryListValue.AnyExpr(exp), _) =>
          r.copy(value = MiniSummaryListValue.AnyExpr(expandExpr(exp)))
        case r @ MiniSummaryRow.ValueRow(_, MiniSummaryListValue.Reference(ref), _) =>
          r.copy(value = MiniSummaryListValue.Reference(expandFormCtx(ref)))
        case r @ MiniSummaryRow.HeaderRow(_) => r
      }
      .map {
        case r @ MiniSummaryRow.ValueRow(ss, _, includeIf) =>
          r.copy(key = ss.map(expandSmartString), includeIf = includeIf.map(expandIncludeIf))
        case otherwise => otherwise
      }
    summaryList.copy(rows = expRows)
  }

  private val updated = formComponent.copy(
    includeIf = formComponent.includeIf.map(expandIncludeIf),
    validIf = formComponent.validIf.map(expandValidIf),
    `type` = formComponent.`type` match {
      case t: Text               => expandText(t)
      case t: TextArea           => t.copy(value = expandExpr(t.value))
      case t: HmrcTaxPeriod      => t.copy(idNumber = expandExpr(t.idNumber))
      case t: Choice             => expandChoice(t)
      case t: RevealingChoice    => expandRevealingChoice(t)
      case t: Group              => expandGroup(t)
      case t: InformationMessage => t.copy(infoText = expandSmartString(t.infoText))
      case t: MiniSummaryList    => expandSummaryList(t)
      case otherwise             => otherwise
    },
    label = expandSmartString(formComponent.label),
    helpText = formComponent.helpText.map(expandSmartString),
    shortName = formComponent.shortName.map(expandSmartString),
    errorMessage = formComponent.errorMessage.map(expandSmartString),
    validators = formComponent.validators.map(expandFormComponentValidator),
    instruction = formComponent.instruction.map(i => i.copy(name = i.name.map(expandSmartString)))
  )

  val updatedWithId: FormComponent = updated.copy(id = ExpandUtils.addPrefix(index, formComponent.id))

}
