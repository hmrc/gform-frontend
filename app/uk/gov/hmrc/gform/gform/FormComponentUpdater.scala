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

package uk.gov.hmrc.gform.gform

import cats.instances.int._
import cats.syntax.eq._
import uk.gov.hmrc.gform.models.{ AddToListUtils, ExpandUtils, PageMode }
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class FormComponentUpdater(formComponent: FormComponent, index: Int, baseIds: List[FormComponentId]) {

  private def expandExpr(expr: Expr): Expr = ExprUpdater(expr, index, baseIds)

  private def expandBooleanExpr(booleanExpr: BooleanExpr): BooleanExpr = BooleanExprUpdater(booleanExpr, index, baseIds)

  private def expandeRevealingChoice(revealingChoice: RevealingChoice): RevealingChoice =
    revealingChoice.copy(options = revealingChoice.options.map { revealingChoiceElement =>
      revealingChoiceElement.copy(
        choice = expandSmartString(revealingChoiceElement.choice),
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
    options = choice.options.map(expandSmartString),
    optionHelpText = choice.optionHelpText.map(_.map(expandSmartString))
  )

  private def expandValidId(validIf: ValidIf) = ValidIf(expandBooleanExpr(validIf.booleanExpr))

  private def expandFormComponentValidator(formComponentValidator: FormComponentValidator) =
    formComponentValidator.copy(
      validIf = expandValidId(formComponentValidator.validIf),
      errorMessage = expandSmartString(formComponentValidator.errorMessage)
    )

  private def expandSmartString(smartString: SmartString) = smartString.expand(index, baseIds)

  private val updated = formComponent.copy(
    validIf = formComponent.validIf.map(expandValidId),
    `type` = formComponent.`type` match {
      case t: Text               => t.copy(value = expandExpr(t.value))
      case t: TextArea           => t.copy(value = expandExpr(t.value))
      case t: UkSortCode         => t.copy(value = expandExpr(t.value))
      case t: HmrcTaxPeriod      => t.copy(idNumber = expandExpr(t.idNumber))
      case t: Choice             => expandChoice(t)
      case t: RevealingChoice    => expandeRevealingChoice(t)
      case t: Group              => expandGroup(t)
      case t: InformationMessage => t.copy(infoText = expandSmartString(t.infoText))
      case otherwise             => otherwise
    },
    label = expandSmartString(formComponent.label),
    helpText = formComponent.helpText.map(expandSmartString),
    shortName = formComponent.shortName.map(expandSmartString),
    errorMessage = formComponent.errorMessage.map(expandSmartString),
    validators = formComponent.validators.map(expandFormComponentValidator)
  )

  val updatedWithId: FormComponent = updated.copy(id = ExpandUtils.addPrefix(index, formComponent.id))

}
