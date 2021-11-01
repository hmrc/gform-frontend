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

package uk.gov.hmrc.gform.gform

import uk.gov.hmrc.gform.models.PageMode
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BooleanExpr, Confirmation, FormComponentId, IncludeIf, Page, Validator }

class PageUpdater[A <: PageMode](page: Page[A], index: Int, baseIds: List[FormComponentId]) {

  private def expandBooleanExpr(booleanExpr: BooleanExpr): BooleanExpr = BooleanExprUpdater(booleanExpr, index, baseIds)

  private def expandIncludeIf(includeIf: IncludeIf) = IncludeIf(expandBooleanExpr(includeIf.booleanExpr))

  private def expandValidator(validator: Validator) = ValidatorUpdater(validator, index, baseIds)

  private def expandDataRetrieve(dataRetrieve: DataRetrieve) = DataRetrieveUpdater(dataRetrieve, index, baseIds)

  private def expandSmartString(smartString: SmartString) = smartString.expand(index, baseIds)

  private def expandConfirmation(confirmation: Confirmation) = confirmation.copy(
    question = new FormComponentUpdater(confirmation.question, index, baseIds).updatedWithId,
    pageId = confirmation.pageId.withIndex(index)
  )

  def updated: Page[A] =
    page.copy(
      title = expandSmartString(page.title),
      id = page.id.map(id => id.withIndex(index)),
      description = page.description.map(expandSmartString),
      shortName = page.shortName.map(expandSmartString),
      progressIndicator = page.progressIndicator.map(expandSmartString),
      includeIf = page.includeIf.map(expandIncludeIf),
      fields = page.fields.map { field =>
        new FormComponentUpdater(field, index, baseIds).updatedWithId
      },
      validators = page.validators.map(expandValidator),
      continueLabel = page.continueLabel.map(expandSmartString),
      instruction = page.instruction.map(i => i.copy(name = i.name.map(expandSmartString))),
      dataRetrieve = page.dataRetrieve.map(expandDataRetrieve),
      confirmation = page.confirmation.map(expandConfirmation)
    )
}

object PageUpdater {
  def apply[A <: PageMode](page: Page[A], index: Int, baseIds: List[FormComponentId]): Page[A] =
    new PageUpdater[A](page, index, baseIds).updated
}
