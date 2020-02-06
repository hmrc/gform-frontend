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

import uk.gov.hmrc.gform.models.PageMode
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BooleanExpr, FormComponentId, IncludeIf, Page, Validator }

class PageUpdater[A <: PageMode](page: Page[A], index: Int, baseIds: List[FormComponentId]) {

  private def expandBooleanExpr(booleanExpr: BooleanExpr): BooleanExpr = BooleanExprUpdater(booleanExpr, index, baseIds)

  private def expandIncludeId(includeIf: IncludeIf) = IncludeIf(expandBooleanExpr(includeIf.booleanExpr))

  private def expandValidator(validator: Validator) = ValidatorUpdater(validator, index, baseIds)

  private def expandSmartString(smartString: SmartString) = smartString.expand(index, baseIds)

  def updated: Page[A] = page.copy(
    title = expandSmartString(page.title),
    description = page.description.map(expandSmartString),
    shortName = page.shortName.map(expandSmartString),
    progressIndicator = page.progressIndicator.map(expandSmartString),
    includeIf = page.includeIf.map(expandIncludeId),
    fields = page.fields.map { field =>
      new FormComponentUpdater(field, index, baseIds).updatedWithId
    },
    validators = page.validators.map(expandValidator),
    continueLabel = page.continueLabel.map(expandSmartString)
  )

}

object PageUpdater {
  def apply[A <: PageMode](page: Page[A], index: Int, baseIds: List[FormComponentId]): Page[A] =
    new PageUpdater[A](page, index, baseIds).updated
}
