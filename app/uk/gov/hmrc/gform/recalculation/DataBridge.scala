/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.recalculation

import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.DataRetrieveId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormCtx }

trait DataBridge {
  def name: String // For debugging purposes
  def valueValue: EvaluationStatus
  def evalFormCtx(formComponentId: FormComponentId, behaviour: Behaviour): EvaluationStatus
  def maybeIndex(formComponentId: FormComponentId): Option[Int]
  def liftDataRetrieveId(id: DataRetrieveId): DataRetrieveId
  def insideAtl(formCtx: FormCtx): Boolean
  def outsideAtl(formCtx: FormCtx): Boolean
  def allModelComponentIds(modelComponentId: ModelComponentId): List[(ModelComponentId, EvaluationStatus)]
}
