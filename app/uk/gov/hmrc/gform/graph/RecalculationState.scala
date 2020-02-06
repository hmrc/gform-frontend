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

package uk.gov.hmrc.gform.graph

import uk.gov.hmrc.gform.eval.EvaluationResults
import uk.gov.hmrc.gform.sharedmodel.BooleanExprCache
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DataSource

class RecalculationState(
  val evaluationResults: EvaluationResults,
  val booleanExprCache: BooleanExprCache
) {

  def update(newEvaluationResults: EvaluationResults): RecalculationState =
    new RecalculationState(newEvaluationResults, booleanExprCache)

  def get(dataSource: DataSource, value: String): Option[Boolean] = booleanExprCache.get(dataSource, value)
  def add(dataSource: DataSource, value: String, result: Boolean): RecalculationState = new RecalculationState(
    evaluationResults,
    booleanExprCache.add(dataSource, value, result)
  )
}
