/*
 * Copyright 2018 HM Revenue & Customs
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

import cats.instances.future._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.gform.PrepopService
import uk.gov.hmrc.gform.sharedmodel.formtemplate.BooleanExprEval

class GraphModule(
  authModule: AuthModule
) {
  private val prepopService = new PrepopService(authModule.eeittService)

  private val evaluator: Evaluator[Future] = new Evaluator[Future](prepopService.eeittPrepop)
  val booleanExprEval: BooleanExprEval[Future] = new BooleanExprEval(evaluator)
  val recalculation: Recalculation[Future, Throwable] =
    new Recalculation(booleanExprEval, ((s: GraphException) => new IllegalArgumentException(s.reportProblem)))

}
