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

package uk.gov.hmrc.gform.tasklist

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.graph.GraphModule
import uk.gov.hmrc.gform.models.{ ProcessDataService, TaxPeriodStateChecker }
import uk.gov.hmrc.gform.validation.ValidationModule

class TaskListModule(
  configModule: ConfigModule,
  validationModule: ValidationModule,
  gformBackendModule: GformBackendModule,
  graphModule: GraphModule
)(implicit
  ec: ExecutionContext
) {

  val taxPeriodStateChecker = new TaxPeriodStateChecker[Future, Throwable] {
    def error: Throwable = new Exception("Call to des to retrieve obligation-data has failed")
  }

  val processDataService: ProcessDataService[Future] =
    new ProcessDataService[Future](graphModule.recalculation, taxPeriodStateChecker)

  val taskListRenderingService = new TaskListRenderingService(
    configModule.frontendAppConfig,
    validationModule.validationService,
    gformBackendModule.gformConnector,
    processDataService
  )

}
