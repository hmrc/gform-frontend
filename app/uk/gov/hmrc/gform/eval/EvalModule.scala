/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.eval

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule

class EvalModule(
  authModule: AuthModule,
  gformBackendModule: GformBackendModule
)(implicit
  ec: ExecutionContext
) {
  private val seissEligibilityChecker = new SeissEligibilityChecker(
    authModule.selfEmployedIncomeSupportEligibilityConnector.eligibilityStatus
  )

  private val delegatedEnrolmentCheckStatus =
    new DelegatedEnrolmentChecker(authModule.delegatedEnrolmentService.checkDelegatedEnrolment)

  private val dbLookupCheckStatus = new DbLookupChecker(gformBackendModule.gformConnector.dbLookup)

  val refreshBooleanExprCacheService: RefreshBooleanExprCacheService[Future] =
    new RefreshBooleanExprCacheService(seissEligibilityChecker, delegatedEnrolmentCheckStatus, dbLookupCheckStatus)

}
