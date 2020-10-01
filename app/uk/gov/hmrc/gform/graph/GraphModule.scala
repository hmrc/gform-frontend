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

import java.util.concurrent.TimeUnit

import cats.Id
import cats.instances.future._
import cats.syntax.show._

import scala.concurrent.{ Await, ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.eval.{ BooleanExprEval, DbLookupChecker, DelegatedEnrolmentChecker }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.eval.SeissEligibilityChecker
import uk.gov.hmrc.gform.eval.smartstring.{ RealSmartStringEvaluatorFactory, SmartStringEvaluatorFactory }
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.typeclasses.identityThrowableMonadError

class GraphModule(
  authModule: AuthModule,
  gformBackendModule: GformBackendModule
)(
  implicit ec: ExecutionContext
) {

  val seissEligibilityChecker = new SeissEligibilityChecker(
    authModule.selfEmployedIncomeSupportEligibilityConnector.eligibilityStatus)

  val delegatedEnrolmentCheckStatus =
    new DelegatedEnrolmentChecker(authModule.delegatedEnrolmentService.checkDelegatedEnrolment)

  val dbLookupCheckStatus = new DbLookupChecker(gformBackendModule.gformConnector.dbLookup)

  private val graphErrorHandler = (s: GraphException) => new IllegalArgumentException(s.reportProblem)

  val recalculation: Recalculation[Future, Throwable] =
    new Recalculation(seissEligibilityChecker, delegatedEnrolmentCheckStatus, dbLookupCheckStatus, graphErrorHandler)

  val smartStringEvaluatorFactory: SmartStringEvaluatorFactory =
    new RealSmartStringEvaluatorFactory()

  val booleanExprEval: BooleanExprEval[Future] =
    new BooleanExprEval(seissEligibilityChecker)

}
