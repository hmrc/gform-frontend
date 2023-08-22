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

package uk.gov.hmrc.gform.eval

import cats.syntax.eq._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.graph.processor.UserCtxEvaluatorProcessor
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

// This is used to evaluate if expressions from exitPages before user's form data are retrieved,
// mainly to exit form for agents before they are asked for access code
class NoFormEvaluation(retrievals: MaterialisedRetrievals, authConfig: AuthConfig) {

  def evalIncludeIf(includeIf: IncludeIf): Boolean =
    evalBooleanExpr(includeIf.booleanExpr)

  private def evalBooleanExpr(be: BooleanExpr): Boolean =
    be match {
      case Equals(l, r)                                   => evalExpr(l) === evalExpr(r)
      case And(l, r)                                      => evalBooleanExpr(l) && evalBooleanExpr(r)
      case Or(l, r)                                       => evalBooleanExpr(l) || evalBooleanExpr(r)
      case Not(be)                                        => !evalBooleanExpr(be)
      case IsLogin(value)                                 => BooleanExprEval.evalIsLoginExpr(value, retrievals)
      case DateAfter(DateValueExpr(l), DateValueExpr(r))  => l.toLocalDate.isAfter(r.toLocalDate)
      case DateBefore(DateValueExpr(l), DateValueExpr(r)) => l.toLocalDate.isBefore(r.toLocalDate)
      case IsTrue                                         => true
      case _                                              => false
    }

  private def evalExpr(e: Expr): String = e match {
    case UserCtx(userField) =>
      UserCtxEvaluatorProcessor.processEvaluation(
        retrievals,
        userField,
        authConfig
      )
    case Constant(c) => c
    case _           => ""
  }
}
