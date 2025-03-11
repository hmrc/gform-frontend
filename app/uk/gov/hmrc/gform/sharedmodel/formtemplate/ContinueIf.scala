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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.Eq
import play.api.libs.json._
import julienrf.json.derived
import uk.gov.hmrc.gform.eval.BooleanExprResolver

sealed trait ContinueIf {
  def isTerminationPage(booleanExprResolver: BooleanExprResolver): Boolean =
    this match {
      case _: ContinueIf.Continue.type               => false
      case _: ContinueIf.Stop.type                   => true
      case ContinueIf.Conditional(booleanExpression) => !booleanExprResolver.resolve(booleanExpression)
    }
}

object ContinueIf {
  case object Continue extends ContinueIf
  case object Stop extends ContinueIf
  case class Conditional(booleanExpression: BooleanExpr) extends ContinueIf

  implicit val catsEq: Eq[ContinueIf] = Eq.fromUniversalEquals
  implicit val format: OFormat[ContinueIf] = derived.oformat()
}
