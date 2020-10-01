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

package uk.gov.hmrc.gform.sharedmodel.graph

import cats.Eq
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

sealed trait GraphNode extends Product with Serializable

object GraphNode {
  implicit val equal: Eq[GraphNode] = Eq.fromUniversalEquals
  implicit val equalExpr: Eq[GraphNode.Expr] = Eq.fromUniversalEquals
  case class Simple(formComponentId: FormComponentId) extends GraphNode
  case class Expr(expr: uk.gov.hmrc.gform.sharedmodel.formtemplate.Expr) extends GraphNode
}
