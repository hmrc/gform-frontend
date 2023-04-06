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

package uk.gov.hmrc.gform.graph

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import uk.gov.hmrc.gform.sharedmodel.graph.GraphNode

case class GraphData(graphTopologicalOrder: Iterable[(Int, List[GraphNode])], graph: Graph[GraphNode, DiEdge])

object GraphData {
  val empty: GraphData = GraphData(List.empty[(Int, List[GraphNode])], Graph.empty)
}
