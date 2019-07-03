/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.form

import cats.Semigroup
import cats.syntax.eq._
import com.softwaremill.quicklens._
import play.api.libs.json._
import uk.gov.hmrc.gform.graph.{ Data, RecData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, Section }
import uk.gov.hmrc.gform.sharedmodel.graph.{ GraphNode, IncludeIfGN, SimpleGN }

case class FormDataRecalculated(invisible: Set[GraphNode], recData: RecData) {

  val data = recData.data // ToDo JoVl Rename to recalculatedData

  def isVisible(section: Section): Boolean =
    !invisible.exists {
      case SimpleGN(_)               => false
      case IncludeIfGN(_, includeIf) => section.includeIf.exists(_ === includeIf)
    }
}

object FormDataRecalculated {
  val empty = FormDataRecalculated(Set.empty, RecData.empty)

  def clearTaxResponses(data: FormDataRecalculated): FormDataRecalculated =
    data
      .modify(_.recData.data)
      .setTo(data.recData.cleared)
}

case class FormData(fields: Seq[FormField]) extends AnyVal {
  def toData: Data = fields.map(x => x.id -> List(x.value)).toMap
  def find(id: FormComponentId): Option[String] = fields.find(_.id === id).map(_.value)
}

object FormData {

  implicit val semigroup: Semigroup[FormData] = new Semigroup[FormData] {
    def combine(x: FormData, y: FormData): FormData = FormData(x.fields ++ y.fields)
  }

  implicit val format: OFormat[FormData] = Json.format[FormData]
}
