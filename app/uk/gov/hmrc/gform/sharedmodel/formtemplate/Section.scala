/*
 * Copyright 2017 HM Revenue & Customs
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

import play.api.libs.json._
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.collection.immutable.List

case class Section(
    title: String,
    description: Option[String],
    shortName: Option[String],
    includeIf: Option[IncludeIf],
    repeatsMax: Option[TextExpression],
    repeatsMin: Option[TextExpression],
    fields: List[FieldValue]
) {

  //TODO remove all logic from that case class representing data
  //TODO move this to repeate service. Case class representing data is not a place for such logic
  def atomicFields(repeatService: RepeatingComponentService)(implicit hc: HeaderCarrier): List[FieldValue] = {

    def atomicFields(fields: List[FieldValue]): List[FieldValue] = {
      fields.flatMap {
        case (fv: FieldValue) => fv.`type` match {
          case groupField @ Group(_, _, _, _, _, _) =>
            atomicFields(repeatService.getAllFieldsInGroup(fv, groupField))
          case _ => List(fv)
        }
      }
    }

    atomicFields(fields)
  }
}

object Section {
  implicit val format = Json.format[Section]
}

case class SectionFormField(
  title: String,
  fields: List[(List[FormField], FieldValue)]
)
