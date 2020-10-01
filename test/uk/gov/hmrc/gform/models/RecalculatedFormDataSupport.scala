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

package uk.gov.hmrc.gform.models

import cats.instances.list._
import cats.syntax.foldable._
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.VariadicValue.One
import uk.gov.hmrc.gform.sharedmodel.{ VariadicFormData, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics

trait VariadicFormDataSupport {

  private def mkModelComponentId(value: String) = FormComponentId(value).modelComponentId

  def variadicFormDataWithSingleValue(value: String, ids: String*): VariadicFormData[SourceOrigin.OutOfDate] =
    ids.toList.foldMap(id => VariadicFormData.one(mkModelComponentId(id), value))

  def variadicFormData[S <: SourceOrigin](kv: (String, String)*): VariadicFormData[S] =
    kv.toList.foldMap { case (id, v) => VariadicFormData.one(mkModelComponentId(id), v) }

  def variadicFormDataMany(kv: (String, List[Int])*): VariadicFormData[SourceOrigin.OutOfDate] =
    kv.toList.foldMap { case (id, v) => VariadicFormData.many(mkModelComponentId(id), v.map(_.toString)) }

  /* def mkFormDataRecalculated(kv: (String, String)*): FormDataRecalculated = {
 *   val data: Seq[(String, VariadicValue)] = kv.map { case (k, v) => (k, One(v)) }
 *   mkVariadicFormDataRecalculated(data: _*)
 * }
 *
 * def mkVariadicFormDataRecalculated(data: (String, VariadicValue)*): FormDataRecalculated = {
 *   val fcData = data.map { case (k, v) => (FormComponentId(k), v) }
 *   FormDataRecalculated.empty.copy(recData = RecData.fromData(VariadicFormData.create(fcData: _*)))
 * } */
}
