/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData

trait VariadicFormDataSupport {

  private def mkModelComponentId(value: String) = FormComponentId(value).modelComponentId

  def variadicFormDataWithSingleValue(value: String, ids: String*): VariadicFormData[SourceOrigin.OutOfDate] =
    ids.toList.foldMap(id => VariadicFormData.one(mkModelComponentId(id), value))

  def variadicFormData[S <: SourceOrigin](kv: (String, String)*): VariadicFormData[S] =
    kv.toList.foldMap { case (id, v) => VariadicFormData.one(mkModelComponentId(id), v) }

  def variadicFormDataMany(kv: (String, List[Int])*): VariadicFormData[SourceOrigin.OutOfDate] =
    kv.toList.foldMap { case (id, v) => VariadicFormData.many(mkModelComponentId(id), v.map(_.toString)) }

  def mkVariadicFormData[T <: SourceOrigin](data: (String, VariadicValue)*): VariadicFormData[T] = {
    val fcData = data.map { case (k, v) => (mkModelComponentId(k), v) }
    VariadicFormData.create(fcData: _*)
  }
}
