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

package uk.gov.hmrc.gform.models.optics

sealed trait DataOrigin extends Product with Serializable
object DataOrigin {
  def swapDataOrigin[M[_ <: DataOrigin]](x: M[Browser]): M[Mongo] = x.asInstanceOf[M[Mongo]]
  def unSwapDataOrigin[M[_ <: DataOrigin]](x: M[Mongo]): M[Browser] = x.asInstanceOf[M[Browser]]
  trait Mongo extends DataOrigin
  trait Browser extends DataOrigin
  trait Artificial extends DataOrigin
}
