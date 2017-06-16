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

package uk.gov.hmrc.gform.models.components

import play.api.libs.json._
import uk.gov.hmrc.gform.models.ValueClassFormat

case class FieldId(value: String) extends AnyVal {
  override def toString = value

  def withSuffix(suffix: String): FieldId = FieldId(value + "." + suffix)

  def withJSSafeSuffix(suffix: String): FieldId = FieldId(value + "-" + suffix)

  def getSuffix(replacement: FieldId) = value.replace(replacement + ".", "")
}

object FieldId {
  implicit val format: Format[FieldId] = ValueClassFormat.format(FieldId.apply)(_.value)
}
