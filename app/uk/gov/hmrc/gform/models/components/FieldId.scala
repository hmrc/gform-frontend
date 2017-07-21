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

import play.api.Logger
import play.api.libs.json._
import uk.gov.hmrc.gform.models.ValueClassFormat

case class FieldId(value: String) extends AnyVal {
  override def toString = value

  def withSuffix(suffix: String): FieldId = {
    if (validate(value) && validate(suffix))
      FieldId(value + "." + suffix)
    else {
      Logger.debug(s"Illegal Value : $value, Illegal Suffix : $suffix")
      throw new IllegalArgumentException
    }
  }

  def validate(str: String): Boolean = {
    val Reg = "[.-]".r.unanchored
    str match {
      case Reg() => false
      case _ => true
    }
  }

  def withJSSafeSuffix(suffix: String): FieldId = {
    if (validate(value) && validate(suffix))
      FieldId(value + "-" + suffix)
    else {
      Logger.debug(s"Illegal Value : $value, Illegal Suffix : $suffix")
      throw new IllegalArgumentException
    }
  }

  def toJsSuffix = {
    FieldId(value.replace(".", "-"))
  }

  def toSuffix = {
    FieldId(value.replace("-", "."))
  }

  def getSuffix(replacement: FieldId) =
    if (value.contains("-"))
      value.replace(replacement + "-", "")
    else
      value.replace(replacement + ".", "")

}

object FieldId {
  implicit val format: Format[FieldId] = ValueClassFormat.format(FieldId.apply)(_.value)
}
