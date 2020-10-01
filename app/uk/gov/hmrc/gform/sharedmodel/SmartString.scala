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

package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json.{ Format, Json }
import uk.gov.hmrc.gform.models.ExpandUtils
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormComponentId }

case class SmartString(localised: LocalisedString, interpolations: List[Expr]) {
  def replace(toReplace: String, replaceWith: String): SmartString =
    copy(localised = localised.replace(toReplace, replaceWith))

  def rawValue(implicit l: LangADT): String = localised.value(l)

  def isEmpty(implicit l: LangADT): Boolean = rawValue.isEmpty

  def expand(index: Int, baseIds: List[FormComponentId]) = ExpandUtils.expandSmartString(this, index, baseIds)
}

object SmartString {
  val empty: SmartString = SmartString(LocalisedString.empty, Nil)

  implicit val format: Format[SmartString] = Json.format[SmartString]
}
