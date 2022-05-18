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

package uk.gov.hmrc.gform.sharedmodel
import play.api.libs.json.Format
import play.api.libs.functional.syntax._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils

case class LocalisedString(m: Map[LangADT, String]) {
  def value(implicit l: LangADT): String = m.getOrElse(l, m.getOrElse(LangADT.En, ""))
  def replace(toReplace: String, replaceWith: String): LocalisedString =
    copy(m = (m.map { case (lang, message) => (lang, message.replace(toReplace, replaceWith)) }))
  def uncapitalize(): LocalisedString =
    copy(m = m.map { case (lang, message) => (lang, message.splitAt(1) match { case (c, cs) => c.toLowerCase + cs }) })
}

object LocalisedString {
  val empty: LocalisedString = LocalisedString(Map.empty)

  val formatMap: Format[Map[LangADT, String]] =
    JsonUtils.formatMap(LangADT.stringToLangADT, LangADT.langADTToString)
  implicit val format: Format[LocalisedString] =
    formatMap.inmap[LocalisedString](m => LocalisedString(m), ls => ls.m)
}
