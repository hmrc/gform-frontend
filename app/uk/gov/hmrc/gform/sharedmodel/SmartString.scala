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

package uk.gov.hmrc.gform.sharedmodel

import cats.instances.string._
import cats.syntax.eq._
import play.api.libs.json.{ JsError, JsObject, JsResult, JsString, JsSuccess, JsValue, OFormat, Reads }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, OFormatWithTemplateReadFallback }

case class SmartString(localised: LocalisedString, interpolations: List[Expr]) {
  def replace(toReplace: String, replaceWith: String): SmartString =
    copy(localised = localised.replace(toReplace, replaceWith))

  def rawValue(implicit l: LangADT): String = localised.value(l)
}

object SmartString {
  val empty: SmartString = SmartString(LocalisedString.empty, Nil)

  private def interpolationsFieldName = "interpolations"

  private def readFromTemplateString(value: String) =
    JsSuccess(SmartString(LocalisedString(Map(LangADT.En -> value)), Nil))

  private def readLocalisedStringFromTemplateObject(value: Map[String, JsValue]) =
    LocalisedString.format.reads(JsObject(value.filterKeys(_ =!= interpolationsFieldName)))

  private def readInterpolationsFromTemplateObject(value: Map[String, JsValue]) =
    value
      .get(interpolationsFieldName)
      .map { a =>
        Reads.of[List[Expr]].reads(a)
      }
      .getOrElse(JsSuccess(Nil))

  private def readFromTemplateObject(value: Map[String, JsValue]) =
    for {
      localisedString <- readLocalisedStringFromTemplateObject(value)
      interpolations  <- readInterpolationsFromTemplateObject(value)
    } yield SmartString(localisedString, interpolations)

  private val templateReads: Reads[SmartString] = new Reads[SmartString] {
    override def reads(json: JsValue): JsResult[SmartString] = json match {
      case JsString(value) => readFromTemplateString(value)
      case obj: JsObject   => readFromTemplateObject(obj.fields.toMap)
      case _               => JsError(s"Expected a String or an Object while reading a SmartString. Got $json")
    }
  }

  implicit val format: OFormat[SmartString] = OFormatWithTemplateReadFallback(templateReads)
}
