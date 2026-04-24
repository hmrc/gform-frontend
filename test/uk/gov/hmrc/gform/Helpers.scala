/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormComponentId }

object Helpers {
  def mkData(fields: (String, String)*): VariadicFormData =
    fields.foldLeft(VariadicFormData.empty) { case (acc, (fcId, value)) =>
      acc addOne (FormComponentId(fcId).modelComponentId -> value)
    }

  def mkDataMany(fields: (String, Seq[String])*): VariadicFormData =
    fields.foldLeft(VariadicFormData.empty) { case (acc, (fcId, value)) =>
      acc addMany (FormComponentId(fcId).modelComponentId -> value)
    }

  def toLocalisedString(string: String): LocalisedString =
    LocalisedString(Map(LangADT.En -> string))

  def toLocalisedString(string: Option[String]): Option[LocalisedString] = string.map(s => toLocalisedString(s))

  def toSmartString(string: String): SmartString =
    SmartString(toLocalisedString(string), Nil)

  def toSmartStringExpression(string: String, expression: Expr): SmartString =
    SmartString(toLocalisedString(string), expression :: Nil)

  def toSmartString(stringEn: String, stringCy: String): SmartString =
    SmartString(LocalisedString(Map(LangADT.En -> stringEn, LangADT.Cy -> stringCy)), Nil)

}
