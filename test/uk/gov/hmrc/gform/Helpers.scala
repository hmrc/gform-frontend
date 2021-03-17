/*
 * Copyright 2021 HM Revenue & Customs
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

import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormComponentId }

object Helpers {
  def mkData[A <: SourceOrigin](fields: (String, String)*): VariadicFormData[A] =
    fields.foldLeft(VariadicFormData.empty[A]) { case (acc, (fcId, value)) =>
      acc addOne (FormComponentId(fcId).modelComponentId -> value)
    }

  def mkDataMany[A <: SourceOrigin](fields: (String, Seq[String])*): VariadicFormData[A] =
    fields.foldLeft(VariadicFormData.empty[A]) { case (acc, (fcId, value)) =>
      acc addMany (FormComponentId(fcId).modelComponentId -> value)
    }

  def mkDataCurrent(fields: (String, String)*): VariadicFormData[SourceOrigin.Current] =
    mkData[SourceOrigin.Current](fields: _*)

  def mkDataOutOfDate(fields: (String, String)*): VariadicFormData[SourceOrigin.OutOfDate] =
    mkData[SourceOrigin.OutOfDate](fields: _*)

  def mkDataManyCurrent(fields: (String, String)*): VariadicFormData[SourceOrigin.Current] =
    mkDataMany[SourceOrigin.Current](fields.map(stringToSeq): _*)

  def mkDataManyOutOfDate(fields: (String, String)*): VariadicFormData[SourceOrigin.OutOfDate] =
    mkDataMany[SourceOrigin.OutOfDate](fields.map(stringToSeq): _*)

  private def stringToSeq(t: (String, String)): (String, Seq[String]) = {
    val (a, b) = t
    (a, b.split(","))
  }

  def toLocalisedString(string: String): LocalisedString =
    LocalisedString(Map(LangADT.En -> string))

  def toLocalisedString(string: Option[String]): Option[LocalisedString] = string.map(s => toLocalisedString(s))

  def toSmartString(string: String): SmartString =
    SmartString(toLocalisedString(string), Nil)

  def toSmartStringExpression(string: String, expression: Expr): SmartString =
    SmartString(toLocalisedString(string), expression :: Nil)

}
