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

package uk.gov.hmrc.bforms.core

import julienrf.json.derived
import play.api.libs.json.OFormat

/**
  * Created by dimitra on 04/04/17.
  */
sealed trait BeforeOrAfter
case object After extends BeforeOrAfter
case object Before extends BeforeOrAfter

object BeforeOrAfter {
  implicit val formatExpr: OFormat[BeforeOrAfter] = derived.oformat[BeforeOrAfter]
}

sealed trait DateFormat
case object Today extends DateFormat
case class AnyDate(year: Int, month: Int, day: Int) extends DateFormat
case class NextDate(month: Int, day: Int) extends DateFormat
case class PreviousDate(month: Int, day: Int) extends DateFormat
case class AnyWord(value: String) extends DateFormat

object DateFormat {
  implicit val formatExpr: OFormat[DateFormat] = derived.oformat[DateFormat]
}

sealed trait OffsetFormat
case class OffsetDate(value: Int) extends OffsetFormat

object OffsetFormat {
  implicit val formatExpr: OFormat[OffsetFormat] = derived.oformat[OffsetFormat]
}

sealed trait FormatExpr

final case class TextExpression(value: String) extends FormatExpr
final case class DateExpression(beforeOrAfter: BeforeOrAfter, dateFormat: DateFormat, offsetFormat: OffsetFormat) extends FormatExpr
final case object GeneralDate extends FormatExpr
final case object AnyOtherWord extends FormatExpr

object FormatExpr {
  implicit val format: OFormat[FormatExpr] = derived.oformat
}
