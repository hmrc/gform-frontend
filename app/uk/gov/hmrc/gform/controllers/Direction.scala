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

package uk.gov.hmrc.gform.controllers

import cats.syntax.show._
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.BadRequestException

trait Navigation {
  def sections: List[Section]
  def data: FormDataRecalculated

  lazy val availableSectionNumbers: List[SectionNumber] = {
    sections.zipWithIndex.collect {
      case (section, index) if data.isVisible(section) => SectionNumber(index)
    }
  }

  lazy val minSectionNumber: SectionNumber = availableSectionNumbers.min(Ordering.by((_: SectionNumber).value))
}

// TODO: Origin should not be in controllers, but Navigator probably should!
case class Origin(sections: List[Section], val data: FormDataRecalculated) extends Navigation

sealed trait Direction

case object SaveAndContinue extends Direction
case class Back(sectionNumber: SectionNumber) extends Direction
case object SaveAndExit extends Direction
case class AddGroup(groupId: String) extends Direction
case class RemoveGroup(idx: Int, groupId: String) extends Direction

case class Navigator(
  sectionNumber: SectionNumber,
  sections: List[Section],
  data: FormDataRecalculated
) extends Navigation {
  require(
    sectionNumber >= minSectionNumber,
    s"section number is too low: ${sectionNumber.value} is not >= $minSectionNumber")
  require(
    sectionNumber <= maxSectionNumber,
    s"section number is too big: ${sectionNumber.value} is not <= $maxSectionNumber")

  val RemoveGroupR = "RemoveGroup-(\\d*)_(.*)".r.unanchored

  def navigate: Direction = actionValue match {
    case "Save"                        => SaveAndExit
    case "Continue"                    => SaveAndContinue
    case "Back"                        => Back(previousOrCurrentSectionNumber)
    case x if x.startsWith("AddGroup") => AddGroup(x)
    case RemoveGroupR(idx, x)          => RemoveGroup(idx.toInt, x)
    case other                         => throw new BadRequestException(s"Invalid action: $other")
  }

  private val actionValueFieldId = FormComponentId("save")
  private def actionValue: String =
    data.data
      .oneOrElse(actionValueFieldId, throw new BadRequestException(show"Missing '$actionValueFieldId' form field"))

  private lazy val maxSectionNumber: SectionNumber = availableSectionNumbers.max(Ordering.by((_: SectionNumber).value))

  private val previousOrCurrentSectionNumber: SectionNumber =
    availableSectionNumbers.reverse
      .find(_ < sectionNumber)
      .getOrElse(sectionNumber)

}
