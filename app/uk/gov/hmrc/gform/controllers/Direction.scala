/*
 * Copyright 2018 HM Revenue & Customs
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

import play.api.Logger
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.BadRequestException

trait Navigation {
  def sections: List[Section]
  def data: Map[FormComponentId, Seq[String]]
  def retrievals: Retrievals

  lazy val availableSectionNumbers: List[SectionNumber] = {
    def shouldInclude(s: Section): Boolean = {
      val isIncludedExpression = s.includeIf.map(_.expr).getOrElse(IsTrue)
      BooleanExpr.isTrue(isIncludedExpression, data, retrievals)
    }

    sections
      .zipWithIndex
      .filter(si => shouldInclude(si._1))
      .map(si => SectionNumber(si._2))
  }

  lazy val minSectionNumber: SectionNumber = availableSectionNumbers.min(Ordering.by((_: SectionNumber).value))
}

// TODO: Origin should not be in controllers, but Navigator probably should!
case class Origin(sections: List[Section], retrievals: Retrievals) extends Navigation {
  val data: Map[FormComponentId, Seq[String]] = Map.empty
}

sealed trait Direction

case class SaveAndContinue(sectionNumber: SectionNumber) extends Direction
case class Back(sectionNumber: SectionNumber) extends Direction
object SaveAndExit extends Direction
object SaveAndSummary extends Direction
object BackToSummary extends Direction
case class AddGroup(groupId: String) extends Direction
case class RemoveGroup(idx: Int, groupId: String) extends Direction

case class Navigator(sectionNumber: SectionNumber, sections: List[Section], data: Map[FormComponentId, Seq[String]], retrievals: Retrievals) extends Navigation {
  require(sectionNumber >= minSectionNumber, s"section number is to big: ${sectionNumber.value}")
  require(sectionNumber <= maxSectionNumber, s"section number is to low: ${sectionNumber.value}")

  val RemoveGroupR = "RemoveGroup-(\\d*)_(.*)".r.unanchored

  def navigate: Direction = actionValue match {
    // format: OFF
    case "Save"                               => SaveAndExit
    case "Continue" if isLastSectionNumber    => SaveAndSummary
    case "Continue" if !isLastSectionNumber   => SaveAndContinue(nextSectionNumber)
    case "Back"                               => Back(previousOrCurrentSectionNumber)
    case "BackToSummary"                      => BackToSummary
    case  x if x.startsWith("AddGroup")       => AddGroup(x)
    case  RemoveGroupR(idx, x)                => RemoveGroup(idx.toInt, x)
    case other                                => throw new BadRequestException(s"Invalid action: $other")
    // format: ON
  }

  private def actionValue: String = {
    val fieldId = FormComponentId("save")
    FormDataHelpers.get(data, fieldId).headOption
      .getOrElse(
        throw new BadRequestException(s"Missing '${fieldId.value}' form field")
      )
  }

  private lazy val maxSectionNumber: SectionNumber = availableSectionNumbers.max(Ordering.by((_: SectionNumber).value))
  private lazy val isLastSectionNumber: Boolean = sectionNumber == maxSectionNumber

  private lazy val nextSectionNumber: SectionNumber =
    availableSectionNumbers
      .find(_ > sectionNumber)
      .get //This supposed to be called only if the section is findable

  private val previousOrCurrentSectionNumber: SectionNumber =
    availableSectionNumbers
      .reverse
      .find(_ < sectionNumber)
      .getOrElse(sectionNumber)

}
