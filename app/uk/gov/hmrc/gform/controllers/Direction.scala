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
import uk.gov.hmrc.gform.models.{ ExpandUtils, FormModel }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.BadRequestException

trait Navigation {

  def formModelOptics: FormModelOptics[DataOrigin.Browser]

  lazy val availableSectionNumbers: List[SectionNumber] =
    formModelOptics.formModelVisibilityOptics.formModel.availableSectionNumbers

  lazy val minSectionNumber: SectionNumber = availableSectionNumbers.min(Ordering.by((_: SectionNumber).value))
}

// TODO: Origin should not be in controllers, but Navigator probably should!
case class Origin(formModelOptics: FormModelOptics[DataOrigin.Browser]) extends Navigation {
  //def data[S <: SourceOrigin] = sssdata
}

sealed trait Direction

case object SaveAndContinue extends Direction
case class Back(sectionNumber: SectionNumber) extends Direction
case object SaveAndExit extends Direction
case class AddGroup(modelComponentId: ModelComponentId) extends Direction
case class RemoveGroup(modelComponentId: ModelComponentId) extends Direction
case class RemoveAddToList(idx: Int, addToListId: AddToListId) extends Direction
case class EditAddToList(idx: Int, addToListId: AddToListId) extends Direction

case class Navigator(
  sectionNumber: SectionNumber,
  requestRelatedData: RequestRelatedData,
  formModelOptics: FormModelOptics[DataOrigin.Browser]
) extends Navigation {
  require(
    sectionNumber >= minSectionNumber,
    s"section number is too low: ${sectionNumber.value} is not >= $minSectionNumber")
  require(
    sectionNumber <= maxSectionNumber,
    s"section number is too big: ${sectionNumber.value} is not <= $maxSectionNumber")

  val AddGroupR = "AddGroup-(.*)".r.unanchored
  val RemoveGroupR = "RemoveGroup-(.*)".r.unanchored
  val RemoveAddToListR = "RemoveAddToList-(\\d*)-(.*)".r.unanchored
  val EditAddToListR = "EditAddToList-(\\d*)-(.*)".r.unanchored

  def navigate: Direction = actionValue match {
    case "Save"                   => SaveAndExit
    case "Continue"               => SaveAndContinue
    case "Back"                   => Back(previousOrCurrentSectionNumber)
    case AddGroupR(x)             => AddGroup(ExpandUtils.toModelComponentId(x))
    case RemoveGroupR(x)          => RemoveGroup(ExpandUtils.toModelComponentId(x))
    case RemoveAddToListR(idx, x) => RemoveAddToList(idx.toInt, AddToListId(FormComponentId(x)))
    case EditAddToListR(idx, x)   => EditAddToList(idx.toInt, AddToListId(FormComponentId(x)))
    case other                    => throw new BadRequestException(s"Invalid action: $other")
  }

  private def actionValue: String = requestRelatedData.get("save")

  private lazy val maxSectionNumber: SectionNumber = availableSectionNumbers.max(Ordering.by((_: SectionNumber).value))

  private val previousOrCurrentSectionNumber: SectionNumber =
    availableSectionNumbers.reverse
      .find(_ < sectionNumber)
      .getOrElse(sectionNumber)

}
