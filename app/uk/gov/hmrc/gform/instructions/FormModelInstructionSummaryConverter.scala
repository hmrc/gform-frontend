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

package uk.gov.hmrc.gform.instructions

import cats.data.NonEmptyList
import play.api.i18n.Messages
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluator, _ }
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.Bracket.AddToList
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ Bracket, Repeater, SingletonWithNumber, Visibility }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, Instruction, Page }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString }
import uk.gov.hmrc.gform.validation.ValidationResult

object FormModelInstructionSummaryConverter {

  sealed trait SummaryData

  sealed trait PageField

  case class SimpleField(label: Option[String], values: List[String]) extends PageField
  case class GroupField(label: Option[String], fields: List[PageField]) extends PageField
  case class ChoiceElement(label: String, fields: List[PageField])
  case class RevealingChoiceField(label: Option[String], choiceElements: List[ChoiceElement]) extends PageField

  case class PageData(title: Option[String], fields: List[PageField], id: String) extends SummaryData

  case class AddToListPageGroup(title: String, pages: List[PageData], id: String)
  case class AddToListSummary(title: String, values: List[String])
  case class AddToListData(title: String, summary: AddToListSummary, pageGroups: List[AddToListPageGroup], id: String)
      extends SummaryData

  implicit val pageOrdering: Ordering[Page[Visibility]] = (x: Page[Visibility], y: Page[Visibility]) =>
    instructionOrderVal(x.instruction).compareTo(instructionOrderVal(y.instruction))

  implicit val bracketOrdering: Ordering[Bracket[Visibility]] = (x: Bracket[Visibility], y: Bracket[Visibility]) =>
    x.fold(a => instructionOrderVal(a.source.page.instruction))(a => instructionOrderVal(a.source.page.instruction))(
        a => instructionOrderVal(a.source.instruction)
      )
      .compareTo(
        y.fold(a => instructionOrderVal(a.source.page.instruction))(a =>
          instructionOrderVal(a.source.page.instruction))(
          a => instructionOrderVal(a.source.instruction)
        ))

  implicit val singletonWithNumberOrdering: Ordering[SingletonWithNumber[Visibility]] =
    (x: SingletonWithNumber[Visibility], y: SingletonWithNumber[Visibility]) =>
      pageOrdering.compare(x.singleton.page, y.singleton.page)

  implicit val fieldOrdering: Ordering[FormComponent] = (x: FormComponent, y: FormComponent) =>
    instructionOrderVal(x.instruction).compareTo(instructionOrderVal(y.instruction))

  def convert[D <: DataOrigin](
    formModelOptics: FormModelOptics[D],
    cache: AuthCacheWithForm,
    envelopeWithMapping: EnvelopeWithMapping,
    validationResult: ValidationResult)(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): List[SummaryData] = {

    val sortedBrackets: List[Bracket[Visibility]] =
      formModelOptics.formModelVisibilityOptics.formModel.brackets.brackets.toList.sorted

    sortedBrackets.flatMap {
      _.fold { nonRepeatingPage =>
        InstructionPDFPageConverter
          .convert(
            nonRepeatingPage.singleton.page,
            nonRepeatingPage.sectionNumber,
            cache,
            envelopeWithMapping,
            validationResult)
          .toList: List[SummaryData]
      } { repeatingPage =>
        repeatingPage.singletons.toList.flatMap {
          case SingletonWithNumber(singleton, sectionNumber) =>
            InstructionPDFPageConverter
              .convert(singleton.page, sectionNumber, cache, envelopeWithMapping, validationResult)
        }
      } { addToList =>
        convertAddToList(cache, envelopeWithMapping, validationResult, addToList)
      }
    }
  }

  private def convertAddToList[D <: DataOrigin](
    cache: AuthCacheWithForm,
    envelopeWithMapping: EnvelopeWithMapping,
    validationResult: ValidationResult,
    addToList: AddToList[Visibility])(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): List[AddToListData] = {
    def addToListTitle(addToList: AddToList[Visibility]): String =
      addToList.source.summaryName.value()

    def addToListSummary(addToList: AddToList[Visibility]): AddToListSummary = {
      val repeaters: NonEmptyList[Repeater[Visibility]] = addToList.repeaters
      val recordTable: NonEmptyList[SmartString] = repeaters.map(_.expandedDescription)
      val values = recordTable.map(_.value()).toList
      AddToListSummary(repeaters.last.title.value(), values)
    }

    val addToListPageGroups: List[AddToListPageGroup] = addToList.iterations.toList.zipWithIndex.flatMap {
      case (iteration, index) =>
        val addToListPages: List[PageData] = iteration.singletons.toList.sorted
          .flatMap {
            case SingletonWithNumber(singleton, sectionNumber) =>
              InstructionPDFPageConverter
                .convert(singleton.page, sectionNumber, cache, envelopeWithMapping, validationResult)
          }
        if (addToListPages.isEmpty)
          None
        else {
          val addToListIterationTitle = iteration.repeater.repeater.expandedShortName.value()
          Some(AddToListPageGroup(addToListIterationTitle, addToListPages, toId(addToListIterationTitle) + index))
        }
    }

    if (addToListPageGroups.isEmpty) {
      List.empty
    } else {
      val title = addToListTitle(addToList)
      List(AddToListData(title, addToListSummary(addToList), addToListPageGroups, toId(title)))
    }
  }

  private def instructionOrderVal(i: Option[Instruction]): Int = i.flatMap(_.order).getOrElse(Integer.MAX_VALUE)

  private def toId(value: String) = value.replaceAll("\\W", "")
}
