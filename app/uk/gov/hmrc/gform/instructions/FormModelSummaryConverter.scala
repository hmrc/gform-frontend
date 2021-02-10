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
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ Bracket, Repeater, Singleton, SingletonWithNumber, Visibility }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString }
import uk.gov.hmrc.gform.validation.ValidationResult

object FormModelSummaryConverter {

  sealed trait SummaryData

  sealed trait PageField

  case class SimpleField(label: String, values: List[String], errors: Set[String]) extends PageField
  case class GroupField(label: String, fields: List[PageField], errors: Set[String]) extends PageField
  case class ChoiceElement(label: String, fields: List[PageField])
  case class RevealingChoiceField(label: String, choiceElements: List[ChoiceElement]) extends PageField

  case class PageData(title: Option[String], fields: List[PageField]) extends SummaryData

  case class AddToListPageGroup(title: String, pages: List[PageData])
  case class AddToListSummary(title: String, values: List[String])
  case class AddToListData(title: String, summary: AddToListSummary, pageGroups: List[AddToListPageGroup])
      extends SummaryData

  def convert[D <: DataOrigin](
    formModelOptics: FormModelOptics[D],
    cache: AuthCacheWithForm,
    envelope: Envelope,
    validationResult: ValidationResult)(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): List[SummaryData] = {
    val formModel = formModelOptics.formModelVisibilityOptics.formModel

    def instructionOrder(bracket: Bracket[Visibility]): Int =
      bracket
        .fold(_.source.page.instruction.flatMap(_.order))(_.source.page.instruction.flatMap(_.order))(
          _.source.instruction.flatMap(_.order))
        .getOrElse(Integer.MAX_VALUE)

    val sortedBrackets: List[Bracket[Visibility]] = formModel.brackets.brackets.toList.sortBy(instructionOrder)
    sortedBrackets.flatMap {
      _.fold { nonRepeatingPage =>
        List[SummaryData](
          mapSingleton(nonRepeatingPage.singleton, nonRepeatingPage.sectionNumber, cache, envelope, validationResult))
      } { repeatingPage =>
        repeatingPage.singletons.toList.map {
          case SingletonWithNumber(singleton, sectionNumber) =>
            mapSingleton(singleton, sectionNumber, cache, envelope, validationResult)
        }
      } { addToList =>
        def addToListTitle(addToList: Bracket.AddToList[Visibility]): String =
          addToList.source.summaryName.value()

        def addToListSummary(addToList: Bracket.AddToList[Visibility]): AddToListSummary = {
          val repeaters: NonEmptyList[Repeater[Visibility]] = addToList.repeaters
          val recordTable: NonEmptyList[SmartString] = repeaters.map(_.expandedDescription)
          val values = recordTable.map(_.value()).toList
          AddToListSummary(repeaters.last.title.value(), values)
        }

        val addToListPageGroups: List[AddToListPageGroup] = addToList.iterations.toList.flatMap { iteration =>
          val addToListPages: List[PageData] = iteration.singletons.toList.map {
            case SingletonWithNumber(singleton, sectionNumber) =>
              mapSingleton(singleton, sectionNumber, cache, envelope, validationResult)
          }
          if (addToListPages.isEmpty)
            None
          else
            Some(AddToListPageGroup(iteration.repeater.repeater.expandedShortName.value(), addToListPages))
        }

        if (addToListPageGroups.isEmpty)
          List.empty
        else
          List(AddToListData(addToListTitle(addToList), addToListSummary(addToList), addToListPageGroups))
      }
    }
  }

  def mapSingleton(
    singleton: Singleton[Visibility],
    sectionNumber: SectionNumber,
    cache: AuthCacheWithForm,
    envelope: Envelope,
    validationResult: ValidationResult)(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): PageData = {
    val pageTitle = singleton.page.instruction.flatMap(_.name).map(_.value())
    val pageFields =
      singleton.page.fields.map(c => mapFormComponent(c, cache, sectionNumber, validationResult, envelope))
    PageData(pageTitle, pageFields)
  }

  def mapFormComponent(
    component: FormComponent,
    cache: AuthCacheWithForm,
    sectionNumber: SectionNumber,
    validationResult: ValidationResult,
    envelope: Envelope)(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): PageField = {
    import InstructionsPDFPageFieldConverters._
    (component.`type` match {
      case _: Text =>
        PageFieldConverter[Text]
      case _: TextArea =>
        PageFieldConverter[TextArea]
      case _: UkSortCode =>
        PageFieldConverter[UkSortCode]
      case _: Date =>
        PageFieldConverter[Date]
      case _: Time =>
        PageFieldConverter[Time]
      case _: InformationMessage =>
        PageFieldConverter[InformationMessage]
      case _: FileUpload =>
        PageFieldConverter[FileUpload]
      case _: HmrcTaxPeriod =>
        PageFieldConverter[HmrcTaxPeriod]
      case _: Choice =>
        PageFieldConverter[Choice]
      case _: RevealingChoice =>
        PageFieldConverter[RevealingChoice]
      case _: Group =>
        PageFieldConverter[Group]
      case _: Address =>
        PageFieldConverter[Address]
    }).convert(component, cache, sectionNumber, validationResult, envelope)
  }
}
