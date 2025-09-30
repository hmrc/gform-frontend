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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.eval.{ ExprType, RevealingChoiceInfo, StaticTypeData, StaticTypeInfo, SumInfo }
import uk.gov.hmrc.gform.models.Basic
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils.nelFormat
import uk.gov.hmrc.gform.sharedmodel.formtemplate.LayoutDisplayWidth.LayoutDisplayWidth

sealed trait Section extends Product with Serializable {

  def fold[B](f: Section.NonRepeatingPage => B)(g: Section.RepeatingPage => B)(h: Section.AddToList => B): B =
    this match {
      case n: Section.NonRepeatingPage => f(n)
      case r: Section.RepeatingPage    => g(r)
      case a: Section.AddToList        => h(a)
    }

  def getTitle: SmartString = fold(_.page.title)(_.page.title)(_.title)
  def getCaption: Option[SmartString] = fold(_.page.caption)(_.page.caption)(_ => None)
  def continueLabel: Option[SmartString] = fold(_.page.continueLabel)(_.page.continueLabel)(_ => None)
  def staticTypeInfo: StaticTypeInfo = fold(_.page.staticTypeInfo)(_.page.staticTypeInfo)(_.staticInfo)
  def revealingChoiceInfo: RevealingChoiceInfo =
    fold(_.page.revealingChoiceInfo)(_.page.revealingChoiceInfo)(_.allRevealingChoiceInfo)
  def sumInfo: SumInfo = fold(_.page.sumInfo)(_.page.sumInfo)(_.allSumInfo)

}

object Section {
  case class NonRepeatingPage(page: Page[Basic]) extends Section

  case class RepeatingPage(page: Page[Basic], repeats: Expr) extends Section {
    val allIds: List[FormComponentId] = page.allIds
  }

  case class AddToList(
    title: SmartString,
    caption: Option[SmartString],
    noPIITitle: Option[SmartString],
    description: AtlDescription,
    summaryDescription: SmartString,
    shortName: SmartString,
    summaryName: SmartString,
    includeIf: Option[IncludeIf],
    pages: NonEmptyList[Page[Basic]],
    repeatsUntil: Option[IncludeIf],
    repeatsWhile: Option[IncludeIf],
    repeaterContinueLabel: Option[SmartString],
    addAnotherQuestion: FormComponent,
    instruction: Option[Instruction],
    presentationHint: Option[PresentationHint],
    infoMessage: Option[SmartString],
    errorMessage: Option[SmartString],
    descriptionTotal: Option[AtlDescription.KeyValueBased],
    defaultPage: Option[Page[Basic]] = None,
    cyaPage: Option[CheckYourAnswersPage] = None,
    fields: Option[NonEmptyList[FormComponent]] = None,
    pageIdToDisplayAfterRemove: Option[PageId] = None,
    notRequiredIf: Option[IncludeIf],
    declarationSection: Option[DeclarationSection] = None,
    displayWidth: Option[LayoutDisplayWidth] = None,
    removePageContent: Option[SmartString] = None
  ) extends Section {
    val pageId: PageId = PageId(addAnotherQuestion.id.value)
    val id: AddToListId = AddToListId(addAnotherQuestion.id)
    val allPagesIds: List[FormComponentId] = pages.toList.flatMap(_.allIds)
    val allIds: List[FormComponentId] = {
      addAnotherQuestion.id :: allPagesIds
    }
    val atlMap: Map[FormComponentId, FormComponentId] =
      allPagesIds.map(fcId => (fcId, addAnotherQuestion.id)).toMap

    val addToListTypeInfo = StaticTypeInfo(
      Map(addAnotherQuestion.baseComponentId -> StaticTypeData(ExprType.number, None))
    )

    val staticInfo: StaticTypeInfo =
      pages.toList.foldLeft(addToListTypeInfo) { case (acc, page) =>
        acc ++ page.staticTypeInfo
      }

    val allRevealingChoiceInfo: RevealingChoiceInfo =
      pages.toList.foldLeft(RevealingChoiceInfo.empty)(_ ++ _.revealingChoiceInfo)

    val allSumInfo: SumInfo =
      pages.toList.foldLeft(SumInfo.empty)(_ ++ _.sumInfo)

  }

  implicit val format: OFormat[Section] = derived.oformat()
}

sealed trait AtlDescription extends Product with Serializable

object AtlDescription {
  case class SmartStringBased(value: SmartString) extends AtlDescription
  case class KeyValueBased(key: SmartString, value: SmartString) extends AtlDescription

  implicit val format: OFormat[AtlDescription] = derived.oformat()
  implicit val formatKv: OFormat[KeyValueBased] = derived.oformat()
}

case class DeclarationSection(
  title: SmartString,
  caption: Option[SmartString],
  noPIITitle: Option[SmartString],
  description: Option[SmartString],
  shortName: Option[SmartString],
  continueLabel: Option[SmartString],
  fields: List[FormComponent],
  includeIf: Option[IncludeIf]
) {
  def toSection = Section.NonRepeatingPage(toPage)

  def toPage: Page[Basic] =
    Page(
      title = title,
      id = None,
      noPIITitle = noPIITitle,
      description = description,
      shortName = shortName,
      caption = caption,
      includeIf = None,
      fields = fields,
      continueLabel = continueLabel,
      continueIf = None,
      instruction = None,
      presentationHint = None,
      dataRetrieve = None,
      confirmation = None,
      redirects = None,
      hideSaveAndComeBackButton = None,
      removeItemIf = None,
      displayWidth = None,
      notRequiredIf = None,
      specimenNote = None
    )
}

object DeclarationSection {
  implicit val format: OFormat[DeclarationSection] = Json.format[DeclarationSection]
}

case class AcknowledgementSection(
  title: Option[SmartString],
  description: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent],
  showReference: Boolean,
  pdf: Option[PdfCxt],
  instructionPdf: Option[PdfCxt],
  displayFeedbackLink: Boolean,
  noPIITitle: Option[SmartString],
  showBanner: Boolean
) {

  def toSection = Section.NonRepeatingPage(toPage)

  def toPage: Page[Basic] =
    Page(
      title = title.getOrElse(SmartString.empty),
      id = None,
      noPIITitle = noPIITitle,
      description = description,
      shortName = shortName,
      caption = None,
      includeIf = None,
      fields = fields,
      continueLabel = None,
      continueIf = None,
      instruction = None,
      presentationHint = None,
      dataRetrieve = None,
      confirmation = None,
      redirects = None,
      hideSaveAndComeBackButton = None,
      removeItemIf = None,
      displayWidth = None,
      notRequiredIf = None,
      specimenNote = None
    )
}

object AcknowledgementSection {
  implicit val format: OFormat[AcknowledgementSection] = Json.format[AcknowledgementSection]
}

case class EnrolmentSection(
  title: SmartString,
  noPIITitle: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent],
  identifiers: NonEmptyList[IdentifierRecipe],
  verifiers: List[VerifierRecipe],
  continueLabel: Option[SmartString]
) {
  def toSection = Section.NonRepeatingPage(toPage)

  def toPage: Page[Basic] =
    Page(
      title = title,
      id = None,
      noPIITitle = noPIITitle,
      description = None,
      shortName = shortName,
      caption = None,
      includeIf = None,
      fields = fields,
      continueLabel = continueLabel,
      continueIf = None,
      instruction = None,
      presentationHint = None,
      dataRetrieve = None,
      confirmation = None,
      redirects = None,
      hideSaveAndComeBackButton = None,
      removeItemIf = None,
      displayWidth = None,
      notRequiredIf = None,
      specimenNote = None
    )
}

object EnrolmentSection {
  import JsonUtils._
  implicit val format: OFormat[EnrolmentSection] = Json.format[EnrolmentSection]
}

case class IdentifierRecipe(key: String, value: FormCtx)
object IdentifierRecipe {
  implicit val format: OFormat[IdentifierRecipe] = Json.format[IdentifierRecipe]
}

case class VerifierRecipe(key: String, value: FormCtx)
object VerifierRecipe {
  implicit val format: OFormat[VerifierRecipe] = Json.format[VerifierRecipe]
}

final case class EnrolmentOutcomes(
  notMatchedPage: EnrolmentOutcome,
  alreadyLinkedPage: EnrolmentOutcome,
  technicalFailurePage: EnrolmentOutcome,
  successPage: EnrolmentOutcome,
  insufficientCredentialsPage: EnrolmentOutcome
)

object EnrolmentOutcomes {
  implicit val format: OFormat[EnrolmentOutcomes] = Json.format[EnrolmentOutcomes]
}

final case class EnrolmentOutcome(
  title: SmartString,
  content: SmartString
)

object EnrolmentOutcome {
  implicit val format: OFormat[EnrolmentOutcome] = Json.format[EnrolmentOutcome]
}
