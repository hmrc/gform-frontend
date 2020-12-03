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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.eval.{ RevealingChoiceInfo, StaticTypeInfo, SumInfo }
import uk.gov.hmrc.gform.models.Basic
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils.nelFormat

sealed trait Section extends Product with Serializable {

  def fold[B](f: Section.NonRepeatingPage => B)(g: Section.RepeatingPage => B)(h: Section.AddToList => B): B =
    this match {
      case n: Section.NonRepeatingPage => f(n)
      case r: Section.RepeatingPage    => g(r)
      case a: Section.AddToList        => h(a)
    }

  def getTitle: SmartString = fold(_.page.title)(_.page.title)(_.title)
  def validators: Option[Validator] = fold(_.page.validators)(_.page.validators)(_ => None)
  def progressIndicator: Option[SmartString] = fold(_.page.progressIndicator)(_.page.progressIndicator)(_ => None)
  def continueLabel: Option[SmartString] = fold(_.page.continueLabel)(_.page.continueLabel)(_ => None)
  def isTerminationPage: Boolean = fold(_.page.continueIf.contains(Stop))(_.page.continueIf.contains(Stop))(_ => false)
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
    description: SmartString,
    shortName: SmartString,
    summaryName: SmartString,
    includeIf: Option[IncludeIf],
    repeatsMax: Option[Expr],
    pages: NonEmptyList[Page[Basic]],
    addAnotherQuestion: FormComponent,
    instruction: Option[Instruction],
    presentationHint: Option[PresentationHint]
  ) extends Section {
    val id: AddToListId = AddToListId(addAnotherQuestion.id)
    val allIds: List[FormComponentId] = {
      addAnotherQuestion.id :: pages.toList.flatMap(_.allIds)
    }

    val staticInfo: StaticTypeInfo =
      pages.toList.foldLeft(
        StaticTypeInfo(Map(addAnotherQuestion.baseComponentId -> addAnotherQuestion.staticTypeData))) {
        case (acc, page) => acc ++ page.staticTypeInfo
      }

    val allRevealingChoiceInfo: RevealingChoiceInfo =
      pages.toList.foldLeft(RevealingChoiceInfo.empty)(_ ++ _.revealingChoiceInfo)

    val allSumInfo: SumInfo =
      pages.toList.foldLeft(SumInfo.empty)(_ ++ _.sumInfo)

  }

  implicit val format: OFormat[Section] = derived.oformat()
}

case class DeclarationSection(
  title: SmartString,
  description: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent]
) {
  def toSection = Section.NonRepeatingPage(toPage)

  def toPage: Page[Basic] =
    Page(
      title = title,
      description = description,
      shortName = shortName,
      progressIndicator = None,
      includeIf = None,
      validators = None,
      fields = fields,
      continueLabel = None,
      continueIf = None,
      instruction = None
    )
}

object DeclarationSection {
  implicit val format: OFormat[DeclarationSection] = Json.format[DeclarationSection]
}

case class AcknowledgementSection(
  title: SmartString,
  description: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent],
  showReference: Boolean,
  pdf: Option[AcknowledgementSectionPdf],
  instructionPdf: Option[AcknowledgementSectionPdf],
  displayFeedbackLink: Boolean
) {

  def toSection = Section.NonRepeatingPage(toPage)

  def toPage: Page[Basic] =
    Page(
      title = title,
      description = description,
      shortName = shortName,
      progressIndicator = None,
      includeIf = None,
      validators = None,
      fields = fields,
      continueLabel = None,
      continueIf = None,
      instruction = None
    )
}

object AcknowledgementSection {
  implicit val format: OFormat[AcknowledgementSection] = Json.format[AcknowledgementSection]
}

case class AcknowledgementSectionPdf(header: Option[SmartString], footer: Option[SmartString])

object AcknowledgementSectionPdf {
  implicit val format: OFormat[AcknowledgementSectionPdf] = Json.format[AcknowledgementSectionPdf]
}

case class EnrolmentSection(
  title: SmartString,
  shortName: Option[SmartString],
  fields: List[FormComponent],
  identifiers: NonEmptyList[IdentifierRecipe],
  verifiers: List[VerifierRecipe]
) {
  def toSection = Section.NonRepeatingPage(toPage)

  def toPage: Page[Basic] =
    Page(
      title = title,
      description = None,
      shortName = shortName,
      progressIndicator = None,
      includeIf = None,
      validators = None,
      fields = fields,
      continueLabel = None,
      continueIf = None,
      instruction = None
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
