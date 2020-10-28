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
import cats.syntax.eq._
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.models.Basic
import uk.gov.hmrc.gform.sharedmodel.SmartString

import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils.nelFormat

sealed trait Section extends Product with Serializable {
  def getTitle: SmartString = this match {
    case s: Section.NonRepeatingPage => s.page.title
    case s: Section.RepeatingPage    => s.page.title
    case s: Section.AddToList        => s.title
  }

  def validators: Option[Validator] = this match {
    case s: Section.NonRepeatingPage => s.page.validators
    case s: Section.RepeatingPage    => s.page.validators
    case s: Section.AddToList        => None
  }

  def progressIndicator: Option[SmartString] = this match {
    case s: Section.NonRepeatingPage => s.page.progressIndicator
    case s: Section.RepeatingPage    => s.page.progressIndicator
    case s: Section.AddToList        => None
  }

  def continueLabel: Option[SmartString] = this match {
    case s: Section.NonRepeatingPage => s.page.continueLabel
    case s: Section.RepeatingPage    => s.page.continueLabel
    case s: Section.AddToList        => None
  }

  def isRepeating: Boolean = this match {
    case s: Section.NonRepeatingPage => false
    case s: Section.RepeatingPage    => true
    case s: Section.AddToList        => false
  }

  def isTerminationPage: Boolean = this match {
    case s: Section.NonRepeatingPage => s.page.continueIf.contains(Stop)
    case s: Section.RepeatingPage    => s.page.continueIf.contains(Stop)
    case s: Section.AddToList        => false
  }

  def addToList: Option[Section.AddToList] =
    fold[Option[Section.AddToList]](_ => None)(_ => None)(addToList => Some(addToList))

  def byAddToListId(addToListId: AddToListId): Boolean = fold(_ => false)(_ => false)(_.id === addToListId)

  def fold[B](f: Section.NonRepeatingPage => B)(g: Section.RepeatingPage => B)(h: Section.AddToList => B): B =
    this match {
      case n: Section.NonRepeatingPage => f(n)
      case r: Section.RepeatingPage    => g(r)
      case a: Section.AddToList        => h(a)
    }

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
    includeIf: Option[IncludeIf],
    repeatsMax: Option[Expr],
    pages: NonEmptyList[Page[Basic]],
    addAnotherQuestion: FormComponent,
    instruction: Option[Instruction]
  ) extends Section {
    val id: AddToListId = AddToListId(addAnotherQuestion.id)
    val allIds: List[FormComponentId] = {
      addAnotherQuestion.id :: pages.toList.flatMap(_.allIds)
    }
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
  instructionPdf: Option[AcknowledgementSectionPdf]
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
