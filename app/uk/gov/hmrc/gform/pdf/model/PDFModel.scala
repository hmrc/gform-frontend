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

package uk.gov.hmrc.gform.pdf.model

import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluator, _ }
import uk.gov.hmrc.gform.models.{ Bracket, SingletonWithNumber }
import uk.gov.hmrc.gform.recalculation.FreeCalculator
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import cats.syntax.option._
import play.twirl.api.Html

trait PDFModel {
  sealed trait SummaryData

  sealed trait PageField

  case class SimpleField(label: Option[String], values: List[Html]) extends PageField
  case class ChoiceField(label: Option[String], values: List[Html]) extends PageField
  case class GroupField(label: Option[String], fields: List[PageField]) extends PageField
  case class ChoiceElement(label: String, fields: List[PageField])
  case class RevealingChoiceField(
    label: Option[String],
    choiceElements: List[ChoiceElement],
    isSeparate: Boolean
  ) extends PageField

  case class PageData(title: Option[String], fields: List[PageField], id: String) extends SummaryData

  case class AddToListPageGroup(title: String, pages: List[PageData], id: String)
  case class AddToListSummary(title: String, values: List[String])
  case class AddToListData(title: String, summary: AddToListSummary, pageGroups: List[AddToListPageGroup], id: String)
      extends SummaryData

  object IsGroupField {
    def unapply(pageField: PageField): Option[GroupField] = pageField match {
      case SimpleField(_, _)             => None
      case ChoiceField(_, _)             => None
      case g @ GroupField(_, _)          => Some(g)
      case RevealingChoiceField(_, _, _) => None
    }
  }

  case class HeaderFooter(header: Option[SmartString], footer: Option[SmartString])

  case class Options(tabularFormat: Option[Boolean], includeSignatureBox: Option[Boolean])
}

object PDFModel extends PDFModel

sealed trait PDFType

object PDFType {
  trait Instruction extends PDFType
  trait Summary extends PDFType
}

sealed trait PDFLayout
object PDFLayout {
  case object Default extends PDFLayout
  case object Tabular extends PDFLayout
}

trait PDFCustomRender[A] {

  val layout: PDFLayout

  val pageOrdering: Option[Ordering[Page]] = None
  val bracketOrdering: Option[Ordering[Bracket]] = None
  val singletonWithNumberOrdering: Option[Ordering[SingletonWithNumber]] = None
  val formComponentOrdering: Option[Ordering[FormComponent]] = None

  def getPageTitle(page: Page, maybePresentationHint: Option[PresentationHint])(implicit
    lise: SmartStringEvaluator
  ): Option[String]
  def getFormComponentLabel(formComponent: FormComponent)(implicit lise: SmartStringEvaluator): Option[String]
  def doFilter(fields: List[FormComponent], freeCalculator: FreeCalculator): List[FormComponent]
}

object PDFCustomRender {

  implicit val summaryPDFCustomRenderInstance: PDFCustomRender[PDFType.Summary] =
    new PDFCustomRender[PDFType.Summary] {

      override val layout: PDFLayout = PDFLayout.Default

      override def getPageTitle(page: Page, maybePresentationHint: Option[PresentationHint])(implicit
        lise: SmartStringEvaluator
      ): Option[String] =
        maybePresentationHint match {
          case Some(InvisiblePageTitle) => None
          case _ =>
            page.shortName.orElse(Some(page.title)).flatMap { s =>
              val evaluated = s.value()
              if (evaluated.isEmpty) None else Some(evaluated)
            }
        }

      override def getFormComponentLabel(formComponent: FormComponent)(implicit
        lise: SmartStringEvaluator
      ): Option[String] =
        formComponent.shortName.map(_.value()).orElse(Some(formComponent.label.value()))

      override def doFilter(
        fields: List[FormComponent],
        freeCalculator: FreeCalculator
      ): List[FormComponent] = fields.filterNot(_.hideOnSummary(freeCalculator))
    }

  implicit val instructionPDFCustomRenderInstance: PDFCustomRender[PDFType.Instruction] =
    new PDFCustomRender[PDFType.Instruction] {

      override val layout: PDFLayout = PDFLayout.Tabular

      private def instructionOrderVal(i: Option[Instruction]): Int = i.flatMap(_.order).getOrElse(Integer.MAX_VALUE)

      override val pageOrdering: Option[Ordering[Page]] = new Ordering[Page] {
        override def compare(x: Page, y: Page) =
          instructionOrderVal(x.instruction).compareTo(instructionOrderVal(y.instruction))
      }.some

      override val bracketOrdering: Option[Ordering[Bracket]] = new Ordering[Bracket] {
        override def compare(x: Bracket, y: Bracket) =
          x.fold(a => instructionOrderVal(a.source.page.instruction))(a =>
            instructionOrderVal(a.source.page.instruction)
          )(a => instructionOrderVal(a.source.instruction))
            .compareTo(
              y.fold(a => instructionOrderVal(a.source.page.instruction))(a =>
                instructionOrderVal(a.source.page.instruction)
              )(a => instructionOrderVal(a.source.instruction))
            )
      }.some

      override val singletonWithNumberOrdering: Option[Ordering[SingletonWithNumber]] =
        new Ordering[SingletonWithNumber] {
          override def compare(x: SingletonWithNumber, y: SingletonWithNumber) =
            instructionOrderVal(x.singleton.page.instruction)
              .compareTo(instructionOrderVal(y.singleton.page.instruction))
        }.some

      override val formComponentOrdering: Option[Ordering[FormComponent]] = new Ordering[FormComponent] {
        override def compare(x: FormComponent, y: FormComponent) =
          instructionOrderVal(x.instruction).compareTo(instructionOrderVal(y.instruction))
      }.some

      override def getPageTitle(page: Page, maybePresentationHint: Option[PresentationHint])(implicit
        lise: SmartStringEvaluator
      ): Option[String] =
        page.instruction.flatMap(_.name.map(_.value()))

      override def getFormComponentLabel(formComponent: FormComponent)(implicit
        lise: SmartStringEvaluator
      ): Option[String] =
        formComponent.instruction.flatMap(_.name.map(_.value()))

      override def doFilter(
        fields: List[FormComponent],
        freeCalculator: FreeCalculator
      ): List[FormComponent] = fields
        .filter(f => !f.hideOnSummary(freeCalculator) && f.instruction.isDefined)
    }
}
