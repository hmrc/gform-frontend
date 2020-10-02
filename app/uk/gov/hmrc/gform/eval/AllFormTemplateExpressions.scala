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

package uk.gov.hmrc.gform.eval

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.models.Basic
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, PrintSection }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.{ DestinationList, DestinationPrint }

/*
 * Extracts metadata for all expressions of FormTemplate.
 * This doesn't include expressions in sections
 */
object AllFormTemplateExpressions extends ExprExtractorHelpers {
  def apply(formTemplate: FormTemplate): List[ExprMetadata] = {
    val emailExprs: List[Expr] = fromOptionF(formTemplate.emailParameters)(_.toList.map(_.value))
    val summarySectionExprs: List[Expr] = {
      val SummarySection(title, header, footer) = formTemplate.summarySection
      fromSmartStrings(title, header, footer)
    }

    def fromPage(page: Page[Basic]): List[ExprMetadata] =
      page.fields.flatMap(AllFormComponentExpressions.unapply).flatten ++
        toFirstOperandPlainExprs(
          fromSmartStrings(page.title),
          fromOption(
            page.description,
            page.shortName
          ))

    def fromAuth: List[ExprMetadata] = formTemplate.authConfig match {
      case HasEnrolmentSection(_, enrolmentSection, _, _) =>
        fromPage(enrolmentSection.toPage) ++
          toFirstOperandPlainExprs(
            enrolmentSection.identifiers.map(_.value).toList,
            enrolmentSection.verifiers.map(_.value)
          )
      case _ => Nil
    }

    def fromDestinationList(destinationList: DestinationList): List[ExprMetadata] = {

      def fromDestination(destination: Destination): List[Expr] = destination match {
        case d: Destination.HmrcDms                => d.customerId :: Nil
        case d: Destination.SubmissionConsolidator => d.customerId :: Nil
        case d: Destination.HandlebarsHttpApi      => Nil
        case d: Destination.Composite              => d.destinations.toList.flatMap(fromDestination)
        case d: Destination.StateTransition        => Nil
        case d: Destination.Log                    => Nil
        case d: Destination.Email                  => Nil
      }

      def fromDestinations(destinations: NonEmptyList[Destination]): List[ExprMetadata] =
        toFirstOperandPlainExprs(destinations.toList.flatMap(fromDestination))
      def fromDeclarationSection(declaration: DeclarationSection): List[ExprMetadata] = fromPage(declaration.toPage)
      def fromAcknowledgementSection(acknowledgement: AcknowledgementSection): List[ExprMetadata] =
        fromPage(acknowledgement.toPage)

      fromDestinations(destinationList.destinations) ++
        fromDeclarationSection(destinationList.declarationSection) ++
        fromAcknowledgementSection(destinationList.acknowledgementSection)
    }
    def fromDestinationPrint(destinationPrint: DestinationPrint): List[ExprMetadata] = {
      def fromPage(page: PrintSection.Page): List[Expr] = fromSmartStrings(page.title, page.instructions)
      def fromPdf(pdf: PrintSection.Pdf): List[Expr] = fromSmartStrings(pdf.header, pdf.footer)
      def fromPdfNotification(pdf: PrintSection.PdfNotification): List[Expr] = fromSmartStrings(pdf.header, pdf.footer)
      val DestinationPrint(page, pdf, pdfNotification) = destinationPrint
      toFirstOperandPlainExprs(
        fromPage(page),
        fromPdf(pdf),
        fromOptionF(pdfNotification)(fromPdfNotification)
      )
    }

    val destinationsExprs: List[ExprMetadata] =
      formTemplate.destinations.fold(fromDestinationList)(fromDestinationPrint)

    toFirstOperandPlainExprs(emailExprs, summarySectionExprs) ++ destinationsExprs ++ fromAuth
  }
}
