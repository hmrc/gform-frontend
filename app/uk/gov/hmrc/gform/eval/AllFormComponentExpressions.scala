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

package uk.gov.hmrc.gform.eval

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SelectionCriteriaValue._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

/*
 * Extracts metadata for all expressions of FormComponent.
 * This assume that RevealingChoice's and Group's fields (ie. nested FormComponents) are being expanded.
 */
object AllFormComponentExpressions extends ExprExtractorHelpers {
  def unapply(fc: FormComponent): Option[List[ExprMetadata]] = {

    def fromRcElements(revealingChoiceElements: List[RevealingChoiceElement]): List[ExprMetadata] =
      revealingChoiceElements.toList.flatMap { case RevealingChoiceElement(choice, _, _, _) =>
        // We don't need to do anything about revealingFields, since they are expanded, see FormModelExpander[DependencyGraphVerification]
        toPlainExprs(choice.interpolations)
      }

    def fromGroup(group: Group): List[ExprMetadata] = {
      // We don't need to do anything about fields, since they are expanded, see FormModelExpander[DependencyGraphVerification]
      val Group(_, _, _, repeatLabel, repeatAddAnotherText) = group

      val exprs = fromOption(repeatLabel, repeatAddAnotherText)
      toPlainExprs(exprs)
    }

    def fromNel(nel: NonEmptyList[SmartString]): List[Expr] = nel.toList.flatMap(_.interpolations)

    val fcExprs: List[Expr] =
      fc.label.interpolations ++
        fromOption(fc.helpText, fc.shortName, fc.instruction.flatMap(_.name))

    val fcSelfRefferingExprs: List[Expr] =
      fromOption(fc.errorMessage) ++
        fc.validators.flatMap(_.errorMessage.interpolations)

    def fcLookupExpr(selectionCriteria: Option[List[SelectionCriteria]]): List[ExprMetadata] =
      selectionCriteria.fold(List.empty[ExprMetadata]) { selectionCriterias =>
        toPlainExprs(
          selectionCriterias collect {
            case SelectionCriteria(_, SelectionCriteriaExpr(expr))         => expr
            case SelectionCriteria(_, SelectionCriteriaReference(expr, _)) => expr
          }
        )
      }

    val componentTypeExprs: List[ExprMetadata] = fc match {
      case IsText(Text(Lookup(_, sc), _, _, _, _, _))     => fcLookupExpr(sc)
      case IsGroup(group)                                 => fromGroup(group)
      case IsRevealingChoice(RevealingChoice(options, _)) => fromRcElements(options)
      case IsChoice(Choice(_, options, _, _, hints, optionHelpText)) =>
        toPlainExprs(
          fromNel(options),
          hints.fold(List.empty[Expr])(fromNel),
          optionHelpText.fold(List.empty[Expr])(fromNel)
        )
      case IsInformationMessage(InformationMessage(_, infoText)) =>
        toPlainExprs(infoText.interpolations)
      case HasExpr(expr) => toPlainExprs(expr :: Nil)
      case _             => Nil
    }

    val allExprs: List[ExprMetadata] =
      toPlainExprs(fcExprs) ++
        toSelfReferringExprs(fc.id, fcSelfRefferingExprs) ++
        componentTypeExprs

    Some(allExprs).filter(_.nonEmpty)
  }
}
