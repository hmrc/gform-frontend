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
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

/*
 * Extracts metadata for all expressions of FormComponent.
 * This assume that RevealingChoice's and Group's fields (ie. nested FormComponents) are being expanded.
 */
object AllFormComponentExpressions extends ExprExtractorHelpers {
  def unapply(fc: FormComponent): Option[List[ExprMetadata]] = {

    def toFirstOperandExprs(exprs: List[Expr], toExprMetadata: InferrableExpr => ExprMetadata): List[ExprMetadata] =
      exprs.map(expr => toExprMetadata(InferrableExpr(expr, InferringRule.FirstOperand)))

    def fromRcElements(revealingChoiceElements: List[RevealingChoiceElement]): List[ExprMetadata] =
      revealingChoiceElements.toList.flatMap {
        case RevealingChoiceElement(choice, _, _) =>
          // We don't need to do anything about revealingFields, since they are expanded, see FormModelExpander[DependencyGraphVerification]
          toFirstOperandPlainExprs(choice.interpolations)
      }

    def fromGroup(group: Group): List[ExprMetadata] = {
      // We don't need to do anything about fields, since they are expanded, see FormModelExpander[DependencyGraphVerification]
      val Group(_, _, _, repeatLabel, repeatAddAnotherText) = group

      val exprs = fromOption(repeatLabel, repeatAddAnotherText)
      toFirstOperandPlainExprs(exprs)
    }

    def fromNel(nel: NonEmptyList[SmartString]): List[Expr] = nel.toList.flatMap(_.interpolations)

    val fcExprs: List[Expr] =
      fc.label.interpolations ++
        fromOption(fc.helpText, fc.shortName)

    val fcSelfRefferingExprs: List[Expr] =
      fromOption(fc.errorMessage) ++
        fc.validators.flatMap(_.errorMessage.interpolations)

    val componentTypeExprs: List[ExprMetadata] = fc match {
      case IsGroup(group)                                 => fromGroup(group)
      case IsRevealingChoice(RevealingChoice(options, _)) => fromRcElements(options)
      case IsChoice(Choice(_, options, _, _, optionHelpText)) =>
        toFirstOperandPlainExprs(fromNel(options), optionHelpText.fold(List.empty[Expr])(fromNel))
      case IsInformationMessage(InformationMessage(_, infoText)) =>
        toFirstOperandPlainExprs(infoText.interpolations)
      case _ => Nil
    }

    val firstOperandExprs: List[ExprMetadata] =
      toFirstOperandPlainExprs(fcExprs) ++
        toFirstOperandExprs(fcSelfRefferingExprs, ExprMetadata.SelfReferring(_, fc.id)) ++
        componentTypeExprs

    val explicitExprs = fc match {
      case HasExpr(expr) =>
        Some(ExprMetadata.Plain(InferrableExpr(expr, InferringRule.Explicit(fc.id))))
      case _ => None
    }

    (firstOperandExprs, explicitExprs) match {
      case (Nil, None)       => None
      case (xs, None)        => Some(xs)
      case (xs, Some(exprs)) => Some(exprs :: xs)
    }
  }
}
