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

import uk.gov.hmrc.gform.models.{ BracketPlain, PageMode, Repeater, Singleton }
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.{ BusinessBankAccountExistence, ValidateBankDetails }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, HmrcRosmRegistrationCheckValidator }

/*
 * Extracts metadata for all expressions of a Page.
 * This doesn't include expressions in fields
 */
object AllPageModelExpressions extends ExprExtractorHelpers {
  def unapply[A <: PageMode](bracket: BracketPlain[A]): Option[List[ExprMetadata]] = {

    def fromSingleton(singleton: Singleton[_]): List[Expr] = {
      val page = singleton.page
      val pageExprs = page.title.interpolations ++ fromOption(
        page.description,
        page.shortName,
        page.progressIndicator,
        page.continueLabel,
        page.instruction.flatMap(_.name)
      )

      val validatorExprs = page.validators.fold(List.empty[Expr]) {
        case HmrcRosmRegistrationCheckValidator(errorMessage, _, utr, postCode) =>
          utr :: postCode :: errorMessage.interpolations
      }

      val dataRetrieveExpressions = page.dataRetrieve.fold(List.empty[Expr]) {
        case ValidateBankDetails(_, sortCode, accountNumber) => List(sortCode, accountNumber)
        case BusinessBankAccountExistence(_, sortCode, accountNumber, companyName) =>
          List(sortCode, accountNumber, companyName)
      }
      pageExprs ++ validatorExprs ++ dataRetrieveExpressions
    }

    def fromRepeater(repeater: Repeater[_]): List[Expr] =
      fromSmartStrings(
        repeater.expandedTitle,
        repeater.expandedDescription,
        repeater.expandedShortName,
        repeater.expandedSummaryName
      )

    def fromNonRepeatingBracket(bracket: BracketPlain.NonRepeatingPage[A]): List[Expr] =
      fromSingleton(bracket.singleton)

    def fromRepeatedBracket(bracket: BracketPlain.RepeatingPage[A]): List[Expr] =
      bracket.source.repeats :: bracket.singletons.toList.flatMap(fromSingleton)

    def fromAddToListBracket(bracket: BracketPlain.AddToList[A]): List[Expr] =
      fromSmartStrings(bracket.source.summaryName) ++ fromOption(
        bracket.source.infoMessage
      ) ++ bracket.iterations.toList
        .flatMap { iteration =>
          iteration.singletons.toList.flatMap(fromSingleton) ::: fromRepeater(iteration.repeater)
        }

    val pageExprs: List[Expr] = bracket.fold(fromNonRepeatingBracket)(fromRepeatedBracket)(fromAddToListBracket)
    val pageExprsMeta = toPlainExprs(pageExprs)

    pageExprsMeta match {
      case Nil => None
      case xs  => Some(xs)
    }
  }
}
