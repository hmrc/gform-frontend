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

package uk.gov.hmrc.gform.eval

import uk.gov.hmrc.gform.models.{ Bracket, PageMode, Repeater, SingletonWithNumber }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AtlDescription, Expr }

/*
 * Extracts metadata for all expressions of a Page.
 * This doesn't include expressions in fields
 */
object AllPageModelExpressions extends ExprExtractorHelpers {
  def unapply[A <: PageMode](bracket: Bracket[A]): Option[List[ExprMetadata]] = {

    def fromSingleton(singleton: SingletonWithNumber[_]): List[Expr] = {
      val page = singleton.singleton.page
      val pageExprs = page.title.allInterpolations ++ fromOption(
        page.description,
        page.shortName,
        page.caption,
        page.continueLabel,
        page.instruction.flatMap(_.name)
      )

      val dataRetrieveExpressions = page.dataRetrieves().foldRight(List.empty[Expr]) { case (dataRetrieve, acc) =>
        dataRetrieve.params.map(_.expr) ++ acc
      }

      pageExprs ++ dataRetrieveExpressions
    }

    def fromRepeater(repeater: Repeater[_]): List[Expr] =
      fromSmartStrings(
        repeater.expandedTitle,
        repeater.expandedShortName,
        repeater.expandedSummaryName
      ) ++ (repeater.expandedDescription match {
        case AtlDescription.SmartStringBased(ss) => fromSmartStrings(ss)
        case AtlDescription.KeyValueBased(k, v)  => fromSmartStrings(k) ++ fromSmartStrings(v)
      }) ++ (repeater.expandedDescriptionTotal match {
        case Some(kvBased) => fromSmartStrings(kvBased.key) ++ fromSmartStrings(kvBased.value)
        case None          => None
      })

    def fromNonRepeatingBracket(bracket: Bracket.NonRepeatingPage[A]): List[Expr] =
      fromSingleton(bracket.singleton)

    def fromRepeatedBracket(bracket: Bracket.RepeatingPage[A]): List[Expr] =
      bracket.source.repeats :: bracket.singletons.toList.flatMap(fromSingleton)

    def fromAddToListBracket(bracket: Bracket.AddToList[A]): List[Expr] =
      fromSmartStrings(
        bracket.source.summaryName,
        bracket.source.addAnotherQuestion.label,
        bracket.source.summaryDescription
      ) ++ fromOption(
        bracket.source.infoMessage,
        bracket.source.addAnotherQuestion.helpText
      ) ++ bracket.iterations.toList.flatMap { iteration =>
        iteration.singletons.toList.flatMap(fromSingleton) ::: fromRepeater(iteration.repeater.repeater)
      }

    val pageExprs: List[Expr] = bracket.fold(fromNonRepeatingBracket)(fromRepeatedBracket)(fromAddToListBracket)
    val pageExprsMeta = toPlainExprs(pageExprs)

    pageExprsMeta match {
      case Nil => None
      case xs  => Some(xs)
    }
  }
}
