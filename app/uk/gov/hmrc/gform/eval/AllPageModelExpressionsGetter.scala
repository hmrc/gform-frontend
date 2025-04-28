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

import uk.gov.hmrc.gform.models.{ Basic, Bracket, DataExpanded, FormModel, PageMode, Singleton }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BooleanExpr, CheckYourAnswersPage, Expr, Page }

object AllPageModelExpressionsGetter extends ExprExtractorHelpers {
  /*
   * Returns list of every single expression in a bracket
   */
  def allExprs[A <: PageMode](formModel: FormModel[DataExpanded])(bracket: Bracket[A]): List[Expr] = {
    val bracketExprs =
      bracket match {
        case AllPageModelExpressions(exprMetadatas) => exprMetadatas.map(_.expr)
        case otherwise                              => List.empty[Expr]
      }
    bracketExprs ++ formComponentsExprs(formModel)(bracket)

  }

  private def fromBooleanExprExprs(formModel: FormModel[DataExpanded])(
    booleanExpr: BooleanExpr
  ): List[Expr] =
    booleanExpr.allExpressions.flatMap(_.leafs(formModel))

  def fromSingleton[A <: PageMode](
    formModel: FormModel[DataExpanded]
  )(singleton: Singleton[A]): List[Expr] = {
    val componentsExprs: List[Expr] = singleton.allFormComponents
      .flatMap {
        case AllFormComponentExpressions(exprs) => exprs.map(_.expr)
        case _                                  => List.empty[Expr]
      }

    val includeIfBooleanExprs: List[BooleanExpr] =
      singleton.getIncludeIf.toList.map(_.booleanExpr)
    val includeIfsBooleanExprs: List[BooleanExpr] =
      singleton.allComponentIncludeIfs
        .map(_._1)
        .map(_.booleanExpr)
    val validIfsBooleanExprs: List[BooleanExpr] =
      singleton.allValidIfs
        .flatMap(_._1)
        .map(_.booleanExpr)

    val booleanExprsExprs: List[Expr] =
      (includeIfBooleanExprs ++ includeIfsBooleanExprs ++ validIfsBooleanExprs).flatMap(fromBooleanExprExprs(formModel))

    componentsExprs ++ booleanExprsExprs
  }
  def fromCheckYourAnswerPage(cyap: CheckYourAnswersPage): List[Expr] =
    cyap.updateTitle.allInterpolations ++
      fromOption(
        cyap.title,
        cyap.caption,
        cyap.noPIITitle,
        cyap.noPIIUpdateTitle,
        cyap.header,
        cyap.footer,
        cyap.continueLabel
      )

  private def fromPage(page: Page[Basic]) =
    page.allFields.flatMap {
      case AllFormComponentExpressions(exprs) => exprs.map(_.expr)
      case _                                  => List.empty[Expr]
    } ++
      fromSmartStrings(page.title) ++
      fromOption(
        page.description,
        page.shortName,
        page.continueLabel
      )

  private def formComponentsExprs[A <: PageMode](
    formModel: FormModel[DataExpanded]
  )(bracket: Bracket[A]): List[Expr] =
    bracket.fold { nonRepeatingPage =>
      fromSingleton(formModel)(nonRepeatingPage.singleton.singleton)
    } { repeatingPage =>
      repeatingPage.singletons.map(_.singleton).toList.flatMap(fromSingleton(formModel))
    } { addToList =>
      addToList.iterations.toList.flatMap { iteration =>
        iteration.defaultPage.map(_.singleton).toList.flatMap(fromSingleton(formModel)) ++
          iteration.singletons.map(_.singleton).toList.flatMap(fromSingleton(formModel)) ++
          addToList.source.cyaPage.map(fromCheckYourAnswerPage).getOrElse(Nil) ++
          addToList.source.defaultPage.map(fromPage).getOrElse(Nil)
      }
    }

}
