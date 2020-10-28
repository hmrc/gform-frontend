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

import cats.syntax.option._
import uk.gov.hmrc.gform.models.{ PageMode, PageModel, Repeater, Singleton }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BankAccountModulusCheck, Expr, HmrcRosmRegistrationCheckValidator }

/*
 * Extracts metadata for all expressions of a Page.
 * This doesn't include expressions in fields
 */
object AllPageModelExpressions extends ExprExtractorHelpers {
  def unapply[A <: PageMode](pageModel: PageModel[A]): Option[List[ExprMetadata]] = {

    def fromSingleton(singleton: Singleton[_]): List[Expr] = {
      val page = singleton.page
      val pageExprs = page.title.interpolations ++ fromOption(
        page.description,
        page.shortName,
        page.progressIndicator,
        page.continueLabel,
        page.instruction.map(_.name)
      )

      val validatorExprs = page.validators.fold(List.empty[Expr]) {
        case HmrcRosmRegistrationCheckValidator(errorMessage, _, utr, postCode) =>
          utr :: postCode :: errorMessage.interpolations
        case BankAccountModulusCheck(errorMessage, accountNumber, sortCode) =>
          accountNumber :: sortCode :: errorMessage.interpolations
      }
      pageExprs ++ validatorExprs
    }

    def fromRepeater(repeater: Repeater[_]): List[Expr] =
      fromSmartStrings(repeater.expandedTitle, repeater.expandedDescription, repeater.expandedShortName)

    def fromRepatedSection(singleton: Singleton[_]): Option[Expr] =
      singleton.source.fold[Option[Expr]](_ => none)(_.repeats.some)(_ => none)

    val pageExprs: List[Expr] = pageModel.fold(fromSingleton)(fromRepeater)
    val pageExprsMeta = toFirstOperandPlainExprs(pageExprs)

    val repeatedSectionExprs: Option[Expr] = pageModel.fold[Option[Expr]](fromRepatedSection)(_ => none)
    val repeatedSectionExprsMeta: List[ExprMetadata] = toFirstOperandPlainExprs(repeatedSectionExprs.toList)

    (pageExprsMeta ++ repeatedSectionExprsMeta) match {
      case Nil => None
      case xs  => Some(xs)
    }
  }
}
