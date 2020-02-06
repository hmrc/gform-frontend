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

import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Expr

trait ExprExtractorHelpers {

  def fromSmartStrings(smartStrings: SmartString*): List[Expr] = smartStrings.toList.flatMap(_.interpolations)

  def fromOptionF[A](option: Option[A])(f: A => List[Expr]): List[Expr] = option.fold(List.empty[Expr])(f)

  private def fromOption(maybeSmartString: Option[SmartString]): List[Expr] =
    fromOptionF[SmartString](maybeSmartString)(_.interpolations)

  def fromOption(maybeSmartStrings: Option[SmartString]*): List[Expr] = maybeSmartStrings.toList.flatMap(fromOption)

  def toFirstOperandPlainExprs(exprs: List[Expr]*): List[ExprMetadata] =
    exprs.toList.flatten.map(expr => ExprMetadata.Plain(InferrableExpr(expr, InferringRule.FirstOperand)))

}
