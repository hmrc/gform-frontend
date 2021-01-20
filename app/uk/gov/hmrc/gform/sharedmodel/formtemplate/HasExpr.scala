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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import uk.gov.hmrc.gform.commons.BigDecimalUtil

object HasExpr {
  def unapply(fc: FormComponent): Option[Expr] = unapply(fc.`type`)

  def unapply(ct: ComponentType): Option[Expr] =
    ct match {
      case Text(_, NonValueExpr(expr), _, _, _, _) => Some(expr)
      case TextArea(_, NonValueExpr(expr), _)      => Some(expr)
      case UkSortCode(NonValueExpr(expr))          => Some(expr)
      case HmrcTaxPeriod(_, NonValueExpr(expr), _) => Some(expr)
      case _                                       => None
    }
}

object HasValueExpr {
  def unapply(fc: FormComponent): Option[Expr] = unapply(fc.`type`)

  def unapply(ct: ComponentType): Option[Expr] =
    ct match {
      case Text(_, NonValueExpr(expr), _, _, _, _) => Some(expr)
      case TextArea(_, NonValueExpr(expr), _)      => Some(expr)
      case _                                       => None
    }
}

private object NonValueExpr {
  def unapply(expr: Expr): Option[Expr] =
    expr match {
      case Value => None
      case _     => Some(expr)
    }
}

object IsNumberConstant {
  def unapply(expr: Expr): Option[BigDecimal] = expr match {
    case Constant(c) => BigDecimalUtil.toBigDecimalSafe(c)
    case _           => None
  }
}
