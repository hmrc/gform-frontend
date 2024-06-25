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

import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, Number, PositiveNumber, Sterling, WholeSterling }

case class TypeInfo(expr: Expr, staticTypeData: StaticTypeData) {
  def defaultValue: String = staticTypeData.textConstraint.fold("") {
    case Sterling(_, _)             => "0"
    case WholeSterling(_)           => "0"
    case Number(_, _, _, _)         => "0"
    case PositiveNumber(_, _, _, _) => "0"
    case _                          => ""
  }
}

object TypeInfo {
  def illegal(expr: Expr) = TypeInfo(expr, StaticTypeData.illegal)
}
