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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait OptionDataValue {
  def mapExpr(f: Expr => Expr): OptionDataValue = this match {
    case OptionDataValue.StringBased(value)      => OptionDataValue.StringBased(value)
    case OptionDataValue.ExprBased(prefix, expr) => OptionDataValue.ExprBased(prefix, f(expr))
    case OptionDataValue.FormCtxBased(formCtx)   => OptionDataValue.FormCtxBased(FormCtx.toFormCtx(formCtx.mapExpr(f)))
  }
}

object OptionDataValue {
  case class StringBased(value: String) extends OptionDataValue
  case class ExprBased(prefix: String, expr: Expr) extends OptionDataValue
  case class FormCtxBased(formCtx: FormCtx) extends OptionDataValue

  implicit val format: OFormat[OptionDataValue] = derived.oformat()
}
