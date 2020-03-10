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

package uk.gov.hmrc.gform.commons

import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait FormatType extends Product with Serializable

object FormatType {
  case object Default extends FormatType
  case class FromText(text: Text) extends FormatType
}

object ExprFormat {

  private def formatFor(formComponent: FormComponent): FormatType = formComponent match {
    case IsText(text) => FormatType.FromText(text)
    case _            => FormatType.Default
  }

  private def formatForFormCtx(fc: FormCtx, componentLookup: Map[FormComponentId, FormComponent]): FormatType =
    componentLookup.get(fc.toFieldId).fold[FormatType](FormatType.Default)(formatFor)

  def formatForExpr(expr: Expr, componentLookup: Map[FormComponentId, FormComponent]): FormatType = expr match {
    case Value                               => FormatType.Default
    case HmrcRosmRegistrationCheck(rosmProp) => FormatType.Default
    case UserCtx(_)                          => FormatType.Default
    case AuthCtx(_)                          => FormatType.Default
    case EeittCtx(_)                         => FormatType.Default
    case SubmissionReference                 => FormatType.Default
    case Constant(_)                         => FormatType.Default
    case fc: FormCtx                         => formatForFormCtx(fc, componentLookup)
    case Sum(fc: FormCtx)                    => formatForFormCtx(fc, componentLookup)
    case Sum(_)                              => FormatType.Default
    case Else(f1, f2)                        => formatForExpr(f1, componentLookup)
    case Add(f1, f2)                         => formatForExpr(f1, componentLookup)
    case Subtraction(f1, f2)                 => formatForExpr(f1, componentLookup)
    case Multiply(f1, f2)                    => formatForExpr(f1, componentLookup)
  }
}
