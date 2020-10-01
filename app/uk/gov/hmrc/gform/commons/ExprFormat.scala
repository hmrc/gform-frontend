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

import uk.gov.hmrc.gform.models.{ FormModel, Visibility }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait FormatType extends Product with Serializable

object FormatType {
  case object Default extends FormatType
  case class FromText(text: Text) extends FormatType
}

object ExprFormat {

  private def findComponentForFcId(fcId: FormComponentId, formModel: FormModel[Visibility]): Option[FormComponent] = {
    val lookup = formModel.fcLookup
    lookup.get(fcId)
  }

  private def formatFor(formComponent: FormComponent): FormatType = formComponent match {
    case IsText(text) => FormatType.FromText(text)
    case _            => FormatType.Default
  }

  private def formatForFormCtx(fc: FormCtx, formModel: FormModel[Visibility]): FormatType =
    findComponentForFcId(fc.formComponentId, formModel).map(formatFor).getOrElse(FormatType.Default)

  def formatForExpr(expr: Expr, formModel: FormModel[Visibility]): FormatType = expr match {
    case Value                               => FormatType.Default
    case HmrcRosmRegistrationCheck(rosmProp) => FormatType.Default
    case ParamCtx(_)                         => FormatType.Default
    case UserCtx(_)                          => FormatType.Default
    case AuthCtx(_)                          => FormatType.Default
    case FormTemplateCtx(_)                  => FormatType.Default
    case Constant(_)                         => FormatType.Default
    case fc: FormCtx                         => formatForFormCtx(fc, formModel)
    case Sum(f1)                             => formatForExpr(f1, formModel)
    case Else(f1, f2)                        => formatForExpr(f1, formModel)
    case Add(f1, f2)                         => formatForExpr(f1, formModel)
    case Subtraction(f1, f2)                 => formatForExpr(f1, formModel)
    case Multiply(f1, f2)                    => formatForExpr(f1, formModel)
    case LinkCtx(_)                          => FormatType.Default
  }
}
