/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models.helpers

import play.api.i18n.Messages
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.gform.eval.{ ExprType, StaticTypeData, TypeInfo }
import uk.gov.hmrc.gform.models.{ FormModel, Visibility }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormCtx, IncludeIf, IsText }
import uk.gov.hmrc.gform.views.summary.TextFormatter

object MiniSummaryListHelper {
  def getFormattedExprStr[D <: DataOrigin](formModelVisibilityOptics: FormModelVisibilityOptics[D], e: Expr)(implicit
    l: LangADT,
    messages: Messages
  ): String = {
    val exprResult = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(e)
    val exprStr = exprResult.typeInfo match {
      case TypeInfo(_, StaticTypeData(ExprType.address, _)) =>
        exprResult.addressRepresentation.map(HtmlFormat.escape).map(_.body).mkString("<br>")
      case _ => exprResult.stringRepresentation
    }
    exprResult.typeInfo.staticTypeData.textConstraint.fold(exprStr) { textConstraint =>
      TextFormatter.componentTextReadonly(exprStr, textConstraint)
    }
  }

  def evaluateIncludeIf[D <: DataOrigin](
    includeIf: Option[IncludeIf],
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ): Boolean =
    includeIf.forall(incI => formModelVisibilityOptics.evalIncludeIfExpr(incI, None))

  def checkAndReturnSuffix(expr: Expr, formModel: FormModel[Visibility])(implicit sse: SmartStringEvaluator): String =
    expr match {
      case formCtx: FormCtx =>
        formModel.fcLookup(formCtx.formComponentId) match {
          case IsText(text) => text.suffix.fold("")(" " + _.value())
          case _            => ""
        }
      case _ => ""
    }
}
