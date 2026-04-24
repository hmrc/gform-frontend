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
import uk.gov.hmrc.gform.eval.{ ExprType, StaticTypeData }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.gform.models.FormModel
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormCtx, IncludeIf, IsText }
import uk.gov.hmrc.gform.views.summary.TextFormatter

import scala.util.{ Success, Try }

object MiniSummaryListHelper {
  def getFormattedExprStr(formModelVisibilityOptics: FormModelVisibilityOptics, e: Expr)(implicit
    l: LangADT,
    messages: Messages
  ): String = {
    val exprResult = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(e)
    val exprStr = exprResult.staticTypeData match {
      case StaticTypeData(ExprType.Address, _) =>
        exprResult.addressRepresentation.map(HtmlFormat.escape).map(_.body).mkString("<br>")
      case _ => exprResult.stringRepresentation
    }
    exprResult.staticTypeData.textConstraint.fold(exprStr) { textConstraint =>
      TextFormatter.componentTextReadonly(exprStr, textConstraint)
    }
  }

  def evaluateIncludeIf(
    includeIf: Option[IncludeIf],
    formModelVisibilityOptics: FormModelVisibilityOptics
  ): Boolean =
    includeIf.forall(incI => formModelVisibilityOptics.evalIncludeIfExpr(incI, None))

  def checkAndReturnSuffix(expr: Expr, formModel: FormModel)(implicit sse: SmartStringEvaluator): String =
    expr match {
      case formCtx: FormCtx =>
        Try(formModel.fcLookup(formCtx.formComponentId)) match {
          case Success(IsText(text)) => text.suffix.fold("")(" " + _.value())
          case _                     => ""
        }
      case _ => ""
    }
}
