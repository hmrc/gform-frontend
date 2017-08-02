/*
 * Copyright 2017 HM Revenue & Customs
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

import uk.gov.hmrc.gform.models.components._

object Javascript {

  def fieldJavascript(fields: List[FieldValue]): String = {

    val fieldIdWithExpr: List[(FieldId, Expr)] =
      fields.collect { case FieldValue(id, Text(_, expr, _), _, _, _, _, _, _, _, _) => (id, expr) }

    fieldIdWithExpr.map((toJavascriptFn _).tupled).mkString(";\n")
  }

  def toJavascriptFn(fieldId: FieldId, expr: Expr): String = {

    expr match {
      case Add(FormCtx(amountA), FormCtx(amountB)) =>

        val functionName = "add" + fieldId.value;

        val eventListeners =
          for {
            elementId <- List(amountA, amountB)
            event <- List("change", "keyup")
          } yield s"""document.getElementById("$elementId").addEventListener("$event",$functionName);"""

        s"""|function $functionName() {
            |  var el1 = document.getElementById("$amountA").value;
            |  var el2 = document.getElementById("$amountB").value;
            |  var result = (parseInt(el1) || 0) + (parseInt(el2) || 0);
            |  return document.getElementById("${fieldId.value}").value = result;
            |};
            |${eventListeners.mkString("\n")}
            |""".stripMargin
      case otherwise => ""
    }
  }

}
