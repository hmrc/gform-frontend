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

import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FieldId, FieldValue }

object Javascript {

  def fieldJavascript(fields: List[FieldValue], groupList: List[List[List[FieldValue]]]): String = {

    val fieldIdWithExpr: List[(FieldId, Expr)] =
      fields.collect {
        case FieldValue(id, Text(_, expr), _, _, _, _, _, _, _, _) => (id, expr)
      }

    fieldIdWithExpr.map(x => toJavascriptFn(x._1, x._2, groupList)).mkString(";\n")
  }

  def toJavascriptFn(fieldId: FieldId, expr: Expr, groupList: List[List[List[FieldValue]]]): String = {

    expr match {
      case Sum(FormCtx(id)) =>
        val eventListeners = Group.getGroup(groupList, FieldId(id)).map { x =>
          s"""document.getElementById("$x").addEventListener("change",sum$id);
              document.getElementById("$x").addEventListener("keyup",sum$id);
           """
        }.mkString("\n")

        val groups = Group.getGroup(groupList, FieldId(id)).map { x =>
          s"""parseInt(document.getElementById("$x").value) || 0"""
        }.mkString(",")
        s"""function sum$id() {
              var sum = [${groups}];
              var result = sum.reduce(add, 0);
              return document.getElementById("${fieldId.value}").value = result;
            };

            function add(a, b) {
             return a + b
            };
            $eventListeners
            """
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

  def collapsingGroupJavascript(fieldId: FieldId, group: Group) = {
    s"""
       |function removeOnClick$fieldId() {
       |${group.fields.map(fv => s"""document.getElementById("${fv.id}").value = '' """).mkString(";\n")}
       |}
     """.stripMargin
  }
}
