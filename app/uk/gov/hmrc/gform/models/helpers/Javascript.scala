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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormComponentId, FormComponent }

import scala.concurrent.{ ExecutionContext, Future }

object Javascript {

  def fieldJavascript(fields: List[FormComponent], groupList: Future[List[List[List[FormComponent]]]])(implicit ex: ExecutionContext): Future[String] = {

    val fieldIdWithExpr: List[(FormComponentId, Expr)] =
      fields.collect {
        case FormComponent(id, Text(_, expr), _, _, _, _, _, _, _, _, _) => (id, expr)
      }

    Future.sequence(fieldIdWithExpr.map(x => toJavascriptFn(x._1, x._2, groupList))).map(_.mkString("\n"))
  }

  def toJavascriptFn(fieldId: FormComponentId, expr: Expr, groupList: Future[List[List[List[FormComponent]]]])(implicit ex: ExecutionContext): Future[String] = {

    val functionName = "add" + fieldId.value

    def eventListeners(id: String) = {
      s"""document.getElementById("$id").addEventListener("change",$functionName);
         |document.getElementById("$id").addEventListener("keyup",$functionName);
       """.stripMargin
    }

    def values(id: String) = s"""parseInt(document.getElementById("$id").value.replace(/[£,]/g,'')) || 0"""

    def ids(expr: Expr): Future[List[String]] = {
      expr match {
        case Add(amountA, amountB) => ids(amountA).flatMap(first => ids(amountB).map(_ ::: first))
        case FormCtx(amountX) => Future.successful(List(amountX))
        case Sum(FormCtx(id)) => Group.getGroup(groupList, FormComponentId(id)).map(fieldId => fieldId.map(_.value))
        case otherwise => Future.successful(List(""))
      }
    }

    val demValues = ids(expr).map(_.map(values).mkString(", "))
    val listeners = ids(expr).map(_.map(eventListeners).mkString("\n"))

    expr match {
      case Sum(FormCtx(id)) =>
        val eventListeners = Group.getGroup(groupList, FormComponentId(id)).map { listFieldId =>
          listFieldId.map(groupFieldId =>
            s"""document.getElementById("${groupFieldId.value}").addEventListener("change",sum$id);
              document.getElementById("${groupFieldId.value}").addEventListener("keyup",sum$id);
              window.addEventListener("load",sum$id);
           """).mkString("\n")
        }

        val groups: Future[String] = Group.getGroup(groupList, FormComponentId(id)).map { listFieldId =>
          listFieldId.map(_.value).map(values).mkString(",")
        }
        for {
          listeners <- eventListeners
          values <- groups
        } yield {
          s"""function sum$id() {
              var sum = [$values];
              var result = sum.reduce(add, 0);
              return document.getElementById("${fieldId.value}").value = result;
            };

            function add(a, b) {
             return a + b
            };
            $listeners
            """
        }
      case Add(b, sn) =>
        for {
          values <- demValues
          listener <- listeners
        } yield {
          s"""|function $functionName() {
        |  var x = [ $values ];
        |  var result = x.reduce(add, 0);
        |  return document.getElementById("${fieldId.value}").value = result;
        |};
        |
        |function add(a, b) {
        | return a + b;
        |};
        |$listener
        |""".stripMargin
        }
      case otherwise => Future.successful("")
    }
  }

  def collapsingGroupJavascript(fieldId: FormComponentId, group: Group) = {
    s"""
       |function removeOnClick$fieldId() {
       |${group.fields.map(fv => s"""document.getElementById("${fv.id}").value = '' """).mkString(";\n")}
       |}
     """.stripMargin
  }
}
