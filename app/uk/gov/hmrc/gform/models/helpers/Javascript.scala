/*
 * Copyright 2018 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormComponent, FormComponentId }

import scala.concurrent.{ ExecutionContext, Future }

object Javascript {

  private object HasExpr {
    def unapply(fc: FormComponent): Option[Expr] =
      fc.`type` match {
        case Text(_, expr)     => Some(expr)
        case TextArea(_, expr) => Some(expr)
        case _                 => None
      }
  }

  def fieldJavascript(
    sectionFields: List[FormComponent],
    allFields: List[FormComponent],
    groupList: Future[List[List[List[FormComponent]]]])(implicit ex: ExecutionContext): Future[String] = {

    val sectionFieldIds = sectionFields.map(_.id).toSet

    def isDynamic(expr: Expr): Boolean = expr match {
      case f @ FormCtx(_) =>
        sectionFieldIds.contains(f.toFieldId)
      case Sum(f @ FormCtx(_)) =>
        sectionFieldIds.contains(f.toFieldId)
      case Add(field1, field2) =>
        isDynamic(field1) || isDynamic(field2)
      case Subtraction(field1, field2) =>
        isDynamic(field1) || isDynamic(field2)
      case Multiply(field1, field2) =>
        isDynamic(field1) || isDynamic(field2)
      case otherwise => false
    }

    val fieldIdWithExpr: List[(FormComponent, Expr)] =
      sectionFields.collect {
        case formComponent @ HasExpr(expr) if isDynamic(expr) => (formComponent, expr)
      }

    Future
      .sequence(fieldIdWithExpr.map(x => toJavascriptFn(x._1, x._2, groupList)))
      .map(_.mkString("\n"))
      .map(x =>
        x +
          """function getNumber(value) {
            |  if (value == ""){
            |    return "0";
            |  } else {
            |   return value.replace(",", "");
            |  }
            |};
            |
            |function add(a, b) {
            |  return new Big(a).add(new Big(b));
            |};
            |
            |function subtract(a, b) {
            |  return new Big(a).minus(new Big(b));
            |};
            |
            |function multiply(a, b) {
            |  return new Big(a).times(new Big(b));
            |};
            |""".stripMargin)
      .map(s => {
        val ss: String = s
        s
      })
  }

  private object HasDigits {
    def unapply(expr: ComponentType): Option[Int] =
      expr match {
        case Text(Number(_, digits, _), _)             => Some(digits)
        case Text(PositiveNumber(_, digits, _), _)     => Some(digits)
        case TextArea(Number(_, digits, _), _)         => Some(digits)
        case TextArea(PositiveNumber(_, digits, _), _) => Some(digits)
        case _                                         => None
      }
  }

  private object HasSterling {
    def unapply(expr: ComponentType): Option[Int] =
      expr match {
        case Text(Sterling, _)     => Some(2)
        case TextArea(Sterling, _) => Some(2)
        case _                     => None
      }
  }

  private def toJavascriptFn(field: FormComponent, expr: Expr, groupList: Future[List[List[List[FormComponent]]]])(
    implicit ex: ExecutionContext): Future[String] = {

    def roundTo = field.`type` match {
      case HasDigits(digits)   => digits
      case HasSterling(digits) => digits
      case _                   => TextConstraint.defaultFactionalDigits
    }

    def eventListeners(id: String, functionName: String) =
      s"""document.getElementById("$id").addEventListener("change",$functionName);
         |document.getElementById("$id").addEventListener("keyup",$functionName);
         |window.addEventListener("load", $functionName);
       """.stripMargin

    def values(id: String) = s"""getNumber(document.getElementById("$id").value.replace(/[£,]/g,''))"""

    def ids2(e1: Expr, e2: Expr) =
      for {
        x <- ids(e1)
        y <- ids(e2)
      } yield x ::: y

    def ids3(e1: Expr, e2: Expr, e3: Expr) =
      for {
        x <- ids(e1)
        y <- ids(e2)
        z <- ids(e3)
      } yield x ::: y ::: z

    def ids(expr: Expr): Future[List[String]] =
      expr match {
        case Add(e1, Multiply(e2, e3)) =>
          ids3(e1, e2, e3)
        case Add(e1, e2) =>
          ids2(e1, e2)
        case FormCtx(amountX) =>
          Future.successful(List(amountX))
        case Subtraction(e1, Multiply(e2, e3)) =>
          ids3(e1, e2, e3)
        case Subtraction(e1, e2) =>
          ids2(e1, e2)
        case Multiply(e1, e2) =>
          ids2(e1, e2)
        case Sum(FormCtx(id)) =>
          Group.getGroup(groupList, FormComponentId(id)).map(eId => eId.map(_.value))
        case otherwise =>
          Future.successful(List(""))
      }

    def consts(expr: Expr): List[String] =
      expr match {
        case Constant(amount) => List(amount)
        case Add(amountA, amountB) =>
          val x = consts(amountA)
          val y = consts(amountB)
          x ::: y
        case Subtraction(field1, field2) =>
          val x = consts(field1)
          val y = consts(field2)
          x ::: y
        case Multiply(field1, field2) =>
          val x = consts(field1)
          val y = consts(field2)
          x ::: y
        case _ => List("")
      }

    // TODO: These filters are a bit of a hack
    val demValues =
      ids(expr).map(i => (i.filterNot(_.isEmpty).map(values) ::: consts(expr).filterNot(_.isEmpty)).mkString(", "))
    def listeners(functionName: String) =
      ids(expr).map(_.filterNot(_.isEmpty).map(eventListeners(_, functionName)).mkString("\n"))

    def function(name: String, values: String, calculation: String, listener: String) =
      s"""|function $name() {
          |  var x = [ $values ];
          |  var result = $calculation;
          |  document.getElementById("${field.id.value}").value = result.toFixed($roundTo, 0);
          |  return document.getElementById("${field.id.value}-total").innerHTML = result.toFixed($roundTo, 0);
          |};
          |$listener
          |""".stripMargin

    // TODO: the use of reduce() is simplistic, we need to generate true javascript expressions based on the parsed gform expression
    expr match {
      case Sum(FormCtx(id)) =>
        val eventListeners = Group.getGroup(groupList, FormComponentId(id)).map { listFieldId =>
          listFieldId
            .map(groupFieldId => s"""document.getElementById("${groupFieldId.value}").addEventListener("change",sum$id);
              document.getElementById("${groupFieldId.value}").addEventListener("keyup",sum$id);
              window.addEventListener("load",sum$id);
           """)
            .mkString("\n")
        }

        val groups: Future[String] = Group.getGroup(groupList, FormComponentId(id)).map { listFieldId =>
          listFieldId.map(_.value).map(values).mkString(s",")
        }
        for {
          listeners <- eventListeners
          values    <- groups
        } yield {
          s"""
              function sum$id() {
              var sum = [$values];
              var result = sum.reduce(add, 0);
              return document.getElementById("${field.id.value}").value = result.toFixed($roundTo, 1);
            };
            $listeners
            """
        }
      case Add(field1, Multiply(field2, field3)) =>
        val functionName = "addMultiply" + field.id.value
        for {
          values   <- demValues
          listener <- listeners(functionName)
        } yield function(functionName, values, "add(x[0], multiply(x[1],x[2]))", listener)
      case Add(b, sn) =>
        val functionName = "add" + field.id.value
        for {
          values   <- demValues
          listener <- listeners(functionName)
        } yield function(functionName, values, "x.reduce(add, 0)", listener)
      case Subtraction(field1, Multiply(field2, field3)) =>
        val functionName = "subtractMultiply" + field.id.value
        for {
          values   <- demValues
          listener <- listeners(functionName)
        } yield function(functionName, values, "subtract(x[0], multiply(x[1],x[2]))", listener)
      case Subtraction(field1, field2) =>
        val functionName = "subtract" + field.id.value
        for {
          values   <- demValues
          listener <- listeners(functionName)
        } yield function(functionName, values, "subtract(x[0], x[1])", listener)
      case Multiply(field1, field2) =>
        val functionName = "multiply" + field.id.value
        for {
          values   <- demValues
          listener <- listeners(functionName)
        } yield function(functionName, values, "x.reduce(multiply, 1)", listener)
      case otherwise => Future.successful("")
    }
  }

  def collapsingGroupJavascript(fieldId: FormComponentId, group: Group) =
    s"""
       |function removeOnClick$fieldId() {
       |${group.fields.map(fv => s"""document.getElementById("${fv.id}").value = '' """).mkString(";\n")}
       |}
     """.stripMargin
}
