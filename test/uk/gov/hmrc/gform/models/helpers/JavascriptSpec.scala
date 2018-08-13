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

import cats.implicits._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.models.Dependecies
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.concurrent.Future

class JavascriptSpec extends Spec {

  private val c = Value

  def formComponent(id: String, value: Expr = c) =
    FormComponent(FormComponentId(id), Text(AnyText, value), "", None, None, None, false, true, true, true, false, None)

  private def fieldJavascript(field: FormComponent) = {
    val fields = List(formComponent("thisSection"), field)
    Javascript.fieldJavascript(
      sectionFields = fields,
      allFields = formComponent("otherSection") :: fields,
      groupList = List.empty[List[List[FormComponent]]],
      dependencies = Dependecies(List.empty)
    )
  }

  "if calculation references only a constant" should "not generate Javascript for the static calculation" in {
    val result = fieldJavascript(formComponent("staticExpr"))
    result should not include ("addstaticExpr")
  }

  "if calculation references only a field in this section" should "not generate Javascript for the static calculation" in {
    val result = fieldJavascript(formComponent("staticExpr", FormCtx("thisSection")))
    result should not include ("addstaticExpr")
  }

  "if calculation references only a group in this section" should "generate Javascript for the dynamic calculation" in {
    val result = fieldJavascript(formComponent("dynamicExpr", Sum(FormCtx("thisSection"))))
    result should include("sumthisSection")
  }

  "if calculation adds a field in this section" should "generate Javascript for the dynamic calculation" in {
    val result = fieldJavascript(formComponent("dynamicExpr", Add(FormCtx("thisSection"), c)))
    result should include("adddynamicExpr")
  }

  "if calculation deep inside uses a field in this section" should "generate Javascript for the dynamic calculation" in {
    val result = fieldJavascript(
      formComponent(
        "dynamicExpr",
        Add(c, Add(Subtraction(c, Subtraction(Multiply(c, Multiply(FormCtx("thisSection"), c)), c)), c))))
    result should include("adddynamicExpr")
  }

}
