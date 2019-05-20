/*
 * Copyright 2019 HM Revenue & Customs
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

import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.models.Dependecies
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import Function.const

class JavascriptSpec extends Spec {

  private val c = Constant("5")

  def formComponent(id: String, value: Expr = c) =
    FormComponent(
      FormComponentId(id),
      Text(AnyText, value),
      toLocalisedString(""),
      None,
      None,
      None,
      false,
      true,
      true,
      true,
      false,
      None)

  private def fieldJavascript(
    field: FormComponent,
    rfcIds: RepeatFormComponentIds = RepeatFormComponentIds(const(List.empty[FormComponentId]))) = {
    val fields = List(formComponent("thisSection"), field)
    Javascript.fieldJavascript(
      sectionFields = fields.map(FormComponentSimple.apply),
      allFields = formComponent("otherSection") :: fields,
      repeatFormComponentIds = rfcIds,
      dependencies = Dependecies(List.empty)
    )
  }

  "if calculation references only a constant" should "not generate Javascript for the static calculation" in {
    val result = fieldJavascript(formComponent("staticExpr"))
    result should not include ("decimalPlaces")
  }

  "if calculation references only a field in this section" should "not generate Javascript for the static calculation" in {
    val result = fieldJavascript(formComponent("staticExpr", FormCtx("thisSection")))
    val jsExp = """BigNumber(getValue("thisSection", 0)).decimalPlaces(2, BigNumber.ROUND_DOWN);"""
    result should include(jsExp)
  }

  "if calculation references only a group in this section" should "generate Javascript for the dynamic calculation" in {
    val thisSection = "thisSection"
    val result =
      fieldJavascript(
        formComponent("dynamicExpr", Sum(FormCtx(thisSection))),
        RepeatFormComponentIds(_ :: (1 until 5 map (i => FormComponentId(i + "_" + thisSection))).toList)
      )
    val jsExp =
      """BigNumber(add(add(add(add(add(0, getValue("thisSection", 0)), getValue("1_thisSection", 0)), getValue("2_thisSection", 0)), getValue("3_thisSection", 0)), getValue("4_thisSection", 0))).decimalPlaces(2, BigNumber.ROUND_DOWN)"""
    result should include(jsExp)
  }

  "if calculation adds a field in this section" should "generate Javascript for the dynamic calculation" in {
    val result = fieldJavascript(formComponent("dynamicExpr", Add(FormCtx("thisSection"), c)))
    val jsExp = """BigNumber(add(getValue("thisSection", 0), 5)).decimalPlaces(2, BigNumber.ROUND_DOWN);"""
    result should include(jsExp)
  }

  "if calculation deep inside uses a field in this section" should "generate Javascript for the dynamic calculation" in {
    val result = fieldJavascript(
      formComponent(
        "dynamicExpr",
        Add(c, Add(Subtraction(c, Subtraction(Multiply(c, Multiply(FormCtx("thisSection"), c)), c)), c))))
    val jsExp =
      """BigNumber(add(5, add(subtract(5, subtract(multiply(5, multiply(getValue("thisSection", 1), 5)), 5)), 5))).decimalPlaces(2, BigNumber.ROUND_DOWN);"""
    result should include(jsExp)

  }

}
