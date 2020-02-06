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

package uk.gov.hmrc.gform.gform

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._

class FormComponentUpdaterSpec extends Spec {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)

  "FormComponentUpdater" should "update reference to group's field in ValidIf expression" in {

    val validIf = ValidIf(Equals(FormCtx("choice"), FormCtx("notInGroup")))

    val formComponent = mkFormComponent("abc", Constant("dummy")).copy(validIf = Some(validIf))

    val res = updateGroup(formComponent)

    val expected = ValidIf(Equals(FormCtx("11_choice"), FormCtx("notInGroup")))

    res.validIf.value shouldBe expected
  }

  it should "update reference to group's field in value expression" in {

    val expr = Add(FormCtx("choice"), FormCtx("notInGroup"))

    val formComponent = mkFormComponent("abc", expr)

    val res = updateGroup(formComponent)

    val expected = Add(FormCtx("11_choice"), FormCtx("notInGroup"))

    res should containsExpr(expected)

  }

  it should "update reference to section's field in ValidIf expression" in {
    val validIf = ValidIf(Equals(FormCtx("choice"), FormCtx("notInSection")))

    val formComponent = mkFormComponent("abc", Constant("dummy")).copy(validIf = Some(validIf))

    val res = updateSection(formComponent)

    val expected = ValidIf(Equals(FormCtx("11_choice"), FormCtx("notInSection")))

    res.validIf.value shouldBe expected
  }

  it should "update reference to section's field in value expression" in {
    val expr = Add(FormCtx("choice"), FormCtx("notInGroup"))

    val formComponent = mkFormComponent("abc", expr)

    val res = updateSection(formComponent)

    val expected = Add(FormCtx("11_choice"), FormCtx("notInGroup"))

    res should containsExpr(expected)
  }

  private def updateGroup(formComponent: FormComponent) = {
    val group = mkGroup(2, List(mkFormComponent("choice", Value)))

    new FormComponentUpdater(formComponent, 11, group.fields.map(_.id)).updatedWithId
  }

  private def updateSection(formComponent: FormComponent) = {
    val section = mkSection(List(mkFormComponent("choice", Value)))

    new FormComponentUpdater(formComponent, 11, section.page.fields.map(_.id)).updatedWithId
  }
}
