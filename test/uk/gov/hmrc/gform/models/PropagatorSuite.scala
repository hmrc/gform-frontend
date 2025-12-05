/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import munit.FunSuite
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Checkbox, Choice, FormComponent, FormComponentId, FormCtx, Horizontal, Mandatory, OptionData, OptionDataValue }
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData, VariadicValue }

class PropagatorSuite extends FunSuite {
  test("Change of component value is propagated to selected option value") {

    val formComponents = List(
      mkFormComponent("forWho")
    )

    val propagator = Propagator(formComponents)

    val newData: VariadicFormData[SourceOrigin.OutOfDate] =
      VariadicFormData(
        Map(
          FormComponentId("cake").modelComponentId -> VariadicValue.One("new_value") // Data from POST request
        )
      )

    val oldData: RecData[SourceOrigin.Current] = RecData(
      VariadicFormData(
        Map(
          FormComponentId("cake").modelComponentId   -> VariadicValue.One("old_value"),
          FormComponentId("forWho").modelComponentId -> VariadicValue.Many(List("cake_old_value"))
        )
      )
    )

    val expected: VariadicFormData[SourceOrigin.OutOfDate] =
      VariadicFormData(
        Map(
          FormComponentId("cake").modelComponentId   -> VariadicValue.One("new_value"),
          FormComponentId("forWho").modelComponentId -> VariadicValue.Many(List("cake_new_value"))
        )
      )

    val result: VariadicFormData[SourceOrigin.OutOfDate] = propagator.propagate(newData, oldData)

    assertEquals(result, expected)

  }

  private def mkFormComponent(fcId: String) =
    FormComponent(
      FormComponentId(fcId),
      mkChoice(),
      toSmartString("Label"),
      false,
      None,
      None,
      None,
      None,
      Mandatory.True,
      false,
      true,
      false,
      false,
      None,
      None
    )

  private def mkChoice() =
    Choice(
      Checkbox,
      mkChoiceOptions(),
      Horizontal,
      List.empty,
      None,
      None,
      None,
      toSmartString("or", "neu"),
      None,
      None,
      false,
      false
    )

  private def mkChoiceOptions() =
    NonEmptyList.one(
      OptionData.ValueBased(
        label = toSmartString("label"),
        hint = None,
        includeIf = None,
        value = OptionDataValue.ExprBased(FormCtx(FormComponentId("cake"))),
        dynamic = None,
        summaryValue = None,
        keyWord = None
      )
    )
}
