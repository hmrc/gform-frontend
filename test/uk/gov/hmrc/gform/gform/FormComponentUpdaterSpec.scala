/*
 * Copyright 2023 HM Revenue & Customs
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

import uk.gov.hmrc.gform.Helpers.{ toSmartString, toSmartStringExpression }

import scala.language.implicitConversions
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId }

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

  it should "update dynamic, label and hint in DataRetrieveBased choice option" in {
    val formComponent = mkFormComponent(
      "choice1",
      mkChoice("individualsEmploymentsAtl")
    )

    val expected = mkFormComponent(
      "11_choice1",
      mkChoice("11_individualsEmploymentsAtl")
    )

    val res = updateSection(formComponent)
    res shouldBe expected
  }

  it should "update dynamic DataRetrieveId in TableValueRow when inside an addToList" in {
    val dataRetrieveId = "individualsEmploymentsAtl"
    val formComponent = mkFormComponent(
      "vatDetailsTable",
      mkTableComp(dataRetrieveId)
    )

    val res = updateSection(formComponent)

    val expectedDataRetrieveId = "11_individualsEmploymentsAtl"
    val tableComp = res.`type`.asInstanceOf[TableComp]
    val row = tableComp.rows.head

    row.dynamic shouldBe Some(
      Dynamic.DataRetrieveBased(
        IndexOfDataRetrieveCtx(
          DataRetrieveCtx(DataRetrieveId(expectedDataRetrieveId), DataRetrieve.Attribute("employerName")),
          Constant("0")
        )
      )
    )

    row.values.head.value.allInterpolations shouldBe List(
      DataRetrieveCtx(DataRetrieveId(expectedDataRetrieveId), DataRetrieve.Attribute("employerName"))
    )
    row.values(1).value.allInterpolations shouldBe List(
      DataRetrieveCtx(DataRetrieveId(expectedDataRetrieveId), DataRetrieve.Attribute("payeNumber"))
    )
  }

  private def updateGroup(formComponent: FormComponent) = {
    val group = mkGroup(2, List(mkFormComponent("choice", Value)))

    new FormComponentUpdater(formComponent, 11, group.fields.map(_.id), List.empty[DataRetrieveId]).updatedWithId
  }

  private def updateSection(formComponent: FormComponent) = {
    val section = mkSection(List(mkFormComponent("choice", Value)))

    new FormComponentUpdater(
      formComponent,
      11,
      section.page.fields.map(_.id),
      List(DataRetrieveId("individualsEmploymentsAtl"))
    ).updatedWithId
  }

  private def mkChoice(dataRetrieveId: String) =
    Choice(
      Checkbox,
      mkChoiceOptions(dataRetrieveId),
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

  private def mkChoiceOptions(dataRetrieveId: String) =
    List(
      OptionData.ValueBased(
        label = toSmartStringExpression(
          "{0}",
          DataRetrieveCtx(DataRetrieveId(dataRetrieveId), DataRetrieve.Attribute("employerName"))
        ),
        hint = Option(
          toSmartStringExpression(
            "{0}",
            DataRetrieveCtx(DataRetrieveId(dataRetrieveId), DataRetrieve.Attribute("worksNumber"))
          )
        ),
        includeIf = None,
        value = OptionDataValue.StringBased("EMP"),
        dynamic = Option(
          Dynamic.DataRetrieveBased(
            IndexOfDataRetrieveCtx(
              DataRetrieveCtx(DataRetrieveId(dataRetrieveId), DataRetrieve.Attribute("employerName")),
              Constant("0")
            )
          )
        ),
        summaryValue = None,
        keyWord = None
      )
    )

  private def mkTableComp(dataRetrieveId: String) =
    TableComp(
      header = List(
        TableHeadCell(toSmartString("Employer name"), None),
        TableHeadCell(toSmartString("Employer PAYE reference"), None)
      ),
      rows = List(
        TableValueRow(
          values = List(
            TableValue(
              toSmartStringExpression(
                "{0}",
                DataRetrieveCtx(DataRetrieveId(dataRetrieveId), DataRetrieve.Attribute("employerName"))
              ),
              None,
              None,
              None
            ),
            TableValue(
              toSmartStringExpression(
                "{0}",
                DataRetrieveCtx(DataRetrieveId(dataRetrieveId), DataRetrieve.Attribute("payeNumber"))
              ),
              None,
              None,
              None
            )
          ),
          includeIf = None,
          dynamic = Some(
            Dynamic.DataRetrieveBased(
              IndexOfDataRetrieveCtx(
                DataRetrieveCtx(DataRetrieveId(dataRetrieveId), DataRetrieve.Attribute("employerName")),
                Constant("0")
              )
            )
          )
        )
      ),
      summaryValue = toSmartString("View employments")
    )
}
