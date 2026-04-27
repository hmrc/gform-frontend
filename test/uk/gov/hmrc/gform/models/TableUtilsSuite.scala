/*
 * Copyright 2026 HM Revenue & Customs
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

import munit.FunSuite
import play.api.Configuration
import play.api.Environment
import play.api.http.HttpConfiguration
import play.api.i18n._
import uk.gov.hmrc.gform.Helpers.{ toSmartString, toSmartStringExpression }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ FormModelOptics, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class TableUtilsSuite extends FunSuite with FormModelSupport with VariadicFormDataSupport {

  implicit val lang: LangADT = LangADT.En

  private val environment = Environment.simple()
  private val configuration = Configuration.load(environment)
  private val langs = new DefaultLangs()
  private val httpConfiguration = HttpConfiguration.fromConfiguration(configuration, environment)
  private val messagesApi: MessagesApi =
    new DefaultMessagesApiProvider(environment, configuration, langs, httpConfiguration).get
  implicit val messages: Messages = messagesApi.preferred(Seq(langs.availables.head))

  // --- DataRetrieve Based Dynamic tests ---

  private val dataRetrieveId = DataRetrieveId("individualsEmployments")

  private val employerNameAttr = DataRetrieve.Attribute("employerName")
  private val payeNumberAttr = DataRetrieve.Attribute("payeNumber")

  private val employerNameCtx = DataRetrieveCtx(dataRetrieveId, employerNameAttr)
  private val payeNumberCtx = DataRetrieveCtx(dataRetrieveId, payeNumberAttr)

  private def mkTableValue(label: String, ctx: DataRetrieveCtx): TableValue =
    TableValue(toSmartStringExpression(label, ctx), None, None, None)

  private def mkTableCompComponent(drId: DataRetrieveId): FormComponent = {
    val employerCtx = DataRetrieveCtx(drId, employerNameAttr)
    val payeCtx = DataRetrieveCtx(drId, payeNumberAttr)
    val table = TableComp(
      header = List(
        TableHeadCell(toSmartString("Employer name"), None),
        TableHeadCell(toSmartString("PAYE reference"), None)
      ),
      rows = List(
        TableValueRow(
          values = List(
            mkTableValue("employer", employerCtx),
            mkTableValue("paye", payeCtx)
          ),
          includeIf = None,
          dynamic = Some(
            Dynamic.DataRetrieveBased(
              IndexOfDataRetrieveCtx(employerCtx, Constant("0"))
            )
          )
        )
      ),
      summaryValue = toSmartString("View employments")
    )
    mkFormComponent("vatDetailsTable", table)
  }

  private def mkDataRetrieveResults(
    drId: DataRetrieveId,
    data: List[Map[DataRetrieve.Attribute, String]]
  ): ThirdPartyData =
    ThirdPartyData.empty.copy(
      dataRetrieve = Some(
        Map(
          drId -> DataRetrieveResult(
            id = drId,
            data = RetrieveDataType.ListType(data),
            requestParams = play.api.libs.json.JsNull,
            failureCount = None,
            failureMaxAttempts = None,
            failureCountResetTime = None
          )
        )
      )
    )

  private def buildFmvo(
    tableComponent: FormComponent,
    tpd: ThirdPartyData
  ): FormModelOptics = {
    val section = mkSection(List(tableComponent))
    val formTemplate = mkFormTemplate(List(section))
    val data = VariadicFormData(Map.empty)
    mkFormModelOptics(formTemplate, data, tpd)
  }

  test("expand - DataRetrieveBased row is expanded into one row per DataRetrieve result") {
    val tableComponent = mkTableCompComponent(dataRetrieveId)
    val tpd = mkDataRetrieveResults(
      dataRetrieveId,
      List(
        Map(employerNameAttr -> "Acme Corp", payeNumberAttr      -> "AA1111"),
        Map(employerNameAttr -> "Smith Holdings", payeNumberAttr -> "BB2222"),
        Map(employerNameAttr -> "Widget Inc", payeNumberAttr     -> "CC3333")
      )
    )

    val optics = buildFmvo(tableComponent, tpd)
    val freeCalculator = optics.formModelVisibilityOptics.freeCalculator

    val result = TableUtils.expand(tableComponent, tableComponent.`type`.asInstanceOf[TableComp], freeCalculator)
    val resultTable = result.`type`.asInstanceOf[TableComp]

    assertEquals(resultTable.rows.size, 3)

    val row0 = resultTable.rows(0)
    assertEquals(
      row0.values.head.value.allInterpolations,
      List(IndexOfDataRetrieveCtx(employerNameCtx, Constant("0")))
    )
    assertEquals(
      row0.values(1).value.allInterpolations,
      List(IndexOfDataRetrieveCtx(payeNumberCtx, Constant("0")))
    )
    assertEquals(
      row0.dynamic,
      Some(Dynamic.DataRetrieveBased(IndexOfDataRetrieveCtx(employerNameCtx, Constant("0"))))
    )

    val row1 = resultTable.rows(1)
    assertEquals(
      row1.values.head.value.allInterpolations,
      List(IndexOfDataRetrieveCtx(employerNameCtx, Constant("1")))
    )
    assertEquals(
      row1.values(1).value.allInterpolations,
      List(IndexOfDataRetrieveCtx(payeNumberCtx, Constant("1")))
    )
    assertEquals(
      row1.dynamic,
      Some(Dynamic.DataRetrieveBased(IndexOfDataRetrieveCtx(employerNameCtx, Constant("1"))))
    )

    val row2 = resultTable.rows(2)
    assertEquals(
      row2.values.head.value.allInterpolations,
      List(IndexOfDataRetrieveCtx(employerNameCtx, Constant("2")))
    )
    assertEquals(
      row2.values(1).value.allInterpolations,
      List(IndexOfDataRetrieveCtx(payeNumberCtx, Constant("2")))
    )
    assertEquals(
      row2.dynamic,
      Some(Dynamic.DataRetrieveBased(IndexOfDataRetrieveCtx(employerNameCtx, Constant("2"))))
    )
  }

  test("expand - DataRetrieveBased row is left unchanged when DataRetrieve returns empty results") {
    val tableComponent = mkTableCompComponent(dataRetrieveId)
    val tpd = ThirdPartyData.empty

    val optics = buildFmvo(tableComponent, tpd)
    val freeCalculator = optics.formModelVisibilityOptics.freeCalculator

    val result = TableUtils.expand(tableComponent, tableComponent.`type`.asInstanceOf[TableComp], freeCalculator)
    val resultTable = result.`type`.asInstanceOf[TableComp]

    assertEquals(resultTable.rows.size, 1)
    assertEquals(
      resultTable.rows.head.values.head.value.allInterpolations,
      List(employerNameCtx)
    )
  }

  test("expand - row without dynamic is left unchanged") {
    val table = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None),
        TableHeadCell(toSmartString("Header 2"), None)
      ),
      rows = List(
        TableValueRow(
          values = List(
            TableValue(toSmartString("Static value 1"), None, None, None),
            TableValue(toSmartString("Static value 2"), None, None, None)
          ),
          includeIf = None,
          dynamic = None
        )
      ),
      summaryValue = toSmartString("Summary")
    )
    val staticComponent = mkFormComponent("staticTable", table)

    val optics = buildFmvo(staticComponent, ThirdPartyData.empty)
    val freeCalculator = optics.formModelVisibilityOptics.freeCalculator

    val result = TableUtils.expand(staticComponent, table, freeCalculator)
    val resultTable = result.`type`.asInstanceOf[TableComp]

    assertEquals(resultTable.rows.size, 1)
    assertEquals(resultTable, table)
  }

  // --- ATLBased Dynamic tests ---

  private val personNameFcId = FormComponentId("personName")
  private val personAgeFcId = FormComponentId("personAge")

  private def mkAtlTableComp: TableComp =
    TableComp(
      header = List(
        TableHeadCell(toSmartString("Person name"), None),
        TableHeadCell(toSmartString("Person age"), None)
      ),
      rows = List(
        TableValueRow(
          values = List(
            TableValue(toSmartStringExpression("{0}", FormCtx(personNameFcId)), None, None, None),
            TableValue(toSmartStringExpression("{0}", FormCtx(personAgeFcId)), None, None, None)
          ),
          includeIf = None,
          dynamic = Some(Dynamic.ATLBased(personNameFcId))
        )
      ),
      summaryValue = toSmartString("View people")
    )

  private def buildAtlFmvo(
    tableComponent: FormComponent,
    atlData: VariadicFormData
  ): FormModelOptics = {
    val personNameFc = mkFormComponent("personName", Value)
    val personAgeFc = mkFormComponent("personAge", Value)
    val atlSection = mkAddToListSection(
      "addAnotherPerson",
      None,
      List(personNameFc, personAgeFc)
    )
    val tableSection = mkSection(List(tableComponent))
    val formTemplate = mkFormTemplate(List(atlSection, tableSection))
    mkFormModelOptics(formTemplate, atlData)
  }

  test("expand - ATLBased row is expanded into one row per AddToList iteration") {
    val table = mkAtlTableComp
    val tableComponent = mkFormComponent("personsTable", table)

    val data = mkVariadicFormData(
      "1_personName"       -> VariadicValue.One("nameValue1"),
      "1_personAge"        -> VariadicValue.One("3"),
      "2_personName"       -> VariadicValue.One("nameValue2"),
      "2_personAge"        -> VariadicValue.One("5"),
      "1_addAnotherPerson" -> VariadicValue.Many(List("0")),
      "2_addAnotherPerson" -> VariadicValue.Many(List("1"))
    )

    val optics = buildAtlFmvo(tableComponent, data)
    val freeCalculator = optics.formModelVisibilityOptics.freeCalculator

    val result = TableUtils.expand(tableComponent, table, freeCalculator)
    val resultTable = result.`type`.asInstanceOf[TableComp]

    assertEquals(resultTable.rows.size, 2)

    val row0 = resultTable.rows(0)
    assertEquals(
      row0.values.head.value.allInterpolations,
      List(FormCtx(FormComponentId("1_personName")))
    )
    assertEquals(
      row0.values(1).value.allInterpolations,
      List(FormCtx(FormComponentId("1_personAge")))
    )
    assertEquals(
      row0.dynamic,
      Some(Dynamic.ATLBased(FormComponentId("1_personName")))
    )

    val row1 = resultTable.rows(1)
    assertEquals(
      row1.values.head.value.allInterpolations,
      List(FormCtx(FormComponentId("2_personName")))
    )
    assertEquals(
      row1.values(1).value.allInterpolations,
      List(FormCtx(FormComponentId("2_personAge")))
    )
    assertEquals(
      row1.dynamic,
      Some(Dynamic.ATLBased(FormComponentId("2_personName")))
    )
  }

  test("expand - ATLBased row with single iteration produces one expanded row") {
    val table = mkAtlTableComp
    val tableComponent = mkFormComponent("personsTable", table)

    val data = mkVariadicFormData(
      "1_personName"       -> VariadicValue.One("nameValue1"),
      "1_personAge"        -> VariadicValue.One("3"),
      "1_addAnotherPerson" -> VariadicValue.Many(List("1"))
    )

    val optics = buildAtlFmvo(tableComponent, data)
    val freeCalculator = optics.formModelVisibilityOptics.freeCalculator

    val result = TableUtils.expand(tableComponent, table, freeCalculator)
    val resultTable = result.`type`.asInstanceOf[TableComp]

    assertEquals(resultTable.rows.size, 1)

    val row0 = resultTable.rows(0)
    assertEquals(
      row0.values.head.value.allInterpolations,
      List(FormCtx(FormComponentId("1_personName")))
    )
    assertEquals(
      row0.values(1).value.allInterpolations,
      List(FormCtx(FormComponentId("1_personAge")))
    )
    assertEquals(
      row0.dynamic,
      Some(Dynamic.ATLBased(FormComponentId("1_personName")))
    )
  }

  test("expand - ATLBased row with three iterations produces three expanded rows") {
    val table = mkAtlTableComp
    val tableComponent = mkFormComponent("personsTable", table)

    val data = mkVariadicFormData(
      "1_personName"       -> VariadicValue.One("nameValue1"),
      "1_personAge"        -> VariadicValue.One("3"),
      "2_personName"       -> VariadicValue.One("nameValue2"),
      "2_personAge"        -> VariadicValue.One("5"),
      "3_personName"       -> VariadicValue.One("nameValue3"),
      "3_personAge"        -> VariadicValue.One("2"),
      "1_addAnotherPerson" -> VariadicValue.Many(List("0")),
      "2_addAnotherPerson" -> VariadicValue.Many(List("0")),
      "3_addAnotherPerson" -> VariadicValue.Many(List("1"))
    )

    val optics = buildAtlFmvo(tableComponent, data)
    val freeCalculator = optics.formModelVisibilityOptics.freeCalculator

    val result = TableUtils.expand(tableComponent, table, freeCalculator)
    val resultTable = result.`type`.asInstanceOf[TableComp]

    assertEquals(resultTable.rows.size, 3)

    for (i <- 0 until 3) {
      val row = resultTable.rows(i)
      val idx = i + 1
      assertEquals(
        row.values.head.value.allInterpolations,
        List(FormCtx(FormComponentId(s"${idx}_personName")))
      )
      assertEquals(
        row.values(1).value.allInterpolations,
        List(FormCtx(FormComponentId(s"${idx}_personAge")))
      )
      assertEquals(
        row.dynamic,
        Some(Dynamic.ATLBased(FormComponentId(s"${idx}_personName")))
      )
    }
  }
}
