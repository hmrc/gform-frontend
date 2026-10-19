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

import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Dynamic, IncludeIf, IsFalse, IsTrue, TableComp, TableHeadCell, TableValue, TableValueRow }
import munit.FunSuite
import org.mockito.Mockito.{ mock, when }
import play.api.test.Helpers.stubMessages
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.mkFormComponent
import uk.gov.hmrc.gform.models.OptionDataUtils
import uk.gov.hmrc.gform.recalculation.{ Calculator, EvaluationStatus }
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Choice, FormComponentId, OptionData, OptionDataValue, Radio, Vertical }

class SectionRenderingServiceSuite extends FunSuite {
  test("remaining choices exclude other items but retain the current item's stored value") {
    implicit val messages: play.api.i18n.Messages = stubMessages()
    val calculator = mock(classOf[Calculator])
    val options = List("warehousekeeper", "manufacturer", "importer").map { value =>
      OptionData.ValueBased(toSmartString(value), None, None, None, OptionDataValue.StringBased(value), None, None)
    }
    val choice = Choice(Radio, options, Vertical, Nil, None, None, toSmartString("or"), None, None, true, false, None)
    val component = mkFormComponent("3_approvalType", choice)
    when(calculator.allModelComponentIds(component.modelComponentId)).thenReturn(
      List(
        FormComponentId("1_approvalType").modelComponentId -> EvaluationStatus.OptionResult(List("warehousekeeper")),
        FormComponentId("2_approvalType").modelComponentId -> EvaluationStatus.OptionResult(List("manufacturer")),
        component.modelComponentId                         -> EvaluationStatus.OptionResult(List("importer"))
      )
    )
    val remaining = OptionDataUtils.remainingOptions(component, choice, calculator, _.rawDefaultValue(LangADT.En))
    assertEquals(remaining.map(_._1), List(options(2)))
  }

  test("remaining choices collapse duplicate labels before determining the last option") {
    implicit val messages: play.api.i18n.Messages = stubMessages()
    val calculator = mock(classOf[Calculator])
    val options = List("first", "second").map { value =>
      OptionData.ValueBased(
        toSmartString("Remaining approval"),
        None,
        None,
        None,
        OptionDataValue.StringBased(value),
        None,
        None
      )
    }
    val choice = Choice(Radio, options, Vertical, Nil, None, None, toSmartString("or"), None, None, true, true, None)
    val component = mkFormComponent("approvalType", choice)
    when(calculator.allModelComponentIds(component.modelComponentId)).thenReturn(Nil)
    val remaining = OptionDataUtils.remainingOptions(component, choice, calculator, _.rawDefaultValue(LangADT.En))
    assertEquals(remaining.map(_._1), List(options.head))
  }

  private def isVisibleValueRow(
    row: TableValueRow
  ): Boolean = row.includeIf.fold(true)(includeIf => includeIf === IncludeIf(IsTrue))

  private val visible = Option.empty[IncludeIf]
  private val invisible = Some(IncludeIf(IsFalse))

  test("TableComp normalisation - do nothing if includeIf are not present") {

    val table = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None),
        TableHeadCell(toSmartString("Header 2"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue("Row 1, Column 1", Some(2)),
            mkTableValue("Row 1, Column 2")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 2, Column 2")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val res = SectionRenderingService.normaliseTableComp(table, isVisibleValueRow)

    assertEquals(res, table)
  }

  test("TableComp normalisation - handle case when row defining rospan is hidden") {

    val table = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None),
        TableHeadCell(toSmartString("Header 2"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue(
              "Row 1, Column 1",
              Some(2)
            )
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue(
              "Row 2, Column 1"
            )
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None),
        TableHeadCell(toSmartString("Header 2"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue("Row 1, Column 1", Some(2))
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue(
              "Row 1, Column 1",
              None // <- Since previous page is invisible, this TableValue was cloned form Row 1 into Row 2 and rowspan has been reduced by 1
            ),
            mkTableValue("Row 2, Column 1")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val res = SectionRenderingService.normaliseTableComp(table, isVisibleValueRow)
    assertEquals(res, expectedTable)
  }

  test("TableComp normalisation - handle case when rows contributing to rowspan are hidden") {

    val table = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue("Row 1, Column 1", Some(2))
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 2, Column 1")
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 3, Column 1")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue(
              "Row 1, Column 1",
              None // <- Since next row is invisible this was reduced
            )
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 2, Column 1")
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 3, Column 1")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val res = SectionRenderingService.normaliseTableComp(table, isVisibleValueRow)
    assertEquals(res, expectedTable)
  }

  test("TableComp normalisation - handle case when rows contributing to rowspan are hidden (2)") {

    val table = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue(
              "Row 1, Column 1",
              Some(3)
            )
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 2, Column 1")
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 3, Column 1")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue(
              "Row 1, Column 1",
              Some(3)
            )
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue(
              "Row 1, Column 1",
              Some(2)
            ),
            mkTableValue("Row 2, Column 1")
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 1, Column 1"),
            mkTableValue("Row 3, Column 1")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val res = SectionRenderingService.normaliseTableComp(table, isVisibleValueRow)
    assertEquals(res, expectedTable)
  }

  test("TableComp normalisation - handle case when rows contributing to rowspan are hidden (2)") {

    // |-----------+---+---+---|
    // | visible   |   |   |   |
    // |-----------+---+---+---|
    // | INVISIBLE | 5 |   |   |
    // |-----------+   +---+---|
    // | visible   |   |   |   |
    // |-----------+   +---+---|
    // | INVISIBLE |   | 4 |   |
    // |-----------+   +   +---|
    // | INVISIBLE |   |   | 3 |
    // |-----------+   +   +   |
    // | visible   |   |   |   |
    // |-----------+---+   +   |
    // | visible   |   |   |   |
    // |-----------+---+---+---|

    val table = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None),
        TableHeadCell(toSmartString("Header 2"), None),
        TableHeadCell(toSmartString("Header 3"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue("Row 1, Column 1"),
            mkTableValue("Row 1, Column 2"),
            mkTableValue("Row 1, Column 3")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 2, Column 1", Some(5)),
            mkTableValue("Row 2, Column 2"),
            mkTableValue("Row 2, Column 3")
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 3, Column 2"),
            mkTableValue("Row 3, Column 3")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 4, Column 2", Some(4))
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 5, Column 3", Some(3))
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 7, Column 1")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None),
        TableHeadCell(toSmartString("Header 2"), None),
        TableHeadCell(toSmartString("Header 3"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue("Row 1, Column 1"),
            mkTableValue("Row 1, Column 2"),
            mkTableValue("Row 1, Column 3")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 2, Column 1", Some(5)),
            mkTableValue("Row 2, Column 2"),
            mkTableValue("Row 2, Column 3")
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 2, Column 1", Some(2)),
            mkTableValue("Row 3, Column 2"),
            mkTableValue("Row 3, Column 3")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 4, Column 2", Some(4))
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 4, Column 2", Some(3)),
            mkTableValue("Row 5, Column 3", Some(3))
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 4, Column 2", Some(2)),
            mkTableValue("Row 5, Column 3", Some(2))
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 7, Column 1")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val res = SectionRenderingService.normaliseTableComp(table, isVisibleValueRow)
    assertEquals(res, expectedTable)
  }

  test("TableComp normalisation - handle case when rows contributing to rowspan are hidden (3)") {

    // |-----------+---+----+---+---+---|
    // | visible   |   |    |   |   |   |
    // |-----------+---+----+---+---+---|
    // | INVISIBLE |   | 3,3        |   |
    // |-----------+---+            +---|
    // | visible   |   |            |   |
    // |-----------+---+            +---|
    // | visible   |   |            |   |
    // |-----------+---+----+---+---+---|
    // | visible   |   |    |   |   |   |
    // |-----------+---+----+---+---+---|

    val table = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None),
        TableHeadCell(toSmartString("Header 2"), None),
        TableHeadCell(toSmartString("Header 3"), None),
        TableHeadCell(toSmartString("Header 4"), None),
        TableHeadCell(toSmartString("Header 5"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue("Row 1, Column 1"),
            mkTableValue("Row 1, Column 2"),
            mkTableValue("Row 1, Column 3"),
            mkTableValue("Row 1, Column 4"),
            mkTableValue("Row 1, Column 5")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 2, Column 1"),
            mkTableValue("Row 2, Column 2", Some(3), Some(3)),
            mkTableValue("Row 2, Column 5")
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 3, Column 1"),
            mkTableValue("Row 3, Column 5")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 4, Column 1"),
            mkTableValue("Row 4, Column 5")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 5, Column 1"),
            mkTableValue("Row 5, Column 2"),
            mkTableValue("Row 5, Column 3"),
            mkTableValue("Row 5, Column 4"),
            mkTableValue("Row 5, Column 5")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None),
        TableHeadCell(toSmartString("Header 2"), None),
        TableHeadCell(toSmartString("Header 3"), None),
        TableHeadCell(toSmartString("Header 4"), None),
        TableHeadCell(toSmartString("Header 5"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue("Row 1, Column 1"),
            mkTableValue("Row 1, Column 2"),
            mkTableValue("Row 1, Column 3"),
            mkTableValue("Row 1, Column 4"),
            mkTableValue("Row 1, Column 5")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 2, Column 1"),
            mkTableValue("Row 2, Column 2", Some(3), Some(3)),
            mkTableValue("Row 2, Column 5")
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 3, Column 1"),
            mkTableValue("Row 2, Column 2", Some(2), Some(3)),
            mkTableValue("Row 3, Column 5")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 4, Column 1"),
            mkTableValue("Row 4, Column 5")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 5, Column 1"),
            mkTableValue("Row 5, Column 2"),
            mkTableValue("Row 5, Column 3"),
            mkTableValue("Row 5, Column 4"),
            mkTableValue("Row 5, Column 5")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val res = SectionRenderingService.normaliseTableComp(table, isVisibleValueRow)
    assertEquals(res, expectedTable)
  }

  test("TableComp normalisation - handle case when rows contributing to rowspan are hidden (4)") {

    // |-----------+---+---|
    // | visible   |   |   |
    // |-----------+---+---|
    // | INVISIBLE |   | 3 |
    // |-----------+---+   |
    // | INVISIBLE |   |   |
    // |-----------+---+   |
    // | INVISIBLE |   |   |
    // |-----------+---+---|
    // | visible   |   |   |
    // |-----------+---+---|

    val table = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None),
        TableHeadCell(toSmartString("Header 2"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue("Row 1, Column 1"),
            mkTableValue("Row 1, Column 2")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 2, Column 1"),
            mkTableValue("Row 2, Column 2", Some(3))
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 3, Column 1")
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 4, Column 1")
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 5, Column 1"),
            mkTableValue("Row 5, Column 2")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None),
        TableHeadCell(toSmartString("Header 2"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue("Row 1, Column 1"),
            mkTableValue("Row 1, Column 2")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 2, Column 1"),
            mkTableValue("Row 2, Column 2", Some(3))
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 3, Column 1"),
            mkTableValue("Row 2, Column 2", Some(2))
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 4, Column 1"),
            mkTableValue("Row 2, Column 2", Some(1))
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 5, Column 1"),
            mkTableValue("Row 5, Column 2")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val res = SectionRenderingService.normaliseTableComp(table, isVisibleValueRow)
    assertEquals(res, expectedTable)
  }

  test("TableComp normalisation - handle case when rows contributing to rowspan are hidden (5)") {

    // |-----------+---+---|
    // | visible   |   |   |
    // |-----------+---+---|
    // | INVISIBLE |   | 3 |
    // |-----------+---+   |
    // | INVISIBLE |   |   |
    // |-----------+---+   |
    // | visible   |   |   |
    // |-----------+---+---|
    // | visible   |   |   |
    // |-----------+---+---|

    val table = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None),
        TableHeadCell(toSmartString("Header 2"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue("Row 1, Column 1"),
            mkTableValue("Row 1, Column 2")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 2, Column 1"),
            mkTableValue("Row 2, Column 2", Some(3))
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 3, Column 1")
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 4, Column 1")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 5, Column 1"),
            mkTableValue("Row 5, Column 2")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val expectedTable = TableComp(
      header = List(
        TableHeadCell(toSmartString("Header 1"), None),
        TableHeadCell(toSmartString("Header 2"), None)
      ),
      rows = List(
        TableValueRow(
          List(
            mkTableValue("Row 1, Column 1"),
            mkTableValue("Row 1, Column 2")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 2, Column 1"),
            mkTableValue("Row 2, Column 2", Some(3))
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 3, Column 1"),
            mkTableValue("Row 2, Column 2", Some(2))
          ),
          invisible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 4, Column 1"),
            mkTableValue("Row 2, Column 2")
          ),
          visible,
          Option.empty[Dynamic]
        ),
        TableValueRow(
          List(
            mkTableValue("Row 5, Column 1"),
            mkTableValue("Row 5, Column 2")
          ),
          visible,
          Option.empty[Dynamic]
        )
      ),
      summaryValue = toSmartString("Summary value")
    )

    val res = SectionRenderingService.normaliseTableComp(table, isVisibleValueRow)
    assertEquals(res, expectedTable)
  }

  def mkTableValue(label: String, rowspan: Option[Int] = None, colspan: Option[Int] = None): TableValue = TableValue(
    toSmartString(label),
    None,
    colspan,
    rowspan
  )
}
