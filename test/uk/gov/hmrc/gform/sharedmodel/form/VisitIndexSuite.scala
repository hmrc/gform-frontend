/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.form

import munit.FunSuite
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, SectionNumber, TaskNumber, TaskSectionNumber, TemplateSectionIndex }

class VisitIndexSuite extends FunSuite {

  test("Remove atl iteration from visitindex, nothing to remove") {

    val visitIndex = VisitIndex.Classic(
      Set(
        SectionNumber.Classic.NormalPage(TemplateSectionIndex(0)),
        SectionNumber.Classic.RepeatedPage(TemplateSectionIndex(1), 0),
        SectionNumber.Classic.RepeatedPage(TemplateSectionIndex(1), 1),
        SectionNumber.Classic.RepeatedPage(TemplateSectionIndex(1), 2),
        SectionNumber.Classic.NormalPage(TemplateSectionIndex(1))
      )
    )
    val result = visitIndex.removeIteration(TemplateSectionIndex(0), 0, false, None)

    assertEquals(result, visitIndex)
  }

  test("Remove atl iteration from visitIndex (Classic)") {

    val normalIndex0 = TemplateSectionIndex(0)
    val atlIndex = TemplateSectionIndex(1)
    val normalIndex2 = TemplateSectionIndex(2)

    val visitIndex = VisitIndex.Classic(
      Set(
        SectionNumber.Classic.NormalPage(normalIndex0),
        SectionNumber.Classic.AddToListPage.DefaultPage(atlIndex),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 0),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 2),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 1),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 2, 0),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 2, 1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 2, 2),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 2, 3),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 2),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 2),
        SectionNumber.Classic.NormalPage(normalIndex2)
      )
    )
    val expectedIfNotLast = VisitIndex.Classic(
      Set(
        SectionNumber.Classic.NormalPage(normalIndex0),
        SectionNumber.Classic.AddToListPage.DefaultPage(atlIndex),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 0),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 2),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 3),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 1),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 1),
        SectionNumber.Classic.NormalPage(normalIndex2)
      )
    )
    val expectedIfLast = VisitIndex.Classic(
      Set(
        SectionNumber.Classic.NormalPage(normalIndex0),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 0),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 2),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 3),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 1),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 1),
        SectionNumber.Classic.NormalPage(normalIndex2)
      )
    )

    val result1IfNotLast = visitIndex.removeIteration(atlIndex, 1, false, None)
    val result1IfLast = visitIndex.removeIteration(atlIndex, 1, true, None)

    assertEquals(result1IfNotLast, expectedIfNotLast)
    assertEquals(result1IfLast, expectedIfLast)

    val expected2IfNotLast = VisitIndex.Classic(
      Set(
        SectionNumber.Classic.NormalPage(normalIndex0),
        SectionNumber.Classic.AddToListPage.DefaultPage(atlIndex),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 0),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 2),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 1),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 1),
        SectionNumber.Classic.NormalPage(normalIndex2)
      )
    )

    val result2IfLast = visitIndex.removeIteration(atlIndex, 2, false, None)

    assertEquals(result2IfLast, expected2IfNotLast)

    val result3IfLast = visitIndex.removeIteration(atlIndex, 3, false, None)

    assertEquals(result3IfLast, visitIndex)
  }

  test("Remove atl iteration from visitIndex (TaskList)") {

    val normalIndex0 = TemplateSectionIndex(0)
    val atlIndex = TemplateSectionIndex(1)
    val normalIndex2 = TemplateSectionIndex(2)
    val coordinates = Coordinates(TaskSectionNumber(1), TaskNumber(2))

    val visitIndex = VisitIndex.TaskList(
      Map(
        coordinates ->
          Set(
            SectionNumber.Classic.NormalPage(normalIndex0),
            SectionNumber.Classic.AddToListPage.DefaultPage(atlIndex),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 0),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 1),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 2),
            SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 1),
            SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 1),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 2, 0),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 2, 1),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 2, 2),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 2, 3),
            SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 2),
            SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 2),
            SectionNumber.Classic.NormalPage(normalIndex2)
          )
      )
    )
    val expectedIfNotLast = VisitIndex.TaskList(
      Map(
        coordinates ->
          Set(
            SectionNumber.Classic.NormalPage(normalIndex0),
            SectionNumber.Classic.AddToListPage.DefaultPage(atlIndex),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 0),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 1),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 2),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 3),
            SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 1),
            SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 1),
            SectionNumber.Classic.NormalPage(normalIndex2)
          )
      )
    )
    val expectedIfLast = VisitIndex.TaskList(
      Map(
        coordinates ->
          Set(
            SectionNumber.Classic.NormalPage(normalIndex0),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 0),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 1),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 2),
            SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 3),
            SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 1),
            SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 1),
            SectionNumber.Classic.NormalPage(normalIndex2)
          )
      )
    )

    val result1IfNotLast = visitIndex.removeIteration(atlIndex, 1, false, Some(coordinates))
    val result1IfLast = visitIndex.removeIteration(atlIndex, 1, true, Some(coordinates))

    assertEquals(result1IfNotLast, expectedIfNotLast)
    assertEquals(result1IfLast, expectedIfLast)

    val expected2IfNotLast = VisitIndex.TaskList(
      Map(
        coordinates -> Set(
          SectionNumber.Classic.NormalPage(normalIndex0),
          SectionNumber.Classic.AddToListPage.DefaultPage(atlIndex),
          SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 0),
          SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 1),
          SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 2),
          SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 1),
          SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 1),
          SectionNumber.Classic.NormalPage(normalIndex2)
        )
      )
    )

    val result2IfLast = visitIndex.removeIteration(atlIndex, 2, false, Some(coordinates))

    assertEquals(result2IfLast, expected2IfNotLast)

    val result3IfLast = visitIndex.removeIteration(atlIndex, 3, false, Some(coordinates))

    assertEquals(result3IfLast, visitIndex)
  }

  test("Remove atl iteration from visitIndex with multiple atls (Classic)") {

    val normalIndex0 = TemplateSectionIndex(0)
    val atlIndex1 = TemplateSectionIndex(1)
    val normalIndex2 = TemplateSectionIndex(2)
    val atlIndex3 = TemplateSectionIndex(3)
    val normalIndex4 = TemplateSectionIndex(4)

    val visitIndex = VisitIndex.Classic(
      Set(
        SectionNumber.Classic.NormalPage(normalIndex0),
        SectionNumber.Classic.AddToListPage.DefaultPage(atlIndex1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex1, 1, 0),
        SectionNumber.Classic.AddToListPage.Page(atlIndex1, 1, 1),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex1, 1),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex1, 1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex1, 2, 0),
        SectionNumber.Classic.AddToListPage.Page(atlIndex1, 2, 1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex1, 2, 2),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex1, 2),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex1, 2),
        SectionNumber.Classic.NormalPage(normalIndex2),
        SectionNumber.Classic.AddToListPage.DefaultPage(atlIndex3),
        SectionNumber.Classic.AddToListPage.Page(atlIndex3, 1, 0),
        SectionNumber.Classic.AddToListPage.Page(atlIndex3, 1, 1),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex3, 1),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex3, 1),
        SectionNumber.Classic.NormalPage(normalIndex4)
      )
    )
    val expectedIfLast = VisitIndex.Classic(
      Set(
        SectionNumber.Classic.NormalPage(normalIndex0),
        SectionNumber.Classic.AddToListPage.DefaultPage(atlIndex1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex1, 1, 0),
        SectionNumber.Classic.AddToListPage.Page(atlIndex1, 1, 1),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex1, 1),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex1, 1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex1, 2, 0),
        SectionNumber.Classic.AddToListPage.Page(atlIndex1, 2, 1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex1, 2, 2),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex1, 2),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex1, 2),
        SectionNumber.Classic.NormalPage(normalIndex2),
        SectionNumber.Classic.NormalPage(normalIndex4)
      )
    )

    val result2IfLast = visitIndex.removeIteration(atlIndex3, 1, true, None)

    assertEquals(result2IfLast, expectedIfLast)
  }

  test("Remove atl iteration from visitIndex (Classic) (2)") {

    val normalIndex0 = TemplateSectionIndex(0)
    val atlIndex = TemplateSectionIndex(1)
    val normalIndex2 = TemplateSectionIndex(2)

    val visitIndex = VisitIndex.Classic(
      Set(
        SectionNumber.Classic.NormalPage(normalIndex0),
        SectionNumber.Classic.AddToListPage.DefaultPage(atlIndex),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 0),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 1),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 2, 0),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 2),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 2),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 3, 0),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 3),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 3),
        SectionNumber.Classic.NormalPage(normalIndex2)
      )
    )
    val expectedIfLast = VisitIndex.Classic(
      Set(
        SectionNumber.Classic.NormalPage(normalIndex0),
        SectionNumber.Classic.AddToListPage.DefaultPage(atlIndex),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 1, 0),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 1),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 1),
        SectionNumber.Classic.AddToListPage.Page(atlIndex, 2, 0),
        SectionNumber.Classic.AddToListPage.CyaPage(atlIndex, 2),
        SectionNumber.Classic.AddToListPage.RepeaterPage(atlIndex, 2),
        SectionNumber.Classic.NormalPage(normalIndex2)
      )
    )

    val result1 = visitIndex.removeIteration(atlIndex, 1, false, None)
    val result2 = visitIndex.removeIteration(atlIndex, 2, false, None)
    val result3 = visitIndex.removeIteration(atlIndex, 3, false, None)

    assertEquals(result1, expectedIfLast)
    assertEquals(result2, expectedIfLast)
    assertEquals(result3, expectedIfLast)

  }
}
