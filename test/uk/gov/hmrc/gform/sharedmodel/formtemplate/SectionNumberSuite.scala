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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import munit.FunSuite
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber._

class SectionNumberSuite extends FunSuite {

  val parseSectionNumbers: List[(String, SectionNumber)] =
    List(
      ("n0"               -> Classic.NormalPage(TemplateSectionIndex(0))),
      ("n9"               -> Classic.NormalPage(TemplateSectionIndex(9))),
      ("n10"              -> Classic.NormalPage(TemplateSectionIndex(10))),
      ("n100"             -> Classic.NormalPage(TemplateSectionIndex(100))),
      ("ap0.0.0"          -> Classic.AddToListPage.Page(TemplateSectionIndex(0), 0, 0)),
      ("ap1000.1001.1002" -> Classic.AddToListPage.Page(TemplateSectionIndex(1000), 1001, 1002)),
      ("ad0"              -> Classic.AddToListPage.DefaultPage(TemplateSectionIndex(0))),
      ("ac0.0"            -> Classic.AddToListPage.CyaPage(TemplateSectionIndex(0), 0)),
      ("ar0.0"            -> Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(0), 0)),
      ("r0.0"             -> Classic.RepeatedPage(TemplateSectionIndex(0), 0)),
      ("r100.1001"        -> Classic.RepeatedPage(TemplateSectionIndex(100), 1001))
    )

  test("parse sectionNumbers") {
    parseSectionNumbers.foreach { case (toParse, expectedSectionNumber) =>
      val res = SectionNumber.parse(toParse)
      assertEquals(res, Some(expectedSectionNumber))
    }
  }

  private val EQ = 0
  private val GT = 1
  private val LT = -1

  val compareSectionNumbers: List[(SectionNumber, SectionNumber, Int)] =
    List(
      (Classic.NormalPage(TemplateSectionIndex(0)), Classic.NormalPage(TemplateSectionIndex(0)), EQ),
      (Classic.NormalPage(TemplateSectionIndex(0)), Classic.NormalPage(TemplateSectionIndex(1)), LT),
      (Classic.NormalPage(TemplateSectionIndex(0)), Classic.AddToListPage.Page(TemplateSectionIndex(1), 5, 5), LT),
      (Classic.NormalPage(TemplateSectionIndex(0)), Classic.AddToListPage.DefaultPage(TemplateSectionIndex(1)), LT),
      (Classic.NormalPage(TemplateSectionIndex(0)), Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 5), LT),
      (Classic.NormalPage(TemplateSectionIndex(0)), Classic.RepeatedPage(TemplateSectionIndex(1), 5), LT),
      (Classic.NormalPage(TemplateSectionIndex(1)), Classic.NormalPage(TemplateSectionIndex(0)), GT),
      (Classic.NormalPage(TemplateSectionIndex(1)), Classic.AddToListPage.Page(TemplateSectionIndex(0), 5, 5), GT),
      (Classic.NormalPage(TemplateSectionIndex(1)), Classic.RepeatedPage(TemplateSectionIndex(0), 5), GT),
      (Classic.AddToListPage.DefaultPage(TemplateSectionIndex(1)), Classic.NormalPage(TemplateSectionIndex(0)), GT),
      (Classic.AddToListPage.Page(TemplateSectionIndex(1), 5, 5), Classic.NormalPage(TemplateSectionIndex(0)), GT),
      (Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 5), Classic.NormalPage(TemplateSectionIndex(0)), GT),
      (Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 5), Classic.NormalPage(TemplateSectionIndex(0)), GT),
      (Classic.RepeatedPage(TemplateSectionIndex(1), 5), Classic.NormalPage(TemplateSectionIndex(0)), GT),
      (Classic.AddToListPage.DefaultPage(TemplateSectionIndex(0)), Classic.NormalPage(TemplateSectionIndex(1)), LT),
      (Classic.AddToListPage.Page(TemplateSectionIndex(0), 5, 5), Classic.NormalPage(TemplateSectionIndex(1)), LT),
      (Classic.AddToListPage.CyaPage(TemplateSectionIndex(0), 5), Classic.NormalPage(TemplateSectionIndex(1)), LT),
      (Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(0), 5), Classic.NormalPage(TemplateSectionIndex(1)), LT),
      (Classic.RepeatedPage(TemplateSectionIndex(0), 5), Classic.NormalPage(TemplateSectionIndex(1)), LT),
      (
        Classic.AddToListPage.DefaultPage(TemplateSectionIndex(1)),
        Classic.RepeatedPage(TemplateSectionIndex(0), 5),
        GT
      ),
      (Classic.AddToListPage.Page(TemplateSectionIndex(1), 5, 5), Classic.RepeatedPage(TemplateSectionIndex(0), 5), GT),
      (Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 5), Classic.RepeatedPage(TemplateSectionIndex(0), 5), GT),
      (
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 5),
        Classic.RepeatedPage(TemplateSectionIndex(0), 5),
        GT
      ),
      (
        Classic.RepeatedPage(TemplateSectionIndex(0), 5),
        Classic.AddToListPage.DefaultPage(TemplateSectionIndex(1)),
        LT
      ),
      (Classic.RepeatedPage(TemplateSectionIndex(0), 5), Classic.AddToListPage.Page(TemplateSectionIndex(1), 5, 5), LT),
      (Classic.RepeatedPage(TemplateSectionIndex(0), 5), Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 5), LT),
      (
        Classic.RepeatedPage(TemplateSectionIndex(0), 5),
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 5),
        LT
      ),
      (Classic.RepeatedPage(TemplateSectionIndex(0), 1), Classic.RepeatedPage(TemplateSectionIndex(0), 1), EQ),
      (Classic.RepeatedPage(TemplateSectionIndex(0), 5), Classic.RepeatedPage(TemplateSectionIndex(0), 1), GT),
      (Classic.RepeatedPage(TemplateSectionIndex(0), 1), Classic.RepeatedPage(TemplateSectionIndex(0), 5), LT),
      (
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 5, 4),
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 5, 4),
        EQ
      ),
      (
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 5, 5),
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 4, 5),
        GT
      ),
      (
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 4, 5),
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 5, 5),
        LT
      ),
      (
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 4, 4),
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 4, 5),
        LT
      ),
      (
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 4, 5),
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 4, 4),
        GT
      ),
      (
        Classic.AddToListPage.DefaultPage(TemplateSectionIndex(1)),
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 0, 0),
        LT
      ),
      (
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 0),
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 0, 0),
        GT
      ),
      (
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 0),
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 0, 0),
        GT
      ),
      (
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 0, 0),
        Classic.AddToListPage.DefaultPage(TemplateSectionIndex(1)),
        GT
      ),
      (
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 0, 0),
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 0),
        LT
      ),
      (
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 0, 0),
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 0),
        LT
      ),
      (
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 0),
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 0),
        EQ
      ),
      (
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 0),
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 1),
        LT
      ),
      (
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 1),
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 0),
        GT
      ),
      (
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 0),
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 0),
        EQ
      ),
      (
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 0),
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 1),
        LT
      ),
      (
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 1),
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 0),
        GT
      ),
      (
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 0),
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 0),
        LT
      ),
      (
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(2), 0),
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 0),
        GT
      ),
      (
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 0),
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(1), 0),
        GT
      ),
      (
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 0),
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(2), 0),
        LT
      ),
      (
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 0),
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 1, 0),
        LT
      ),
      (
        Classic.AddToListPage.Page(TemplateSectionIndex(1), 1, 0),
        Classic.AddToListPage.RepeaterPage(TemplateSectionIndex(1), 0),
        GT
      ),
      (
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(0), 1),
        Classic.AddToListPage.DefaultPage(TemplateSectionIndex(1)),
        LT
      ),
      (
        Classic.AddToListPage.DefaultPage(TemplateSectionIndex(1)),
        Classic.AddToListPage.CyaPage(TemplateSectionIndex(0), 1),
        GT
      )
    )

  test("compare sectionNumbers") {
    compareSectionNumbers.foreach { case (sn1, sn2, expected) =>
      val res = sn1.compare(sn2)

      assertEquals(res, expected)

    }
  }

  val compareSectionNumbersInvalid: List[(SectionNumber, SectionNumber)] =
    List(
      (Classic.NormalPage(TemplateSectionIndex(0)), Classic.AddToListPage.Page(TemplateSectionIndex(0), 5, 5)),
      (Classic.NormalPage(TemplateSectionIndex(0)), Classic.RepeatedPage(TemplateSectionIndex(0), 5)),
      (Classic.AddToListPage.Page(TemplateSectionIndex(0), 5, 5), Classic.NormalPage(TemplateSectionIndex(0))),
      (Classic.RepeatedPage(TemplateSectionIndex(0), 5), Classic.NormalPage(TemplateSectionIndex(0))),
      (Classic.AddToListPage.Page(TemplateSectionIndex(0), 5, 5), Classic.RepeatedPage(TemplateSectionIndex(0), 5)),
      (Classic.RepeatedPage(TemplateSectionIndex(0), 5), Classic.AddToListPage.Page(TemplateSectionIndex(0), 5, 5))
    )

  test("invalid compares") {
    compareSectionNumbersInvalid.foreach { case (sn1, sn2) =>
      intercept[java.lang.Exception] {
        sn1.compare(sn2)
      }
    }
  }
}
