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
import uk.gov.hmrc.gform.Helpers.toSmartStringExpression
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, DataRetrieveCtx, Dynamic, IndexOfDataRetrieveCtx, TableValue, TableValueRow }
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId, SmartString }

class TableUtilsSuite extends FunSuite {

  private val employerNameCtx = DataRetrieveCtx(
    DataRetrieveId("individualsEmployments"),
    DataRetrieve.Attribute("employerName")
  )

  private val payeNumberCtx = DataRetrieveCtx(
    DataRetrieveId("individualsEmployments"),
    DataRetrieve.Attribute("payeNumber")
  )

  private def mkSmartStringWithDataRetrieve(label: String, ctx: DataRetrieveCtx): SmartString =
    toSmartStringExpression(label, ctx)

  private def mkTableValue(label: String, ctx: DataRetrieveCtx): TableValue =
    TableValue(mkSmartStringWithDataRetrieve(label, ctx), None, None, None)

  test("expandDataRetrieveTableValue - rewrites DataRetrieveCtx to IndexOfDataRetrieveCtx with correct index") {
    val tableValue = mkTableValue("employer", employerNameCtx)

    val result0 = TableUtils.expandDataRetrieveTableValue(0, tableValue)
    val result1 = TableUtils.expandDataRetrieveTableValue(1, tableValue)

    val expectedInterpolations0 = List(IndexOfDataRetrieveCtx(employerNameCtx, Constant("0")))
    assertEquals(result0.value.allInterpolations, expectedInterpolations0)

    val expectedInterpolations1 = List(IndexOfDataRetrieveCtx(employerNameCtx, Constant("1")))
    assertEquals(result1.value.allInterpolations, expectedInterpolations1)
  }

  test("updateDataRetrieveTableValueRow - expands all values and updates dynamic for each index") {
    val dynamic = Dynamic.DataRetrieveBased(IndexOfDataRetrieveCtx(employerNameCtx, Constant("0")))

    val row = TableValueRow(
      values = List(
        mkTableValue("employer", employerNameCtx),
        mkTableValue("paye", payeNumberCtx)
      ),
      includeIf = None,
      dynamic = Some(dynamic)
    )

    val result0 = TableUtils.updateDataRetrieveTableValueRow(0, row)
    val result1 = TableUtils.updateDataRetrieveTableValueRow(1, row)

    assertEquals(
      result0.values.head.value.allInterpolations,
      List(IndexOfDataRetrieveCtx(employerNameCtx, Constant("0")))
    )
    assertEquals(
      result0.values(1).value.allInterpolations,
      List(IndexOfDataRetrieveCtx(payeNumberCtx, Constant("0")))
    )
    assertEquals(
      result0.dynamic,
      Some(Dynamic.DataRetrieveBased(IndexOfDataRetrieveCtx(employerNameCtx, Constant("0"))))
    )
    assertEquals(
      result1.values.head.value.allInterpolations,
      List(IndexOfDataRetrieveCtx(employerNameCtx, Constant("1")))
    )
    assertEquals(
      result1.values(1).value.allInterpolations,
      List(IndexOfDataRetrieveCtx(payeNumberCtx, Constant("1")))
    )
    assertEquals(
      result1.dynamic,
      Some(Dynamic.DataRetrieveBased(IndexOfDataRetrieveCtx(employerNameCtx, Constant("1"))))
    )
  }
}
