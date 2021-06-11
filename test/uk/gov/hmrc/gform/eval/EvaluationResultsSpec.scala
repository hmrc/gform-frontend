/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.eval

import org.scalatest.prop.TableDrivenPropertyChecks
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.eval.ExpressionResult.{ DateResult, NumberResult, PeriodResult, StringResult }
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.ExpandUtils.toModelComponentId
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OffsetUnit.{ Day, Month, Year }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Add, Constant, Count, DateCtx, DateExprWithOffset, DateFormCtxVar, DateValueExpr, DaysProp, Else, ExactDateExprValue, FormComponentId, FormCtx, MonthsProp, OffsetYMD, PeriodFun, PeriodFunExt, PeriodValue, SumProp, TotalMonthsProp, YearsProp }
import uk.gov.hmrc.gform.sharedmodel.{ VariadicFormData, VariadicValue }

import java.time.{ LocalDate, Period }

class EvaluationResultsSpec extends Spec with TableDrivenPropertyChecks {

  "evalExpr - type dateString" should "evaluate expressions" in {

    val recData = RecData[OutOfDate](
      VariadicFormData.create(
        (toModelComponentId("dateFieldId1-year"), VariadicValue.One("1970")),
        (toModelComponentId("dateFieldId1-month"), VariadicValue.One("1")),
        (toModelComponentId("dateFieldId1-day"), VariadicValue.One("11")),
        (toModelComponentId("dateFieldId2-year"), VariadicValue.One("1971")),
        (toModelComponentId("dateFieldId2-month"), VariadicValue.One("1")),
        (toModelComponentId("dateFieldId2-day"), VariadicValue.One("11")),
        (toModelComponentId("textFieldEmpty"), VariadicValue.One("")),
        (toModelComponentId("textField"), VariadicValue.One("textFieldValue"))
      )
    )

    val table = Table(
      ("typeInfo", "recData", "expectedResult"),
      (
        TypeInfo(
          DateCtx(DateFormCtxVar(FormCtx(FormComponentId("dateFieldId1")))),
          StaticTypeData(ExprType.dateString, None)
        ),
        recData,
        DateResult(LocalDate.of(1970, 1, 11))
      ),
      (
        TypeInfo(FormCtx(FormComponentId("dateFieldId1")), StaticTypeData(ExprType.dateString, None)),
        recData,
        DateResult(LocalDate.of(1970, 1, 11))
      ),
      (
        TypeInfo(
          Else(FormCtx(FormComponentId("dateFieldId1")), FormCtx(FormComponentId("dateFieldId2"))),
          StaticTypeData(ExprType.dateString, None)
        ),
        recData,
        DateResult(LocalDate.of(1970, 1, 11))
      ),
      (
        TypeInfo(
          Else(FormCtx(FormComponentId("textFieldEmpty")), FormCtx(FormComponentId("dateFieldId2"))),
          StaticTypeData(ExprType.dateString, None)
        ),
        recData,
        DateResult(LocalDate.of(1971, 1, 11))
      ),
      (
        TypeInfo(
          Else(FormCtx(FormComponentId("textFieldEmpty")), Constant("someConstant")),
          StaticTypeData(ExprType.dateString, None)
        ),
        recData,
        StringResult("someConstant")
      )
    )

    forAll(table) { (typeInfo: TypeInfo, recData: RecData[OutOfDate], expectedResult: ExpressionResult) =>
      EvaluationResults.empty.evalExpr(typeInfo, recData, evaluationContext) shouldBe expectedResult
    }
  }

  "evalExpr - type string" should "evaluate expressions" in {

    val table = Table(
      ("typeInfo", "recData", "expectedResult", "scenario"),
      (
        TypeInfo(
          DateCtx(DateFormCtxVar(FormCtx(FormComponentId("dateFieldId1")))),
          StaticTypeData(ExprType.string, None)
        ),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("dateFieldId1-year"), VariadicValue.One("1970")),
            (toModelComponentId("dateFieldId1-month"), VariadicValue.One("1")),
            (toModelComponentId("dateFieldId1-day"), VariadicValue.One("11")),
            (toModelComponentId("dateFieldId1"), VariadicValue.One("11 January 1970"))
          )
        ),
        StringResult("11 January 1970"),
        "DateCtx expression converted to type 'string'"
      ),
      (
        TypeInfo(FormCtx(FormComponentId("dateFieldId1")), StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("dateFieldId1-year"), VariadicValue.One("1970")),
            (toModelComponentId("dateFieldId1-month"), VariadicValue.One("1")),
            (toModelComponentId("dateFieldId1-day"), VariadicValue.One("11")),
            (toModelComponentId("dateFieldId1"), VariadicValue.One("11 January 1970"))
          )
        ),
        StringResult("11 January 1970"),
        "FormCtx Expression converted to type 'string'"
      ),
      (
        TypeInfo(Count(FormComponentId("addToListQuestion")), StaticTypeData(ExprType.string, None)),
        RecData[OutOfDate](
          VariadicFormData.create(
            (toModelComponentId("1_addToListQuestion"), VariadicValue.One("0")),
            (toModelComponentId("2_addToListQuestion"), VariadicValue.One("1")),
            (toModelComponentId("1_addToListField1"), VariadicValue.One("Hello")),
            (toModelComponentId("2_addToListField1"), VariadicValue.One("World"))
          )
        ),
        StringResult("2"),
        "Eval Count(addToListComponent) as string"
      )
    )
    forAll(table) { (typeInfo: TypeInfo, recData: RecData[OutOfDate], expectedResult: ExpressionResult, _) =>
      EvaluationResults.empty.evalExpr(typeInfo, recData, evaluationContext) shouldBe expectedResult
    }
  }

  "evalExpr - type number" should "evaluate expressions" in {
    val recData = RecData[OutOfDate](
      VariadicFormData.create(
        (toModelComponentId("1_addToListQuestion"), VariadicValue.One("0")),
        (toModelComponentId("2_addToListQuestion"), VariadicValue.One("1")),
        (toModelComponentId("1_addToListField1"), VariadicValue.One("Hello")),
        (toModelComponentId("2_addToListField1"), VariadicValue.One("World"))
      )
    )

    val table = Table(
      ("typeInfo", "recData", "expectedResult", "scenario"),
      (
        TypeInfo(Count(FormComponentId("addToListQuestion")), StaticTypeData(ExprType.number, None)),
        recData,
        NumberResult(2),
        "Ref to AddToList count in number field"
      )
    )
    forAll(table) { (typeInfo: TypeInfo, recData: RecData[OutOfDate], expectedResult: ExpressionResult, _) =>
      EvaluationResults.empty.evalExpr(typeInfo, recData, evaluationContext) shouldBe expectedResult
    }
  }

  "evalExpr - type period" should "evaluate expressions" in {

    val recData = RecData[OutOfDate](
      VariadicFormData.create(
        (toModelComponentId("startDate1-year"), VariadicValue.One("2000")),
        (toModelComponentId("startDate1-month"), VariadicValue.One("1")),
        (toModelComponentId("startDate1-day"), VariadicValue.One("1")),
        (toModelComponentId("endDate1-year"), VariadicValue.One("2001")),
        (toModelComponentId("endDate1-month"), VariadicValue.One("2")),
        (toModelComponentId("endDate1-day"), VariadicValue.One("2")),
        (toModelComponentId("startDate2-year"), VariadicValue.One("2002")),
        (toModelComponentId("startDate2-month"), VariadicValue.One("1")),
        (toModelComponentId("startDate2-day"), VariadicValue.One("1")),
        (toModelComponentId("endDate2-year"), VariadicValue.One("2003")),
        (toModelComponentId("endDate2-month"), VariadicValue.One("10")),
        (toModelComponentId("endDate2-day"), VariadicValue.One("1"))
      )
    )

    val table = Table(
      ("typeInfo", "recData", "expectedResult"),
      (
        TypeInfo(
          PeriodFun(
            DateCtx(DateValueExpr(ExactDateExprValue(2001, 1, 1))),
            DateCtx(
              DateExprWithOffset(
                DateValueExpr(ExactDateExprValue(2001, 1, 1)),
                OffsetYMD(List(Year(1), Month(1), Day(1)))
              )
            )
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(Period.of(1, 1, 1))
      ),
      (
        TypeInfo(
          PeriodFun(
            DateCtx(
              DateExprWithOffset(DateFormCtxVar(FormCtx(FormComponentId("startDate1"))), OffsetYMD(List(Year(1))))
            ),
            DateCtx(DateValueExpr(ExactDateExprValue(2003, 2, 2)))
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(Period.of(2, 1, 1))
      ),
      (
        TypeInfo(
          PeriodFun(
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
            DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate1"))))
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(Period.of(1, 1, 1))
      ),
      (
        TypeInfo(
          PeriodFunExt(
            PeriodFun(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate1"))))
            ),
            YearsProp
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        NumberResult(1)
      ),
      (
        TypeInfo(
          Add(
            PeriodFun(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate1"))))
            ),
            PeriodFun(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate2")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate2"))))
            )
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(Period.of(2, 10, 1))
      ),
      (
        TypeInfo(
          Add(
            PeriodFun(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate1"))))
            ),
            PeriodValue("P1Y")
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(Period.of(2, 1, 1))
      ),
      (
        TypeInfo(
          Else(
            PeriodFun(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate3")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate3"))))
            ),
            PeriodFun(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate1")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate1"))))
            )
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(Period.of(1, 1, 1))
      )
    )

    forAll(table) { (typeInfo: TypeInfo, recData: RecData[OutOfDate], expectedResult: ExpressionResult) =>
      EvaluationResults.empty.evalExpr(typeInfo, recData, evaluationContext) shouldBe expectedResult
    }
  }

  it should "evaluate group expressions" in {
    val recData = RecData[OutOfDate](
      VariadicFormData.create(
        //group 1
        (toModelComponentId("1_startDate-year"), VariadicValue.One("2000")),
        (toModelComponentId("1_startDate-month"), VariadicValue.One("1")),
        (toModelComponentId("1_startDate-day"), VariadicValue.One("1")),
        (toModelComponentId("1_endDate-year"), VariadicValue.One("2000")),
        (toModelComponentId("1_endDate-month"), VariadicValue.One("10")),
        (toModelComponentId("1_endDate-day"), VariadicValue.One("11")),
        // group 2
        (toModelComponentId("2_startDate-year"), VariadicValue.One("2001")),
        (toModelComponentId("2_startDate-month"), VariadicValue.One("1")),
        (toModelComponentId("2_startDate-day"), VariadicValue.One("1")),
        (toModelComponentId("2_endDate-year"), VariadicValue.One("2001")),
        (toModelComponentId("2_endDate-month"), VariadicValue.One("11")),
        (toModelComponentId("2_endDate-day"), VariadicValue.One("1"))
      )
    )

    val table = Table(
      ("typeInfo", "recData", "expectedResult"),
      (
        TypeInfo(
          PeriodFunExt(
            PeriodFun(
              DateCtx(DateValueExpr(ExactDateExprValue(2000, 1, 1))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
            ),
            SumProp
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(Period.of(2, 7, 10))
      ),
      (
        TypeInfo(
          PeriodFunExt(
            PeriodFun(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate")))),
              DateCtx(DateValueExpr(ExactDateExprValue(2000, 1, 1)))
            ),
            SumProp
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(Period.of(-2, -7, -10))
      ),
      (
        TypeInfo(
          PeriodFunExt(
            PeriodFun(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
            ),
            SumProp
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        PeriodResult(Period.of(1, 7, 10))
      ),
      (
        TypeInfo(
          PeriodFunExt(
            PeriodFun(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
            ),
            TotalMonthsProp
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        NumberResult(19)
      ),
      (
        TypeInfo(
          PeriodFunExt(
            PeriodFun(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
            ),
            YearsProp
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        NumberResult(1)
      ),
      (
        TypeInfo(
          PeriodFunExt(
            PeriodFun(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
            ),
            MonthsProp
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        NumberResult(7)
      ),
      (
        TypeInfo(
          PeriodFunExt(
            PeriodFun(
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("startDate")))),
              DateCtx(DateFormCtxVar(FormCtx(FormComponentId("endDate"))))
            ),
            DaysProp
          ),
          StaticTypeData(ExprType.period, None)
        ),
        recData,
        NumberResult(10)
      )
    )
    forAll(table) { (typeInfo: TypeInfo, recData: RecData[OutOfDate], expectedResult: ExpressionResult) =>
      EvaluationResults.empty.evalExpr(typeInfo, recData, evaluationContext) shouldBe expectedResult
    }
  }
}
