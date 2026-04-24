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

package uk.gov.hmrc.gform.recalculation

import cats.data.NonEmptyList
import java.time.LocalDate
import uk.gov.hmrc.auth.core.{ Enrolment, EnrolmentIdentifier, Enrolments }
import uk.gov.hmrc.gform.sharedmodel.VariadicValue.{ Many, One }
import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormCtx, HmrcTaxPeriod, IdType, RegimeType }
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroup, DataRetrieve, HmrcTaxPeriodWithEvaluatedId, IdNumberValue, Obligation, ObligationDetail, ObligationDetails, RecalculatedTaxPeriodKey, RetrievedObligations, TaxResponse }
import uk.gov.hmrc.gform.recalculation.EvaluationStatus._

object FormModelOpticsTests1 extends DependencyGraphFixture {
  val data = List(
    (
      MongoUserData(
        "pageA" -> One("1"),
        "pageB" -> One("2")
      ),
      List("n0", "n1"),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(1),
        "pageB" -> Hidden
      ),
      List.empty[String],
      "include-if-field.json field is hidden"
    ),
    (
      MongoUserData(
        "pageA" -> One("100"),
        "pageB" -> One("2")
      ),
      List("n0", "n1"),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(100),
        "pageB" -> NumberResult(2)
      ),
      List.empty[String],
      "include-if-field.json field is visible"
    ),
    (
      MongoUserData(),
      List(),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> Empty,
        "pageB" -> Hidden
      ),
      List.empty[String],
      "include-if-field-2.json Page B is hidden and PageB is hidden A"
    ),
    (
      MongoUserData(
        "pageA" -> One("1"),
        "pageB" -> One("2")
      ),
      List("n0", "n1"),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(1),
        "pageB" -> Hidden
      ),
      List.empty[String],
      "include-if-field-2.json Page B is hidden and PageB is hidden B"
    ),
    (
      MongoUserData(
        "pageA" -> One("11"),
        "pageB" -> One("2")
      ),
      List("n0", "n1"),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(11),
        "pageB" -> Hidden
      ),
      List.empty[String],
      "include-if-field-2.json Page B is visible and pageB is hidden"
    ),
    (
      MongoUserData(
        "pageA" -> One("51"),
        "pageB" -> One("2")
      ),
      List("n0", "n1"),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(51),
        "pageB" -> NumberResult(2)
      ),
      List.empty[String],
      "include-if-field-2.json Page B is visible and pageB is visible and answered"
    ),
    (
      MongoUserData(
        "pageA" -> One("51")
      ),
      List("n0", "n1"),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(51),
        "pageB" -> Empty
      ),
      List.empty[String],
      "include-if-field-2.json Page B is visible and pageB is visible and not answered"
    ),
    (
      MongoUserData(
        "pageA" -> One("51"),
        "pageB" -> One("1")
      ),
      List("n0", "n1"),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(51),
        "pageB" -> NumberResult(1),
        "pageC" -> Hidden
      ),
      List.empty[String],
      "include-if-field-3.json pageB is visible and Page C is hidden"
    ),
    (
      MongoUserData(
        "pageA" -> One("51"),
        "pageB" -> One("11")
      ),
      List("n0", "n1"),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(51),
        "pageB" -> NumberResult(11),
        "pageC" -> Empty
      ),
      List.empty[String],
      "include-if-field-3.json pageB is visible and Page C is visible"
    ),
    (
      MongoUserData(
        "pageA" -> One("51"),
        "pageB" -> One("11"),
        "pageC" -> One("20")
      ),
      List("n0", "n1"),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(51),
        "pageB" -> NumberResult(11),
        "pageC" -> NumberResult(20)
      ),
      List.empty[String],
      "include-if-field-3.json pageB is visible and Page C is visible"
    ),
    (
      MongoUserData(
        "pageA" -> One("49")
      ),
      List("n0", "n1"),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(49),
        "pageB" -> Hidden
      ),
      List.empty[String],
      "field-with-value-expr.json detect computed field as hidden"
    ),
    (
      MongoUserData(
        "pageA" -> One("51")
      ),
      List("n0", "n1"),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(51),
        "pageB" -> NumberResult(BigDecimal("41.00"))
      ),
      List.empty[String],
      "field-with-value-expr.json detect and evaluate computed field"
    ),
    (
      MongoUserData(
        "pageA" -> One("51"),
        "pageB" -> One("42")
      ),
      List("n0", "n1"),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(51),
        "pageB" -> NumberResult(BigDecimal("41.00"))
      ),
      List.empty[String],
      "field-with-value-expr.json detect and evaluate new value of computed field"
    ),
    (
      MongoUserData(
        "1_item"        -> One("10"),
        "1_anotherItem" -> Many(List("0")),
        "2_item"        -> One("20"),
        "2_anotherItem" -> Many(List("1")),
        "pageA"         -> One("30")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_item" -> NumberResult(10),
        "2_item" -> NumberResult(20),
        "pageA"  -> NumberResult(30)
      ),
      List(
        "10",
        "1. 10",
        """|<ol class="govuk-list govuk-list--number">
           | <li>10</li>
           |</ol>""".stripMargin,
        "20",
        "2. 20",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>20</li>
           |</ol>""".stripMargin
      ),
      "atl-sum-outside.json"
    ),
    (
      MongoUserData(
        "1_anotherItem" -> Many(List("0")),
        "1_pageA"       -> One("1"),
        "1_pageB"       -> One("12"),
        "2_pageA"       -> One("20"),
        "2_pageB"       -> One("30")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageA" -> NumberResult(1),
        "1_pageB" -> Hidden,
        "2_pageA" -> NumberResult(20),
        "2_pageB" -> NumberResult(30)
      ),
      List(
        "1",
        "1. 1",
        """|<ol class="govuk-list govuk-list--number">
           | <li>1</li>
           |</ol>""".stripMargin,
        "20",
        "2. 20",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>20</li>
           |</ol>""".stripMargin
      ),
      "atl-page-depends-on-atl-page.json"
    ),
    (
      MongoUserData(
        "1_pageB" -> One("1"),
        "2_pageB" -> One("2"),
        "3_pageB" -> One("3"),
        "4_pageB" -> One("4"),
        "5_pageB" -> One("5"),
        "pageA"   -> One("2"),
        "pageC"   -> One("100")
      ),
      List(
        "n0",
        "r1.0",
        "r1.1",
        "r1.2",
        "r1.3",
        "r1.4",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageB" -> NumberResult(1),
        "2_pageB" -> NumberResult(2),
        "3_pageB" -> Hidden,
        "4_pageB" -> Hidden,
        "5_pageB" -> Hidden,
        "pageA"   -> NumberResult(2),
        "pageC"   -> NumberResult(100)
      ),
      List.empty[String],
      "repeated-section.json Generated"
    ),
    (
      MongoUserData(
        "1_pageB" -> One("1"),
        "1_pageC" -> One("10"),
        "2_pageB" -> One("2"),
        "2_pageC" -> One("20"),
        "3_pageC" -> One("30"),
        "pageA"   -> One("2"),
        "pageD"   -> One("123")
      ),
      List(
        "n0",
        "r1.0",
        "r1.1",
        "r2.0",
        "r2.1",
        "r2.2",
        "n3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageB" -> NumberResult(1),
        "1_pageC" -> NumberResult(10),
        "2_pageB" -> NumberResult(2),
        "2_pageC" -> NumberResult(20),
        "3_pageC" -> NumberResult(30),
        "pageA"   -> NumberResult(2),
        "pageD"   -> Hidden
      ),
      List.empty[String],
      "repeated-section-chain.json Generated"
    ),
    (
      MongoUserData(
        "1_pageC" -> One("1"),
        "2_pageC" -> One("2"),
        "3_pageC" -> One("3"),
        "pageA"   -> One("1"),
        "pageB"   -> One("3"),
        "pageD"   -> One("123")
      ),
      List(
        "n0",
        "n1",
        "r2.0",
        "r2.1",
        "r2.2",
        "n3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageC" -> Hidden,
        "2_pageC" -> Hidden,
        "3_pageC" -> Hidden,
        "pageA"   -> NumberResult(1),
        "pageB"   -> NumberResult(3),
        "pageD"   -> Hidden
      ),
      List.empty[String],
      "repeated-section-with-include-if.json pageD is not visible"
    ),
    (
      MongoUserData(
        "1_pageC" -> One("1"),
        "2_pageC" -> One("2"),
        "3_pageC" -> One("3"),
        "pageA"   -> One("11"),
        "pageB"   -> One("3"),
        "pageD"   -> One("123")
      ),
      List(
        "n0",
        "n1",
        "r2.0",
        "r2.1",
        "r2.2",
        "n3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageC" -> NumberResult(1),
        "2_pageC" -> NumberResult(2),
        "3_pageC" -> NumberResult(3),
        "pageA"   -> NumberResult(11),
        "pageB"   -> NumberResult(3),
        "pageD"   -> NumberResult(123)
      ),
      List.empty[String],
      "repeated-section-with-include-if.json pageD is visible"
    ),
    (
      MongoUserData(
        "pageA"             -> One("100"),
        "pageB"             -> One("200"),
        "repayReasonChoice" -> Many(List("1"))
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageA"             -> NumberResult(100),
        "pageB"             -> NumberResult(200),
        "repayReasonChoice" -> OptionResult(List("1"))
      ),
      List.empty[String],
      "revealing-choice.json Generated"
    ),
    (
      MongoUserData(
        "pageA"             -> One("100"),
        "pageB"             -> One("200"),
        "repayReasonChoice" -> Many(List("0"))
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageA"             -> Hidden,
        "pageB"             -> Hidden,
        "repayReasonChoice" -> OptionResult(List("0"))
      ),
      List.empty[String],
      "revealing-choice.json Generated"
    ),
    (
      MongoUserData(
        "pageA"             -> One("100"),
        "pageB"             -> One("200"),
        "repayReasonChoice" -> Many(List("BAR"))
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageA"             -> NumberResult(100),
        "pageB"             -> NumberResult(200),
        "repayReasonChoice" -> OptionResult(List("BAR"))
      ),
      List.empty[String],
      "revealing-choice-value-based.json Generated"
    ),
    (
      MongoUserData(
        "pageA"             -> One("100"),
        "pageB"             -> One("200"),
        "repayReasonChoice" -> Many(List("2029"))
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageA"             -> NumberResult(100),
        "pageB"             -> NumberResult(200),
        "repayReasonChoice" -> OptionResult(List("2029"))
      ),
      List.empty[String],
      "revealing-choice-value-with-expr-based.json revealing field pageA is visible"
    ),
    (
      MongoUserData(
        "pageA"             -> One("100"),
        "pageB"             -> One("200"),
        "repayReasonChoice" -> Many(List("2027"))
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageA"             -> Hidden,
        "pageB"             -> Hidden,
        "repayReasonChoice" -> OptionResult(List("2027"))
      ),
      List.empty[String],
      "revealing-choice-value-with-expr-based.json revealing field pageA is hidden"
    ),
    (
      MongoUserData(
        "initialDate"       -> One("2025"),
        "pageA"             -> One("100"),
        "repayReasonChoice" -> Many(List("2027"))
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "initialDate"       -> NumberResult(2025),
        "pageA"             -> NumberResult(100),
        "repayReasonChoice" -> OptionResult(List("2027")),
        "pageB"             -> Empty
      ),
      List.empty[String],
      "revealing-choice-value-with-field-ref.json Generated"
    ),
    (
      MongoUserData(
        "1_pageB" -> One("1"),
        "2_pageB" -> One("2"),
        "3_pageB" -> One("3"),
        "pageA"   -> One("100"),
        "pageC"   -> One("6")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageB" -> NumberResult(1),
        "2_pageB" -> NumberResult(2),
        "3_pageB" -> NumberResult(3),
        "pageA"   -> NumberResult(100),
        "pageC"   -> NumberResult(BigDecimal("6.00"))
      ),
      List.empty[String],
      "group-with-include-if.json all fields are visible"
    ),
    (
      MongoUserData(
        "1_pageB" -> One("1"),
        "2_pageB" -> One("2"),
        "3_pageB" -> One("3"),
        "pageA"   -> One("200"),
        "pageC"   -> One("6")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageB" -> NumberResult(1),
        "2_pageB" -> NumberResult(2),
        "3_pageB" -> NumberResult(3),
        "pageA"   -> NumberResult(200),
        "pageC"   -> Hidden
      ),
      List.empty[String],
      "group-with-include-if.json pageC field is invisible"
    ),
    (
      MongoUserData(
        "1_pageB" -> One("1"),
        "2_pageB" -> One("2"),
        "3_pageB" -> One("3"),
        "pageA"   -> One("300"),
        "pageC"   -> One("6")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageB" -> Hidden,
        "2_pageB" -> Hidden,
        "3_pageB" -> Hidden,
        "pageA"   -> NumberResult(300),
        "pageC"   -> Hidden
      ),
      List.empty[String],
      "group-with-include-if.json pageB and pageC fields are invisible"
    ),
    (
      MongoUserData(
        "1_pageB" -> One("1"),
        "2_pageB" -> One("2"),
        "3_pageB" -> One("3"),
        "pageA"   -> One("100"),
        "pageC"   -> One("6")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageB" -> Hidden,
        "2_pageB" -> Hidden,
        "3_pageB" -> Hidden,
        "pageA"   -> NumberResult(100),
        "pageC"   -> Empty
      ),
      List.empty[String],
      "group-is-invisible-sum.json sum fields are hidden but sum is visible"
    ),
    (
      MongoUserData(
        "1_pageB" -> One("1"),
        "2_pageB" -> One("2"),
        "3_pageB" -> One("3"),
        "pageA"   -> One("200"),
        "pageC"   -> One("6")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageB" -> Hidden,
        "2_pageB" -> Hidden,
        "3_pageB" -> Hidden,
        "pageA"   -> NumberResult(200),
        "pageC"   -> Hidden
      ),
      List.empty[String],
      "group-is-invisible-sum.json sum fields are hidden and sum is hidden"
    ),
    (
      MongoUserData(
        "1_anotherItem"       -> Many(List("0")),
        "1_pageA"             -> One("1"),
        "1_repayReasonChoice" -> Many(List("0")),
        "2_anotherItem"       -> Many(List("0")),
        "2_pageA"             -> One("2"),
        "2_repayReasonChoice" -> Many(List("1")),
        "3_anotherItem"       -> Many(List("1")),
        "3_pageA"             -> One("3"),
        "3_repayReasonChoice" -> Many(List("2")),
        "pageB"               -> One("42")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageA"             -> Hidden,
        "1_repayReasonChoice" -> OptionResult(List("0")),
        "2_pageA"             -> NumberResult(2),
        "2_repayReasonChoice" -> OptionResult(List("1")),
        "3_pageA"             -> Hidden,
        "3_repayReasonChoice" -> OptionResult(List("2")),
        "pageB"               -> NumberResult(2)
      ),
      List(
        "I am not eligible",
        "1. I am not eligible",
        """|<ol class="govuk-list govuk-list--number">
           | <li>I am not eligible</li>
           |</ol>""".stripMargin,
        "I would prefer to repay it, or part of it",
        "2. I would prefer to repay it, or part of it",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>I would prefer to repay it, or part of it</li>
           |</ol>""".stripMargin,
        "I believe HMRC has paid me too much in error",
        "3. I believe HMRC has paid me too much in error",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>I believe HMRC has paid me too much in error</li>
           |</ol>""".stripMargin
      ),
      "atl-revealing-choice-sum-value-outside.json"
    ),
    (
      MongoUserData(
        "1_pageB"             -> One("1"),
        "1_repayReasonChoice" -> Many(List("1")),
        "2_pageB"             -> One("2"),
        "2_repayReasonChoice" -> Many(List("1")),
        "3_pageB"             -> One("3"),
        "3_repayReasonChoice" -> Many(List("1")),
        "pageA"               -> One("3")
      ),
      List(
        "n0",
        "r1.0",
        "r1.1",
        "r1.2",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageB"             -> NumberResult(1),
        "1_repayReasonChoice" -> OptionResult(List("1")),
        "2_pageB"             -> NumberResult(2),
        "2_repayReasonChoice" -> OptionResult(List("1")),
        "3_pageB"             -> NumberResult(3),
        "3_repayReasonChoice" -> OptionResult(List("1")),
        "pageA"               -> NumberResult(3),
        "pageC"               -> NumberResult(6)
      ),
      List.empty[String],
      "repeated-section-revealing-choice.json all three pageB elements are visible"
    ),
    (
      MongoUserData(
        "1_pageB"             -> One("1"),
        "1_repayReasonChoice" -> Many(List("0")),
        "2_pageB"             -> One("2"),
        "2_repayReasonChoice" -> Many(List("1")),
        "3_pageB"             -> One("3"),
        "3_repayReasonChoice" -> Many(List("2")),
        "pageA"               -> One("3")
      ),
      List(
        "n0",
        "r1.0",
        "r1.1",
        "r1.2",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageB"             -> Hidden,
        "1_repayReasonChoice" -> OptionResult(List("0")),
        "2_pageB"             -> NumberResult(2),
        "2_repayReasonChoice" -> OptionResult(List("1")),
        "3_pageB"             -> Hidden,
        "3_repayReasonChoice" -> OptionResult(List("2")),
        "pageA"               -> NumberResult(3),
        "pageC"               -> NumberResult(2)
      ),
      List.empty[String],
      "repeated-section-revealing-choice.json two of three pageB elements are hidden"
    ),
    (
      MongoUserData(
        "1_pageB"             -> One("1"),
        "1_repayReasonChoice" -> Many(List("0")),
        "2_pageB"             -> One("2"),
        "2_repayReasonChoice" -> Many(List("0")),
        "3_pageB"             -> One("3"),
        "3_repayReasonChoice" -> Many(List("2")),
        "pageA"               -> One("3")
      ),
      List(
        "n0",
        "r1.0",
        "r1.1",
        "r1.2",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageB"             -> Hidden,
        "1_repayReasonChoice" -> OptionResult(List("0")),
        "2_pageB"             -> Hidden,
        "2_repayReasonChoice" -> OptionResult(List("0")),
        "3_pageB"             -> Hidden,
        "3_repayReasonChoice" -> OptionResult(List("2")),
        "pageA"               -> NumberResult(3),
        "pageC"               -> Empty
      ),
      List.empty[String],
      "repeated-section-revealing-choice.json all three pageB elements are hidden"
    ),
    (
      MongoUserData(
        "pageA" -> One("101")
      ),
      List(
        "0,0,n0",
        "1,0,n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(101),
        "pageC" -> Hidden
      ),
      List.empty[String],
      "task-list.json Generated"
    ),
    (
      MongoUserData(
        "pageA" -> One("100")
      ),
      List(
        "0,0,n0",
        "1,0,n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(100),
        "pageC" -> Empty
      ),
      List.empty[String],
      "task-list.json Generated"
    ),
    (
      MongoUserData(
        "pageA" -> One("100"),
        "pageB" -> One("200")
      ),
      List(
        "0,0,n0",
        "1,0,n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(100),
        "pageB" -> NumberResult(200)
      ),
      List.empty[String],
      "task-list-task-level-include-if.json task list include if is true"
    ),
    (
      MongoUserData(
        "pageA" -> One("101"),
        "pageB" -> One("200")
      ),
      List(
        "0,0,n0",
        "1,0,n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageA" -> NumberResult(101),
        "pageB" -> Hidden
      ),
      List.empty[String],
      "task-list-task-level-include-if.json task list include if is false"
    ),
    (
      MongoUserData(
        "1_anotherItemA" -> Many(List("0")),
        "1_anotherItemB" -> Many(List("0")),
        "1_pageA"        -> One("1"),
        "1_pageB"        -> One("10"),
        "2_anotherItemA" -> Many(List("0")),
        "2_anotherItemB" -> Many(List("0")),
        "2_pageA"        -> One("2"),
        "2_pageB"        -> One("20"),
        "3_anotherItemA" -> Many(List("1")),
        "3_anotherItemB" -> Many(List("1")),
        "3_pageA"        -> One("3"),
        "3_pageB"        -> One("30"),
        "pageC"          -> One("66.00"),
        "pageD"          -> One("100")
      ),
      List(
        "0,0,ap0.1.0",
        "0,0,ar0.1",
        "0,0,ap0.2.0",
        "0,0,ar0.2",
        "0,0,ap0.3.0",
        "0,0,ar0.3",
        "1,0,ap0.1.0",
        "1,0,ar0.1",
        "1,0,ap0.2.0",
        "1,0,ar0.2",
        "1,0,ap0.3.0",
        "1,0,ar0.3",
        "2,0,n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_pageA" -> NumberResult(1),
        "1_pageB" -> NumberResult(10),
        "2_pageA" -> NumberResult(2),
        "2_pageB" -> NumberResult(20),
        "3_pageA" -> NumberResult(3),
        "3_pageB" -> NumberResult(30),
        "pageC"   -> NumberResult(BigDecimal("66.00"))
      ),
      List(
        "1",
        "1. 1",
        """|<ol class="govuk-list govuk-list--number">
           | <li>1</li>
           |</ol>""".stripMargin,
        "2",
        "2. 2",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>2</li>
           |</ol>""".stripMargin,
        "3",
        "3. 3",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>3</li>
           |</ol>""".stripMargin,
        "10",
        "1. 10",
        """|<ol class="govuk-list govuk-list--number">
           | <li>10</li>
           |</ol>""".stripMargin,
        "20",
        "2. 20",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>20</li>
           |</ol>""".stripMargin,
        "30",
        "3. 30",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>30</li>
           |</ol>""".stripMargin
      ),
      "task-list-with-atls.json Generated"
    ),
    (
      MongoUserData(
        "pageA" -> One("100"),
        "pageB" -> One("200")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty.copy(retrievals = authenticatedRetrievals.copy(affinityGroup = AffinityGroup.Individual)),
      AnswerMap(
        "pageB" -> NumberResult(200)
      ),
      List.empty[String],
      "include-if-without-field-reference.json show pageB when Individual"
    ),
    (
      MongoUserData(
        "pageA" -> One("100"),
        "pageB" -> One("200")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty.copy(retrievals = authenticatedRetrievals.copy(affinityGroup = AffinityGroup.Agent)),
      AnswerMap(
        "pageB" -> Hidden
      ),
      List.empty[String],
      "include-if-without-field-reference.json hide pageB when Agent"
    ),
    (
      MongoUserData(
        "fooChoice" -> Many(List("Foo", "Bar")),
        "pageB"     -> One("John Doe")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice" -> OptionResult(List("Foo", "Bar")),
        "pageB"     -> Hidden
      ),
      List.empty[String],
      "choice-checkbox-value-based.json pageB hidden"
    ),
    (
      MongoUserData(
        "fooChoice" -> Many(List("Foo", "Bar", "Baz")),
        "pageB"     -> One("John Doe")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice" -> OptionResult(List("Foo", "Bar", "Baz")),
        "pageB"     -> StringResult("John Doe")
      ),
      List.empty[String],
      "choice-checkbox-value-based.json pageB visible"
    ),
    (
      MongoUserData(
        "fooChoice" -> Many(List("0", "2")),
        "pageB"     -> One("200")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice" -> OptionResult(List("0", "2")),
        "pageB"     -> Hidden
      ),
      List.empty[String],
      "choice-checkbox-index-based.json pageB hidden"
    ),
    (
      MongoUserData(
        "fooChoice" -> Many(List("0", "1", "2")),
        "pageB"     -> One("200")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice" -> OptionResult(List("0", "1", "2")),
        "pageB"     -> StringResult("200")
      ),
      List.empty[String],
      "choice-checkbox-index-based.json pageB visible"
    ),
    (
      MongoUserData(
        "pageB"                  -> One("200"),
        "startTradingDate"       -> One(""),
        "startTradingDate-day"   -> One("1"),
        "startTradingDate-month" -> One("1"),
        "startTradingDate-year"  -> One("2025")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageB"                  -> Hidden,
        "startTradingDate-day"   -> NumberResult(1),
        "startTradingDate-month" -> NumberResult(1),
        "startTradingDate-year"  -> NumberResult(2025)
      ),
      List.empty[String],
      "date-component-before.json pageB hidden"
    ),
    (
      MongoUserData(
        "pageB"                  -> One("200"),
        "startTradingDate"       -> One(""),
        "startTradingDate-day"   -> One("1"),
        "startTradingDate-month" -> One("1"),
        "startTradingDate-year"  -> One("2023")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageB"                  -> StringResult("200"),
        "startTradingDate-day"   -> NumberResult(1),
        "startTradingDate-month" -> NumberResult(1),
        "startTradingDate-year"  -> NumberResult(2023)
      ),
      List.empty[String],
      "date-component-before.json pageB visible"
    ),
    (
      MongoUserData(
        "pageB"                  -> One("200"),
        "startTradingDate"       -> One(""),
        "startTradingDate-day"   -> One("1"),
        "startTradingDate-month" -> One("1"),
        "startTradingDate-year"  -> One("2025")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageB"                  -> StringResult("200"),
        "startTradingDate-day"   -> NumberResult(1),
        "startTradingDate-month" -> NumberResult(1),
        "startTradingDate-year"  -> NumberResult(2025)
      ),
      List.empty[String],
      "date-component-after.json pageB visible"
    ),
    (
      MongoUserData(
        "pageB"                  -> One("200"),
        "startTradingDate"       -> One(""),
        "startTradingDate-day"   -> One("1"),
        "startTradingDate-month" -> One("1"),
        "startTradingDate-year"  -> One("2023")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageB"                  -> Hidden,
        "startTradingDate-day"   -> NumberResult(1),
        "startTradingDate-month" -> NumberResult(1),
        "startTradingDate-year"  -> NumberResult(2023)
      ),
      List.empty[String],
      "date-component-after.json pageB hidden"
    ),
    (
      MongoUserData(
        "pageA"                  -> One("Hello-1 January 2025-Hello"),
        "pageB"                  -> One("Foo"),
        "startTradingDate"       -> One(""),
        "startTradingDate-day"   -> One("1"),
        "startTradingDate-month" -> One("1"),
        "startTradingDate-year"  -> One("2025")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageA"                  -> StringResult("Hello-1 January 2025-Hello"),
        "pageB"                  -> StringResult("Foo"),
        "startTradingDate-day"   -> NumberResult(1),
        "startTradingDate-month" -> NumberResult(1),
        "startTradingDate-year"  -> NumberResult(2025)
      ),
      List.empty[String],
      "date-concat-string.json pageB visible when startTradingDate is January"
    ),
    (
      MongoUserData(
        "pageA"                  -> One("Hello-1 January 2025-Hello"),
        "pageB"                  -> One("Foo"),
        "startTradingDate"       -> One(""),
        "startTradingDate-day"   -> One("1"),
        "startTradingDate-month" -> One("2"),
        "startTradingDate-year"  -> One("2025")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "pageA"                  -> StringResult("Hello-1 January 2025-Hello"),
        "pageB"                  -> Hidden,
        "startTradingDate-day"   -> NumberResult(1),
        "startTradingDate-month" -> NumberResult(2),
        "startTradingDate-year"  -> NumberResult(2025)
      ),
      List.empty[String],
      "date-concat-string.json pageB hidden when startTradingDate is February"
    ),
    (
      MongoUserData(
        "1_anotherItem"     -> Many(List("0")),
        "1_startDate"       -> One(""),
        "1_startDate-day"   -> One("1"),
        "1_startDate-month" -> One("1"),
        "1_startDate-year"  -> One("2023"),
        "2_anotherItem"     -> Many(List("0")),
        "2_item"            -> One("210.82"),
        "2_startDate"       -> One(""),
        "2_startDate-day"   -> One("1"),
        "2_startDate-month" -> One("1"),
        "2_startDate-year"  -> One("2024"),
        "3_anotherItem"     -> Many(List("1")),
        "3_item"            -> One("217.85"),
        "3_startDate"       -> One(""),
        "3_startDate-day"   -> One("1"),
        "3_startDate-month" -> One("1"),
        "3_startDate-year"  -> One("2025")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ar0.2",
        "ap0.3.0",
        "ap0.3.1",
        "ar0.3"
      ),
      EvaluationContext.empty.copy(thirdPartyData =
        ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "1_ppt" ->
              Map(
                DataRetrieve.Attribute("rate") -> "200"
              ),
            "2_ppt" ->
              Map(
                DataRetrieve.Attribute("rate") -> "210.82"
              ),
            "3_ppt" ->
              Map(
                DataRetrieve.Attribute("rate") -> "217.85"
              )
          )
        )
      ),
      AnswerMap(
        "1_item" -> Hidden,
        "2_item" -> NumberResult(210.82),
        "3_item" -> NumberResult(217.85)
      ),
      List(
        " - 1 January 2023",
        "1.  - 1 January 2023",
        """|<ol class="govuk-list govuk-list--number">
           | <li>
           |  <ul class="govuk-list govuk-list--bullet">
           |   <li>1 January 2023</li>
           |  </ul></li>
           |</ol>""".stripMargin,
        "210.82 - 1 January 2024",
        "2. 210.82 - 1 January 2024",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>210.82 - 1 January 2024</li>
           |</ol>""".stripMargin,
        "217.85 - 1 January 2025",
        "3. 217.85 - 1 January 2025",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>217.85 - 1 January 2025</li>
           |</ol>""".stripMargin
      ),
      "atl-data-retrieve-hmrc-tax-rates.json Generated"
    ),
    (
      MongoUserData(
        "GDreg"         -> One("77GD77777777"),
        "endDate"       -> One(""),
        "endDate-day"   -> One("1"),
        "endDate-month" -> One("2"),
        "endDate-year"  -> One("2000"),
        "pageC"         -> One("123"),
        "pageD"         -> One("1852000"),
        "returnPeriod"  -> One("17B1")
      ),
      List(
        "n0",
        "n1",
        "n2",
        "n3"
      ),
      EvaluationContext.empty.copy(
        retrievals = authenticatedRetrievals.copy(
          enrolments = Enrolments(
            Set(
              Enrolment(
                "HMRC-OBTDS-ORG",
                Seq(EnrolmentIdentifier("EtmpRegistrationNumber", "77GD77777777")),
                "Activated",
                None
              )
            )
          )
        ),
        thirdPartyData = ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "band15" ->
              Map(
                DataRetrieve.Attribute("rate") -> "2686000"
              ),
            "band20" ->
              Map(
                DataRetrieve.Attribute("rate") -> "1852000"
              )
          )
        )
      ),
      AnswerMap(
        "GDreg" -> StringResult("77GD77777777"),
        "pageD" -> NumberResult(1852000) // This should be StringResult, due to text/text
      ),
      List.empty[String],
      "hmrc-tax-period.json Generated"
    ),
    (
      MongoUserData(
        "GDreg"        -> One("77GD77777777"),
        "pageC"        -> One("222"),
        "returnPeriod" -> One("17B1")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty.copy(
        retrievals = authenticatedRetrievals.copy(
          enrolments = Enrolments(
            Set(
              Enrolment(
                "HMRC-OBTDS-ORG",
                Seq(EnrolmentIdentifier("EtmpRegistrationNumber", "77GD77777777")),
                "Activated",
                None
              )
            )
          )
        ),
        thirdPartyData = ThirdPartyData.empty.copy(
          obligations = RetrievedObligations(
            NonEmptyList.one(
              TaxResponse(
                HmrcTaxPeriodWithEvaluatedId(
                  RecalculatedTaxPeriodKey(
                    FormComponentId("returnPeriod"),
                    HmrcTaxPeriod(
                      IdType("eeits"),
                      FormCtx(FormComponentId("GDreg")),
                      RegimeType("GD")
                    )
                  ),
                  IdNumberValue("77GD77777777")
                ),
                Obligation(
                  List(
                    ObligationDetails(
                      List(
                        ObligationDetail(
                          "O",
                          LocalDate.of(2022, 4, 1),
                          LocalDate.of(2022, 4, 30),
                          LocalDate.of(2022, 5, 15),
                          "17B1"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      AnswerMap(
        "GDreg"        -> StringResult("77GD77777777"),
        "pageC"        -> StringResult("222"),
        "returnPeriod" -> StringResult("17B1")
      ),
      List.empty[String],
      "hmrc-tax-period-in-include-if.json Generated"
    ),
    (
      MongoUserData(),
      List(),
      EvaluationContext.empty,
      AnswerMap("1_filmChoice" -> Empty),
      List.empty[String],
      "atl-page-without-enterable-field.json Generated"
    ),
    (
      MongoUserData(
        "trProjectType" -> Many(List("filmTR"))
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "trProjectType"    -> OptionResult(List("filmTR")),
        "1_filmName"       -> Empty,
        "1_filmAddAnother" -> Empty
      ),
      List(
        "1. ",
        """|<ol class="govuk-list govuk-list--number">
           | <li></li>
           |</ol>""".stripMargin
      ),
      "atl-hidden-by-include-if.json addToList is visible"
    ),
    (
      MongoUserData(
        "trProjectType" -> Many(List("videoGameTR"))
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "trProjectType"    -> OptionResult(List("videoGameTR")),
        "1_filmName"       -> Hidden,
        "1_filmAddAnother" -> Hidden
      ),
      List.empty[String],
      "atl-hidden-by-include-if.json addToList is hidden"
    ),
    (
      MongoUserData(
        "pageA"     -> One("1"),
        "fooChoice" -> Many(List("Bar"))
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice" -> OptionResult(List("Bar")),
        "pageA"     -> NumberResult(1),
        "pageB"     -> Empty
      ),
      List.empty[String],
      "revealing-choice-hidden-option-value-based.json Bar option is visible"
    ),
    (
      MongoUserData(
        "pageA"     -> One("100"),
        "fooChoice" -> Many(List("Bar"))
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice" -> Empty,
        "pageA"     -> NumberResult(100),
        "pageB"     -> Hidden
      ),
      List.empty[String],
      "revealing-choice-hidden-option-value-based.json previously selected Bar option become hidden"
    ),
    (
      MongoUserData(
        "pageA"     -> One("100"),
        "fooChoice" -> Many(List("Foo", "Bar", "Baz"))
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice" -> OptionResult(List("Foo", "Baz")),
        "pageA"     -> NumberResult(100),
        "pageB"     -> Hidden
      ),
      List.empty[String],
      "revealing-choice-hidden-option-value-based.json from previously selected options Bar become hidden"
    ),
    (
      MongoUserData(
        "fooChoice" -> Many(List("2027", "2028", "2029")),
        "pageA"     -> One("12"),
        "pageB"     -> One("200")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice" -> OptionResult(List("2027", "2028", "2029")),
        "pageA"     -> NumberResult(12),
        "pageB"     -> NumberResult(200)
      ),
      List.empty[String],
      "revealing-choice-hidden-option-value-based-with-expr.json Generated1"
    ),
    (
      MongoUserData(
        "fooChoice" -> Many(List("2029")),
        "pageA"     -> One("123"),
        "pageB"     -> One("200")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice" -> Empty,
        "pageA"     -> NumberResult(123),
        "pageB"     -> Hidden
      ),
      List.empty[String],
      "revealing-choice-hidden-option-value-based-with-expr.json Generated2" // HERe
    ),
    (
      MongoUserData(
        "fooChoice" -> Many(List("2028", "2029", "2030")),
        "pageA"     -> One("123"),
        "pageB"     -> One("200")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice" -> OptionResult(List("2028", "2030")),
        "pageA"     -> NumberResult(123),
        "pageB"     -> Hidden
      ),
      List.empty[String],
      "revealing-choice-hidden-option-value-based-with-expr.json Generated3"
    ),
    (
      MongoUserData(
        "startDate-day"   -> One("1"),
        "startDate-month" -> One("2"),
        "startDate-year"  -> One("2024")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "startDate-day"   -> NumberResult(1),
        "startDate-month" -> NumberResult(2),
        "startDate-year"  -> NumberResult(2024),
        "name"            -> Empty
      ),
      List.empty[String],
      "date-expr-year.json show dependent page"
    ),
    (
      MongoUserData(
        "startDate-day"   -> One("1"),
        "startDate-month" -> One("2"),
        "startDate-year"  -> One("2026")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "startDate-day"   -> NumberResult(1),
        "startDate-month" -> NumberResult(2),
        "startDate-year"  -> NumberResult(2026),
        "name"            -> Hidden
      ),
      List.empty[String],
      "date-expr-year.json hide dependent page"
    ),
    (
      MongoUserData(
        "startDate-day"   -> One("1"),
        "startDate-month" -> One("5"),
        "startDate-year"  -> One("2024")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "startDate-day"   -> NumberResult(1),
        "startDate-month" -> NumberResult(5),
        "startDate-year"  -> NumberResult(2024),
        "name"            -> Empty
      ),
      List.empty[String],
      "date-expr-month.json show dependent page"
    ),
    (
      MongoUserData(
        "startDate-day"   -> One("1"),
        "startDate-month" -> One("12"),
        "startDate-year"  -> One("2024")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "startDate-day"   -> NumberResult(1),
        "startDate-month" -> NumberResult(12),
        "startDate-year"  -> NumberResult(2024),
        "name"            -> Hidden
      ),
      List.empty[String],
      "date-expr-month.json hide dependent page"
    ),
    (
      MongoUserData(
        "startDate-day"   -> One("18"),
        "startDate-month" -> One("1"),
        "startDate-year"  -> One("2024")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "startDate-day"   -> NumberResult(18),
        "startDate-month" -> NumberResult(1),
        "startDate-year"  -> NumberResult(2024),
        "name"            -> Empty
      ),
      List.empty[String],
      "date-expr-day.json show dependent page"
    ),
    (
      MongoUserData(
        "startDate-day"   -> One("24"),
        "startDate-month" -> One("1"),
        "startDate-year"  -> One("2024")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "startDate-day"   -> NumberResult(24),
        "startDate-month" -> NumberResult(1),
        "startDate-year"  -> NumberResult(2024),
        "name"            -> Hidden
      ),
      List.empty[String],
      "date-expr-day.json hide dependent page"
    ),
    (
      MongoUserData(
        "enterDate-day"   -> One("1"),
        "enterDate-month" -> One("1"),
        "enterDate-year"  -> One("2024")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty.copy(thirdPartyData =
        ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "apdRate" ->
              Map(
                DataRetrieve.Attribute("endDate")   -> "2024-03-31",
                DataRetrieve.Attribute("startDate") -> "2023-04-01"
              )
          )
        )
      ),
      AnswerMap(
        "name" -> Empty
      ),
      List.empty[String],
      "date-expr-data-retrieve-hmrc-tax-rates.json Generated"
    ),
    (
      MongoUserData(
        "enterDate-day"   -> One("1"),
        "enterDate-month" -> One("1"),
        "enterDate-year"  -> One("2025")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty.copy(thirdPartyData =
        ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "apdRate" ->
              Map(
                DataRetrieve.Attribute("endDate")   -> "2999-12-31",
                DataRetrieve.Attribute("startDate") -> "2024-04-01"
              )
          )
        )
      ),
      AnswerMap(
        "name" -> Hidden
      ),
      List.empty[String],
      "date-expr-data-retrieve-hmrc-tax-rates.json Generated"
    ),
    (
      MongoUserData(
        "1_enterDate-day"   -> One("1"),
        "1_enterDate-month" -> One("2"),
        "1_enterDate-year"  -> One("2023"),
        "1_name"            -> One("AAA")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1"
      ),
      EvaluationContext.empty.copy(thirdPartyData =
        ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrieves(
            "1_apdRate" ->
              Map(
                DataRetrieve.Attribute("endDate")   -> "2024-03-31",
                DataRetrieve.Attribute("startDate") -> "2023-04-01"
              )
          )
        )
      ),
      AnswerMap(
        "1_name" -> StringResult("AAA")
      ),
      List(
        "Enter name 1 1 April 2023",
        "1 February 2023",
        "1. 1 February 2023 - AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>1 February 2023 - AAA</li>
           |</ol>""".stripMargin
      ),
      "date-expr-data-retrieve-hmrc-tax-rates-atl.json Generated"
    ),
    (
      MongoUserData(
        "importerDate-day"   -> One("1"),
        "importerDate-month" -> One("1"),
        "importerDate-year"  -> One("2026")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "importerDate-day"   -> NumberResult(1),
        "importerDate-month" -> NumberResult(1),
        "importerDate-year"  -> NumberResult(2026),
        "note"               -> Empty
      ),
      List.empty[String],
      "date-expr-if-else.json show dependent page"
    ),
    (
      MongoUserData(
        "importerDate-day"   -> One("1"),
        "importerDate-month" -> One("1"),
        "importerDate-year"  -> One("2024")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "importerDate-day"   -> NumberResult(1),
        "importerDate-month" -> NumberResult(1),
        "importerDate-year"  -> NumberResult(2024),
        "note"               -> Hidden
      ),
      List.empty[String],
      "date-expr-if-else.json hide dependent page"
    ),
    (
      MongoUserData(
        "fooChoice"          -> Many(List("Bar")),
        "importerDate-day"   -> One("1"),
        "importerDate-month" -> One("1"),
        "importerDate-year"  -> One("2026"),
        "note"               -> One("AAA")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice"          -> OptionResult(List("Bar")),
        "importerDate-day"   -> Hidden,
        "importerDate-month" -> Hidden,
        "importerDate-year"  -> Hidden,
        "note"               -> StringResult("AAA")
      ),
      List.empty[String],
      "date-expr-or-else.json show dependent page"
    ),
    (
      MongoUserData(
        "fooChoice"          -> Many(List("Foo")),
        "importerDate-day"   -> One("1"),
        "importerDate-month" -> One("1"),
        "importerDate-year"  -> One("2026"),
        "note"               -> One("AAA")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice"          -> OptionResult(List("Foo")),
        "importerDate-day"   -> NumberResult(1),
        "importerDate-month" -> NumberResult(1),
        "importerDate-year"  -> NumberResult(2026),
        "note"               -> Hidden
      ),
      List.empty[String],
      "date-expr-or-else.json hide dependent page"
    ),
    (
      MongoUserData(
        "fooChoice"          -> Many(List("Foo")),
        "importerDate-day"   -> One("1"),
        "importerDate-month" -> One("1"),
        "importerDate-year"  -> One("2022"),
        "note"               -> One("AAA")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice"          -> OptionResult(List("Foo")),
        "importerDate-day"   -> NumberResult(1),
        "importerDate-month" -> NumberResult(1),
        "importerDate-year"  -> NumberResult(2022),
        "note"               -> StringResult("AAA")
      ),
      List.empty[String],
      "date-expr-year-to-date.json show dependent pages"
    ),
    (
      MongoUserData(
        "fooChoice"          -> Many(List("Bar")),
        "importerDate-day"   -> One("1"),
        "importerDate-month" -> One("1"),
        "importerDate-year"  -> One("2022"),
        "note"               -> One("AAA")
      ),
      List(
        "n0",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "fooChoice"          -> OptionResult(List("Bar")),
        "importerDate-day"   -> Hidden,
        "importerDate-month" -> Hidden,
        "importerDate-year"  -> Hidden,
        "note"               -> Hidden
      ),
      List.empty[String],
      "date-expr-year-to-date.json hide dependent pages"
    ),
    (
      MongoUserData(
        "startDate-day"   -> One("15"),
        "startDate-month" -> One("2"),
        "name"            -> One("AAA")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "startDate-day"   -> NumberResult(15),
        "startDate-month" -> NumberResult(2),
        "name"            -> StringResult("AAA")
      ),
      List.empty[String],
      "date-expr-calendar-date.json show dependent page"
    ),
    (
      MongoUserData(
        "startDate-day"   -> One("19"),
        "startDate-month" -> One("2"),
        "name"            -> One("AAA")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "startDate-day"   -> NumberResult(19),
        "startDate-month" -> NumberResult(2),
        "name"            -> Hidden
      ),
      List.empty[String],
      "date-expr-calendar-date.json hide dependent page"
    ),
    (
      MongoUserData(
        "startDate-month" -> One("5"),
        "startDate-year"  -> One("2024")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "startDate-month" -> NumberResult(5),
        "startDate-year"  -> NumberResult(2024),
        "name"            -> Empty
      ),
      List.empty[String],
      "date-expr-tax-period-date.json show dependent page"
    ),
    (
      MongoUserData(
        "startDate-month" -> One("7"),
        "startDate-year"  -> One("2024")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "startDate-month" -> NumberResult(7),
        "startDate-year"  -> NumberResult(2024),
        "name"            -> Hidden
      ),
      List.empty[String],
      "date-expr-tax-period-date.json hide dependent page"
    ),
    (
      MongoUserData(
        "date1-day"   -> One("1"),
        "date1-month" -> One("1"),
        "date1-year"  -> One("2000"),
        "date2-day"   -> One("2"),
        "date2-month" -> One("2"),
        "date2-year"  -> One("2020")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "date1-day"   -> NumberResult(1),
        "date1-month" -> NumberResult(1),
        "date1-year"  -> NumberResult(2000),
        "date2-day"   -> NumberResult(2),
        "date2-month" -> NumberResult(2),
        "date2-year"  -> NumberResult(2020),
        "note"        -> Empty
      ),
      List.empty[String],
      "date-expr-earliest-of.json show dependent page"
    ),
    (
      MongoUserData(
        "date1-day"   -> One("1"),
        "date1-month" -> One("1"),
        "date1-year"  -> One("2025"),
        "date2-day"   -> One("2"),
        "date2-month" -> One("2"),
        "date2-year"  -> One("2025")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "date1-day"   -> NumberResult(1),
        "date1-month" -> NumberResult(1),
        "date1-year"  -> NumberResult(2025),
        "date2-day"   -> NumberResult(2),
        "date2-month" -> NumberResult(2),
        "date2-year"  -> NumberResult(2025),
        "note"        -> Hidden
      ),
      List.empty[String],
      "date-expr-earliest-of.json hide dependent page"
    ),
    (
      MongoUserData(
        "1_anotherItem"     -> Many(List("0")),
        "1_enterDate-day"   -> One("1"),
        "1_enterDate-month" -> One("1"),
        "1_enterDate-year"  -> One("2024"),
        "2_anotherItem"     -> Many(List("1")),
        "2_enterDate-day"   -> One("2"),
        "2_enterDate-month" -> One("2"),
        "2_enterDate-year"  -> One("2000")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_enterDate-day"   -> NumberResult(1),
        "1_enterDate-month" -> NumberResult(1),
        "1_enterDate-year"  -> NumberResult(2024),
        "2_enterDate-day"   -> NumberResult(2),
        "2_enterDate-month" -> NumberResult(2),
        "2_enterDate-year"  -> NumberResult(2000),
        "name"              -> Empty
      ),
      List(
        "1 January 2024",
        "1. 1 January 2024",
        """|<ol class="govuk-list govuk-list--number">
           | <li>1 January 2024</li>
           |</ol>""".stripMargin,
        "2 February 2000",
        "2. 2 February 2000",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>2 February 2000</li>
           |</ol>""".stripMargin
      ),
      "list-result-date-expr-before.json Generated"
    ),
    (
      MongoUserData(
        "dateA-day"   -> One("1"),
        "dateA-month" -> One("1"),
        "dateA-year"  -> One("2001"),
        "dateB-day"   -> One("2"),
        "dateB-month" -> One("2"),
        "dateB-year"  -> One("2002")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        """|date A: 1 January 2001
           |
           |date B: 2 February 2002
           |
           |Earliest: 1 January 2001
           |
           |Latest: 2 February 2002""".stripMargin,
        """|<p>date A: 1 January 2001</p>
           |<p>date B: 2 February 2002</p>
           |<p>Earliest: 1 January 2001</p>
           |<p>Latest: 2 February 2002</p>""".stripMargin
      ),
      "earliest-of-dates.json Generated"
    ),
    (
      MongoUserData(
        "calendarDateA-day"   -> One("1"),
        "calendarDateA-month" -> One("1"),
        "calendarDateB-day"   -> One("2"),
        "calendarDateB-month" -> One("2")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        """|calendar date A: 1 January
           |
           |calendar date B: 2 February
           |
           |Earliest: 1 January
           |
           |Latest: 2 February""".stripMargin,
        """|<p>calendar date A: 1 January</p>
           |<p>calendar date B: 2 February</p>
           |<p>Earliest: 1 January</p>
           |<p>Latest: 2 February</p>""".stripMargin
      ),
      "earliest-of-calender-dates.json Generated"
    ),
    (
      MongoUserData(
        "taxPeriodDateA-month" -> One("1"),
        "taxPeriodDateA-year"  -> One("2001"),
        "taxPeriodDateB-month" -> One("2"),
        "taxPeriodDateB-year"  -> One("2002")
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        """|tax period date A: January 2001
           |
           |tax period date B: February 2002
           |
           |Earliest: January 2001
           |
           |Latest: February 2002""".stripMargin,
        """|<p>tax period date A: January 2001</p>
           |<p>tax period date B: February 2002</p>
           |<p>Earliest: January 2001</p>
           |<p>Latest: February 2002</p>""".stripMargin
      ),
      "earliest-of-tax-period-dates.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnotherName"  -> Many(List("0")),
        "1_name"            -> One("AAA"),
        "2_addAnotherName"  -> Many(List("1")),
        "2_name"            -> One("BBB"),
        "visibilityOptions" -> Many(List("showATL", "showDefaultPage", "showAdditionalInfo"))
      ),
      List(
        "n0",
        "ad1",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnotherName"  -> OptionResult(List("0")),
        "1_name"            -> StringResult("AAA"),
        "2_addAnotherName"  -> OptionResult(List("1")),
        "2_name"            -> StringResult("BBB"),
        "visibilityOptions" -> OptionResult(List("showATL", "showDefaultPage", "showAdditionalInfo"))
      ),
      List(
        "Add 1",
        "Basic info.",
        "<p>Basic info.</p>",
        "Additional info.",
        "<p>Additional info.</p>",
        "AAA",
        "1. AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>AAA</li>
           |</ol>""".stripMargin,
        "BBB",
        "2. BBB",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>BBB</li>
           |</ol>""".stripMargin
      ),
      "atl-default-page.json show default page and show additional info field on default page"
    ),
    (
      MongoUserData(
        "1_addAnotherName"  -> Many(List("0")),
        "1_name"            -> One("AAA"),
        "2_addAnotherName"  -> Many(List("1")),
        "2_name"            -> One("BBB"),
        "visibilityOptions" -> Many(List("showATL", "showDefaultPage"))
      ),
      List(
        "n0",
        "ad1",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnotherName"  -> OptionResult(List("0")),
        "1_name"            -> StringResult("AAA"),
        "2_addAnotherName"  -> OptionResult(List("1")),
        "2_name"            -> StringResult("BBB"),
        "visibilityOptions" -> OptionResult(List("showATL", "showDefaultPage"))
      ),
      List(
        "Add 1",
        "Basic info.",
        "<p>Basic info.</p>",
        "AAA",
        "1. AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>AAA</li>
           |</ol>""".stripMargin,
        "BBB",
        "2. BBB",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>BBB</li>
           |</ol>""".stripMargin
      ),
      "atl-default-page.json show default page and hide additional info field on default page"
    ),
    (
      MongoUserData(
        "1_addAnotherName"  -> Many(List("0")),
        "1_name"            -> One("AAA"),
        "2_addAnotherName"  -> Many(List("1")),
        "2_name"            -> One("BBB"),
        "visibilityOptions" -> Many(List("showATL"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnotherName"  -> OptionResult(List("0")),
        "1_name"            -> StringResult("AAA"),
        "2_addAnotherName"  -> OptionResult(List("1")),
        "2_name"            -> StringResult("BBB"),
        "visibilityOptions" -> OptionResult(List("showATL"))
      ),
      List(
        "AAA",
        "1. AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>AAA</li>
           |</ol>""".stripMargin,
        "BBB",
        "2. BBB",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>BBB</li>
           |</ol>""".stripMargin
      ),
      "atl-default-page.json hide default page and hide additional info field on default page"
    ),
    (
      MongoUserData(
        "1_addAnotherName"  -> Many(List("0")),
        "1_name"            -> One("AAA"),
        "2_addAnotherName"  -> Many(List("1")),
        "2_name"            -> One("BBB"),
        "visibilityOptions" -> Many(List("showATL", "showAdditionalInfo"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ar1.1",
        "ap1.2.0",
        "ar1.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnotherName"  -> OptionResult(List("0")),
        "1_name"            -> StringResult("AAA"),
        "2_addAnotherName"  -> OptionResult(List("1")),
        "2_name"            -> StringResult("BBB"),
        "visibilityOptions" -> OptionResult(List("showATL", "showAdditionalInfo"))
      ),
      List(
        "AAA",
        "1. AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>AAA</li>
           |</ol>""".stripMargin,
        "BBB",
        "2. BBB",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>BBB</li>
           |</ol>""".stripMargin
      ),
      "atl-default-page.json hide default page and keep additional info field on default page hidden"
    ),
    (
      MongoUserData(
        "visibilityOptions" -> Many(List("showNothing"))
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "visibilityOptions" -> OptionResult(List("showNothing")),
        "1_addAnotherName"  -> Hidden,
        "1_name"            -> Hidden
      ),
      List.empty[String],
      "atl-default-page.json hide addToList completely"
    ),
    (
      MongoUserData(
        "1_addAnotherName"  -> Many(List("0")),
        "1_name"            -> One("AAA"),
        "2_addAnotherName"  -> Many(List("1")),
        "2_name"            -> One("BBB"),
        "visibilityOptions" -> Many(List("showATL", "showAdditionalInfo"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ac1.1",
        "ar1.1",
        "ap1.2.0",
        "ac1.2",
        "ar1.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnotherName"  -> OptionResult(List("0")),
        "1_name"            -> StringResult("AAA"),
        "2_addAnotherName"  -> OptionResult(List("1")),
        "2_name"            -> StringResult("BBB"),
        "visibilityOptions" -> OptionResult(List("showATL", "showAdditionalInfo"))
      ),
      List(
        "Basic info about AAA",
        "<p>Basic info about AAA</p>",
        "Additional info about AAA",
        "<p>Additional info about AAA</p>",
        "AAA",
        "1. AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>AAA</li>
           |</ol>""".stripMargin,
        "Basic info about BBB",
        "<p>Basic info about BBB</p>",
        "Additional info about BBB",
        "<p>Additional info about BBB</p>",
        "BBB",
        "2. BBB",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>BBB</li>
           |</ol>""".stripMargin
      ),
      "atl-cya-page.json show additional info component on cyaPage"
    ),
    (
      MongoUserData(
        "1_addAnotherName"  -> Many(List("0")),
        "1_name"            -> One("AAA"),
        "2_addAnotherName"  -> Many(List("1")),
        "2_name"            -> One("BBB"),
        "visibilityOptions" -> Many(List("showATL"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ac1.1",
        "ar1.1",
        "ap1.2.0",
        "ac1.2",
        "ar1.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnotherName"  -> OptionResult(List("0")),
        "1_name"            -> StringResult("AAA"),
        "2_addAnotherName"  -> OptionResult(List("1")),
        "2_name"            -> StringResult("BBB"),
        "visibilityOptions" -> OptionResult(List("showATL"))
      ),
      List(
        "Basic info about AAA",
        "<p>Basic info about AAA</p>",
        "AAA",
        "1. AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>AAA</li>
           |</ol>""".stripMargin,
        "Basic info about BBB",
        "<p>Basic info about BBB</p>",
        "BBB",
        "2. BBB",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>BBB</li>
           |</ol>""".stripMargin
      ),
      "atl-cya-page.json hide additional info component on cyaPage"
    ),
    (
      MongoUserData(
        "visibilityOptions" -> Many(List("showNothing"))
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "visibilityOptions" -> OptionResult(List("showNothing")),
        "1_addAnotherName"  -> Hidden,
        "1_name"            -> Hidden
      ),
      List.empty[String],
      "atl-cya-page.json hide addToList completely"
    ),
    (
      MongoUserData(
        "1_addAnotherName"  -> Many(List("0")),
        "1_name"            -> One("AAA"),
        "2_addAnotherName"  -> Many(List("1")),
        "2_name"            -> One("BBB"),
        "visibilityOptions" -> Many(List("showATL"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ac1.1",
        "ar1.1",
        "ap1.2.0",
        "ac1.2",
        "ar1.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnotherName"  -> OptionResult(List("0")),
        "1_name"            -> StringResult("AAA"),
        "2_addAnotherName"  -> OptionResult(List("1")),
        "2_name"            -> StringResult("BBB"),
        "visibilityOptions" -> OptionResult(List("showATL"))
      ),
      List(
        "AAA",
        "1. AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>AAA</li>
           |</ol>""".stripMargin,
        "BBB",
        "2. BBB",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>BBB</li>
           |</ol>""".stripMargin
      ),
      "atl-declaration-section.json hide declaration section and hide additional info field on declaration page"
    ),
    (
      MongoUserData(
        "1_addAnotherName"  -> Many(List("0")),
        "1_name"            -> One("AAA"),
        "2_addAnotherName"  -> Many(List("1")),
        "2_name"            -> One("BBB"),
        "visibilityOptions" -> Many(List("showATL", "showDeclarationSection"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ac1.1",
        "ar1.1",
        "ap1.2.0",
        "ac1.2",
        "ar1.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnotherName"  -> OptionResult(List("0")),
        "1_name"            -> StringResult("AAA"),
        "2_addAnotherName"  -> OptionResult(List("1")),
        "2_name"            -> StringResult("BBB"),
        "visibilityOptions" -> OptionResult(List("showATL", "showDeclarationSection"))
      ),
      List(
        "Basic info about AAA",
        "<p>Basic info about AAA</p>",
        "AAA",
        "1. AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>AAA</li>
           |</ol>""".stripMargin,
        "Basic info about BBB",
        "<p>Basic info about BBB</p>",
        "BBB",
        "2. BBB",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>BBB</li>
           |</ol>""".stripMargin
      ),
      "atl-declaration-section.json hide additional info component on declarationSection"
    ),
    (
      MongoUserData(
        "1_addAnotherName"  -> Many(List("0")),
        "1_name"            -> One("AAA"),
        "2_addAnotherName"  -> Many(List("1")),
        "2_name"            -> One("BBB"),
        "visibilityOptions" -> Many(List("showATL", "showDeclarationSection", "showAdditionalInfo"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ac1.1",
        "ar1.1",
        "ap1.2.0",
        "ac1.2",
        "ar1.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnotherName"  -> OptionResult(List("0")),
        "1_name"            -> StringResult("AAA"),
        "2_addAnotherName"  -> OptionResult(List("1")),
        "2_name"            -> StringResult("BBB"),
        "visibilityOptions" -> OptionResult(List("showATL", "showDeclarationSection", "showAdditionalInfo"))
      ),
      List(
        "Basic info about AAA",
        "<p>Basic info about AAA</p>",
        "Additional info about AAA",
        "<p>Additional info about AAA</p>",
        "AAA",
        "1. AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>AAA</li>
           |</ol>""".stripMargin,
        "Basic info about BBB",
        "<p>Basic info about BBB</p>",
        "Additional info about BBB",
        "<p>Additional info about BBB</p>",
        "BBB",
        "2. BBB",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>BBB</li>
           |</ol>""".stripMargin
      ),
      "atl-declaration-section.json show additional info component on declarationSection"
    ),
    (
      MongoUserData(
        "1_addAnotherName"  -> Many(List("0")),
        "1_name"            -> One("AAA"),
        "2_addAnotherName"  -> Many(List("1")),
        "2_name"            -> One("BBB"),
        "visibilityOptions" -> Many(List("showATL", "showAdditionalInfo"))
      ),
      List(
        "n0",
        "ap1.1.0",
        "ac1.1",
        "ar1.1",
        "ap1.2.0",
        "ac1.2",
        "ar1.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnotherName"  -> OptionResult(List("0")),
        "1_name"            -> StringResult("AAA"),
        "2_addAnotherName"  -> OptionResult(List("1")),
        "2_name"            -> StringResult("BBB"),
        "visibilityOptions" -> OptionResult(List("showATL", "showAdditionalInfo"))
      ),
      List(
        "AAA",
        "1. AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>AAA</li>
           |</ol>""".stripMargin,
        "BBB",
        "2. BBB",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>BBB</li>
           |</ol>""".stripMargin
      ),
      "atl-declaration-section.json keep additional info component on declaration section hidden"
    ),
    (
      MongoUserData(
        "visibilityOptions" -> Many(List("showNothing"))
      ),
      List(
        "n0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "visibilityOptions" -> OptionResult(List("showNothing")),
        "1_addAnotherName"  -> Hidden,
        "1_name"            -> Hidden
      ),
      List.empty[String],
      "atl-declaration-section.json hide declaration section completely"
    ),
    (
      MongoUserData(
        "1_whatDutyPaid" -> One("1"),
        "2_whatDutyPaid" -> One("2"),
        "3_whatDutyPaid" -> One("3"),
        "repeatCount"    -> One("3")
      ),
      List(
        "n0",
        "r1.0",
        "r1.1",
        "r1.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_whatDutyPaid" -> NumberResult(1),
        "2_whatDutyPaid" -> NumberResult(2),
        "3_whatDutyPaid" -> NumberResult(3),
        "repeatCount"    -> NumberResult(3)
      ),
      List(
        "Name 1 out of 3",
        "Name 2 out of 3",
        "Name 3 out of 3",
        """|whatDutyPaid: £1.00, £2.00, £3.00
           |
           |whatDutyPaid.sum: £6.00""".stripMargin,
        """|<p>whatDutyPaid: £1.00, £2.00, £3.00</p>
           |<p>whatDutyPaid.sum: £6.00</p>""".stripMargin
      ),
      "repeated-section-smartstrings.json Generated"
    ),
    (
      MongoUserData(
        "1_repeatingField" -> One("1"),
        "2_repeatingField" -> One("2"),
        "3_repeatingField" -> One("3"),
        "test"             -> One("4")
      ),
      List(
        "r0.0",
        "r0.1",
        "r0.2",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_repeatingField" -> NumberResult(1),
        "2_repeatingField" -> NumberResult(2),
        "3_repeatingField" -> NumberResult(3),
        "test"             -> NumberResult(4)
      ),
      List(
        "Basic info about 1, 2, 3",
        "<p>Basic info about 1, 2, 3</p>"
      ),
      "repeated-section-outside-reference.json Generated"
    ),
    (
      MongoUserData(
        "1_repeatingField" -> One("1"),
        "2_repeatingField" -> One("2"),
        "3_repeatingField" -> One("3")
      ),
      List(
        "0,0,r0.0",
        "0,0,r0.1",
        "0,0,r0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_repeatingField" -> NumberResult(1),
        "2_repeatingField" -> NumberResult(2),
        "3_repeatingField" -> NumberResult(3),
        "test"             -> Empty
      ),
      List(
        "Basic info about 1, 2, 3 sum of which is 6",
        "<p>Basic info about 1, 2, 3 sum of which is 6</p>"
      ),
      "repeated-section-outside-reference-task-list.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_repeatingField"  -> One("1"),
        "2_addAnotherField" -> Many(List("0")),
        "2_repeatingField"  -> One("2"),
        "3_addAnotherField" -> Many(List("1")),
        "3_repeatingField"  -> One("3")
      ),
      List(
        "0,0,ap0.1.0",
        "0,0,ar0.1",
        "0,0,ap0.2.0",
        "0,0,ar0.2",
        "0,0,ap0.3.0",
        "0,0,ar0.3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_repeatingField" -> NumberResult(1),
        "2_repeatingField" -> NumberResult(2),
        "3_repeatingField" -> NumberResult(3),
        "test"             -> Empty
      ),
      List(
        "1. 1",
        """|<ol class="govuk-list govuk-list--number">
           | <li>1</li>
           |</ol>""".stripMargin,
        "2. 2",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>2</li>
           |</ol>""".stripMargin,
        "3. 3",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>3</li>
           |</ol>""".stripMargin,
        "Basic info about 1, 2, 3 sum of which is 6",
        "<p>Basic info about 1, 2, 3 sum of which is 6</p>"
      ),
      "atl-outside-reference-task-list.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_repeatingField"  -> One("1"),
        "2_addAnotherField" -> Many(List("0")),
        "2_repeatingField"  -> One("2"),
        "3_addAnotherField" -> Many(List("1")),
        "3_repeatingField"  -> One("3")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_repeatingField" -> NumberResult(1),
        "2_repeatingField" -> NumberResult(2),
        "3_repeatingField" -> NumberResult(3),
        "test"             -> Empty
      ),
      List(
        "1. 1",
        """|<ol class="govuk-list govuk-list--number">
           | <li>1</li>
           |</ol>""".stripMargin,
        "2. 2",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>2</li>
           |</ol>""".stripMargin,
        "3. 3",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>3</li>
           |</ol>""".stripMargin,
        "Basic info about 1, 2, 3 sum of which is 6",
        "<p>Basic info about 1, 2, 3 sum of which is 6</p>"
      ),
      "atl-outside-reference.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_repeatingField"  -> Many(List("film")),
        "2_addAnotherField" -> Many(List("0")),
        "2_repeatingField"  -> Many(List("game")),
        "3_addAnotherField" -> Many(List("1")),
        "3_repeatingField"  -> Many(List("book"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "1. Film",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Film</li>
           |</ol>""".stripMargin,
        "2. Game",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>Game</li>
           |</ol>""".stripMargin,
        "3. Book",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>Book</li>
           |</ol>""".stripMargin,
        "Selected choices: film, game, book",
        "<p>Selected choices: film, game, book</p>"
      ),
      "atl-outside-reference-choice.json direct choice reference"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_repeatingField"  -> Many(List("game")),
        "2_addAnotherField" -> Many(List("1")),
        "2_repeatingField"  -> Many(List("book"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "1. Game",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Game</li>
           |</ol>""".stripMargin,
        "2. Book",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>Book</li>
           |</ol>""".stripMargin,
        "Selected choices: Game, Book",
        "<p>Selected choices: Game, Book</p>"
      ),
      "atl-outside-reference-choice.json choice reference in choice function"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_repeatingField"  -> One("AAA"),
        "2_addAnotherField" -> Many(List("0")),
        "2_repeatingField"  -> One("BBB"),
        "3_addAnotherField" -> Many(List("1")),
        "3_repeatingField"  -> One("CCC")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "1. AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>AAA</li>
           |</ol>""".stripMargin,
        "2. BBB",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>BBB</li>
           |</ol>""".stripMargin,
        "3. CCC",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>CCC</li>
           |</ol>""".stripMargin,
        "Selected choices: AAA, BBB, CCC",
        "<p>Selected choices: AAA, BBB, CCC</p>"
      ),
      "atl-outside-reference-text-area.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_genre"           -> Many(List("film")),
        "2_addAnotherField" -> Many(List("1")),
        "2_genre"           -> Many(List("game")),
        "2_repeatingField"  -> One("AAA"),
        "test"              -> One("123")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ar0.2",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_genre"          -> OptionResult(List("film")),
        "2_genre"          -> OptionResult(List("game")),
        "1_repeatingField" -> Hidden,
        "2_repeatingField" -> StringResult("AAA"),
        "test"             -> NumberResult(123)
      ),
      List(
        "1. Film ",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Film</li>
           |</ol>""".stripMargin,
        "2. Game AAA",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>Game AAA</li>
           |</ol>""".stripMargin
      ),
      "atl-outside-reference-count.json show test field with value"
    ),
    (
      MongoUserData(
        "1_genre" -> Many(List("game"))
      ),
      List(
        "ap0.1.0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_genre"          -> OptionResult(List("game")),
        "1_repeatingField" -> Empty,
        "test"             -> Empty
      ),
      List(
        "1. Game ",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Game</li>
           |</ol>""".stripMargin
      ),
      "atl-outside-reference-count.json show test field"
    ),
    (
      MongoUserData(
        "1_genre" -> Many(List("film"))
      ),
      List(
        "ap0.1.0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_genre"          -> OptionResult(List("film")),
        "1_repeatingField" -> Hidden,
        "test"             -> Hidden
      ),
      List(
        "1. Film ",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Film</li>
           |</ol>""".stripMargin
      ),
      "atl-outside-reference-count.json hide test field"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_check"           -> One("1"),
        "1_genre"           -> Many(List("game")),
        "1_repeatingField"  -> One("AAA"),
        "2_addAnotherField" -> Many(List("0")),
        "2_check"           -> One("2"),
        "2_genre"           -> Many(List("film")),
        "3_genre"           -> Many(List("game")),
        "3_repeatingField"  -> One("CCC")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ap0.1.2",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.2",
        "ar0.2",
        "ap0.3.0",
        "ap0.3.1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_genre"          -> OptionResult(List("game")),
        "1_repeatingField" -> StringResult("AAA"),
        "1_secret"         -> Hidden,
        "2_genre"          -> OptionResult(List("film")),
        "2_repeatingField" -> Hidden,
        "2_secret"         -> Hidden,
        "3_genre"          -> OptionResult(List("game")),
        "3_repeatingField" -> StringResult("CCC"),
        "3_secret"         -> Hidden
      ),
      List(
        "Check page 1 2",
        "1. Game AAA",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Game AAA</li>
           |</ol>""".stripMargin,
        "Check page 2 2",
        "2. Film ",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>Film</li>
           |</ol>""".stripMargin,
        "Check page 3 2",
        "3. Game CCC",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>Game CCC</li>
           |</ol>""".stripMargin
      ),
      "atl-inside-reference-count.json count condition false"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_basicInfo"       -> One(""),
        "1_check"           -> One("AAA"),
        "1_genre"           -> Many(List("film")),
        "1_secret"          -> Many(List("reveal")),
        "2_addAnotherField" -> Many(List("0")),
        "2_check"           -> One("BBB"),
        "2_genre"           -> Many(List("game")),
        "2_repeatingField"  -> One("BBB"),
        "2_secret"          -> Many(List("keepHidden")),
        "3_addAnotherField" -> Many(List("0")),
        "3_basicInfo"       -> One(""),
        "3_check"           -> One("CCC"),
        "3_genre"           -> Many(List("game")),
        "3_repeatingField"  -> One("CCC"),
        "3_secret"          -> Many(List("reveal")),
        "4_addAnotherField" -> Many(List("1")),
        "4_basicInfo"       -> One(""),
        "4_check"           -> One("DDD"),
        "4_genre"           -> Many(List("game")),
        "4_repeatingField"  -> One("DDD"),
        "4_secret"          -> Many(List("reveal"))
      ),
      List(
        "ap0.1.0",
        "ap0.1.2",
        "ap0.1.3",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ap0.2.2",
        "ar0.2",
        "ap0.3.0",
        "ap0.3.1",
        "ap0.3.2",
        "ap0.3.3",
        "ar0.3",
        "ap0.4.0",
        "ap0.4.1",
        "ap0.4.2",
        "ap0.4.3",
        "ar0.4"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_genre"          -> OptionResult(List("film")),
        "1_secret"         -> OptionResult(List("reveal")),
        "1_repeatingField" -> Hidden,
        "2_genre"          -> OptionResult(List("game")),
        "2_repeatingField" -> StringResult("BBB"),
        "2_secret"         -> OptionResult(List("keepHidden")),
        "3_genre"          -> OptionResult(List("game")),
        "3_repeatingField" -> StringResult("CCC"),
        "3_secret"         -> OptionResult(List("reveal")),
        "4_genre"          -> OptionResult(List("game")),
        "4_repeatingField" -> StringResult("DDD"),
        "4_secret"         -> OptionResult(List("reveal"))
      ),
      List(
        "Check page 1 3",
        "tada 1",
        "<p>tada 1</p>",
        "1. Film ",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Film</li>
           |</ol>""".stripMargin,
        "Check page 2 3",
        "2. Game BBB",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>Game BBB</li>
           |</ol>""".stripMargin,
        "Check page 3 3",
        "tada 3",
        "<p>tada 3</p>",
        "3. Game CCC",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>Game CCC</li>
           |</ol>""".stripMargin,
        "Check page 4 3",
        "tada 4",
        "<p>tada 4</p>",
        "4. Game DDD",
        """|<ol start="4" class="govuk-list govuk-list--number">
           | <li>Game DDD</li>
           |</ol>""".stripMargin
      ),
      "atl-inside-reference-count.json count condition true"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_genre"           -> Many(List("game")),
        "2_addAnotherField" -> Many(List("0")),
        "2_genre"           -> Many(List("film")),
        "3_addAnotherField" -> Many(List("0")),
        "3_genre"           -> Many(List("game")),
        "4_addAnotherField" -> Many(List("0")),
        "4_genre"           -> Many(List("game")),
        "5_addAnotherField" -> Many(List("0")),
        "5_genre"           -> Many(List("film")),
        "6_addAnotherField" -> Many(List("1")),
        "6_genre"           -> Many(List("film"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "ap0.4.0",
        "ar0.4",
        "ap0.5.0",
        "ar0.5",
        "ap0.6.0",
        "ar0.6"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "1. Game - Total films: 0. Total games: 1.",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Game - Total films: 0. Total games: 1.</li>
           |</ol>""".stripMargin,
        "2. Film - Total films: 1. Total games: 1.",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>Film - Total films: 1. Total games: 1.</li>
           |</ol>""".stripMargin,
        "3. Game - Total films: 1. Total games: 2.",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>Game - Total films: 1. Total games: 2.</li>
           |</ol>""".stripMargin,
        "4. Game - Total films: 1. Total games: 3.",
        """|<ol start="4" class="govuk-list govuk-list--number">
           | <li>Game - Total films: 1. Total games: 3.</li>
           |</ol>""".stripMargin,
        "5. Film - Total films: 2. Total games: 3.",
        """|<ol start="5" class="govuk-list govuk-list--number">
           | <li>Film - Total films: 2. Total games: 3.</li>
           |</ol>""".stripMargin,
        "6. Film - Total films: 3. Total games: 3.",
        """|<ol start="6" class="govuk-list govuk-list--number">
           | <li>Film - Total films: 3. Total games: 3.</li>
           |</ol>""".stripMargin
      ),
      "atl-inside-reference-size.json Generated"
    ),
    (
      MongoUserData(
        "1_genre" -> Many(List("game"))
      ),
      List(
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_genre"  -> OptionResult(List("game")),
        "1_secret" -> Hidden
      ),
      List(
        "1. Game.",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Game.</li>
           |</ol>""".stripMargin
      ),
      "atl-inside-reference-count-choice.json secrets are hidden"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_genre"           -> Many(List("game")),
        "2_addAnotherField" -> Many(List("0")),
        "2_genre"           -> Many(List("film")),
        "3_genre"           -> Many(List("game"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_genre"  -> OptionResult(List("game")),
        "1_secret" -> Empty,
        "2_genre"  -> OptionResult(List("film")),
        "2_secret" -> Empty,
        "3_genre"  -> OptionResult(List("game")),
        "3_secret" -> Empty
      ),
      List(
        "1. Game.",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Game.</li>
           |</ol>""".stripMargin,
        "2. Film.",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>Film.</li>
           |</ol>""".stripMargin,
        "3. Game.",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>Game.</li>
           |</ol>""".stripMargin
      ),
      "atl-inside-reference-count-choice.json reveal all secrets"
    ),
    (
      MongoUserData(
        "1_genre" -> Many(List("film"))
      ),
      List(
        "ap0.1.0"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_genre"  -> OptionResult(List("film")),
        "1_secret" -> Hidden
      ),
      List(
        "1. Film.",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Film.</li>
           |</ol>""".stripMargin
      ),
      "atl-inside-reference-size-choice.json after first genre answer"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_genre"           -> Many(List("film")),
        "2_addAnotherField" -> Many(List("0")),
        "2_genre"           -> Many(List("game")),
        "3_addAnotherField" -> Many(List("0")),
        "3_genre"           -> Many(List("film")),
        "4_addAnotherField" -> Many(List("1")),
        "4_genre"           -> Many(List("film")),
        "4_secret"          -> Many(List("reveal"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "ap0.4.0",
        "ap0.4.1",
        "ar0.4"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_genre"  -> OptionResult(List("film")),
        "1_secret" -> Hidden,
        "2_genre"  -> OptionResult(List("game")),
        "2_secret" -> Hidden,
        "3_genre"  -> OptionResult(List("film")),
        "3_secret" -> Hidden,
        "4_genre"  -> OptionResult(List("film")),
        "4_secret" -> OptionResult(List("reveal"))
      ),
      List(
        "1. Film.",
        """|<ol class="govuk-list govuk-list--number">
           | <li>Film.</li>
           |</ol>""".stripMargin,
        "2. Game.",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>Game.</li>
           |</ol>""".stripMargin,
        "3. Film.",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>Film.</li>
           |</ol>""".stripMargin,
        "4. Film.",
        """|<ol start="4" class="govuk-list govuk-list--number">
           | <li>Film.</li>
           |</ol>""".stripMargin
      ),
      "atl-inside-reference-size-choice.json first three secrets are hidden and the last one is visible"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_tax"             -> One("1"),
        "2_addAnotherField" -> Many(List("0")),
        "2_tax"             -> One("2"),
        "3_addAnotherField" -> Many(List("0")),
        "3_tax"             -> One("3"),
        "4_addAnotherField" -> Many(List("0")),
        "4_tax"             -> One("4"),
        "5_addAnotherField" -> Many(List("1")),
        "5_secret"          -> Many(List("reveal")),
        "5_tax"             -> One("5")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "ap0.4.0",
        "ar0.4",
        "ap0.5.0",
        "ap0.5.1",
        "ar0.5"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_secret" -> Hidden,
        "1_tax"    -> NumberResult(1),
        "2_secret" -> Hidden,
        "2_tax"    -> NumberResult(2),
        "3_secret" -> Hidden,
        "3_tax"    -> NumberResult(3),
        "4_secret" -> Hidden,
        "4_tax"    -> NumberResult(4),
        "5_secret" -> OptionResult(List("reveal")),
        "5_tax"    -> NumberResult(5)
      ),
      List(
        "1 - £1.00 - Sum: £1.00.",
        "<p>1 - £1.00 - Sum: £1.00.</p>",
        "2 - £2.00 - Sum: £3.00.",
        "<p>2 - £2.00 - Sum: £3.00.</p>",
        "3 - £3.00 - Sum: £6.00.",
        "<p>3 - £3.00 - Sum: £6.00.</p>",
        "4 - £4.00 - Sum: £10.00.",
        "<p>4 - £4.00 - Sum: £10.00.</p>",
        "Secret 5 £15.00",
        "5 - £5.00 - Sum: £15.00.",
        "<p>5 - £5.00 - Sum: £15.00.</p>"
      ),
      "atl-inside-reference-sum.json first four secrets are hidden and the last one is visible"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_name"       -> One("AAA"),
        "2_addAnother" -> Many(List("0")),
        "2_name"       -> One("BBB"),
        "3_addAnother" -> Many(List("0")),
        "3_name"       -> One("CCC"),
        "4_addAnother" -> Many(List("0")),
        "4_name"       -> One("DDD"),
        "5_addAnother" -> Many(List("0")),
        "5_name"       -> One("EEE"),
        "6_addAnother" -> Many(List("1")),
        "6_name"       -> One("FFF")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "ap0.4.0",
        "ar0.4",
        "ap0.5.0",
        "ar0.5",
        "ap0.6.0",
        "ar0.6"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "Enter first name",
        "1 - AAA.",
        "<p>1 - AAA.</p>",
        "Enter second name",
        "2 - BBB.",
        "<p>2 - BBB.</p>",
        "Enter third name",
        "3 - CCC.",
        "<p>3 - CCC.</p>",
        "Enter fourth name",
        "4 - DDD.",
        "<p>4 - DDD.</p>",
        "Enter fifth name",
        "5 - EEE.",
        "<p>5 - EEE.</p>",
        "Enter name 6",
        "6 - FFF.",
        "<p>6 - FFF.</p>"
      ),
      "atl-inside-reference-index.json only smartstring reference"
    ),
    (
      MongoUserData(
      ),
      List(),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        "Enter first name",
        "1 - .",
        "<p>1 - .</p>"
      ),
      "atl-inside-reference-index.json evaluation without data"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_name"       -> One("AAA"),
        "2_addAnother" -> Many(List("0")),
        "2_name"       -> One("BBB"),
        "2_secret"     -> Many(List("reveal")),
        "3_addAnother" -> Many(List("1")),
        "3_name"       -> One("CCC")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ar0.2",
        "ap0.3.0",
        "ap0.3.1",
        "ar0.3"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_addAnother" -> OptionResult(List("0")),
        "1_secret"     -> Hidden,
        "2_addAnother" -> OptionResult(List("0")),
        "2_secret"     -> OptionResult(List("reveal")),
        "3_addAnother" -> OptionResult(List("1")),
        "3_secret"     -> Hidden
      ),
      List(
        "1 - AAA.",
        "<p>1 - AAA.</p>",
        "2 - BBB.",
        "<p>2 - BBB.</p>",
        "3 - CCC.",
        "<p>3 - CCC.</p>"
      ),
      "atl-inside-reference-index-choice.json show second secret only"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_name"       -> One("AAA"),
        "2_addAnother" -> Many(List("1")),
        "2_name"       -> One("BBB")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_tax"      -> Empty,
        "2_tax"      -> Empty,
        "correction" -> Hidden
      ),
      List(
        "1 - AAA.",
        "<p>1 - AAA.</p>",
        "2 - BBB.",
        "<p>2 - BBB.</p>",
        """|Name 1: AAA
           |
           |Name 2: BBB""".stripMargin,
        """|<p>Name 1: AAA</p>
           |<p>Name 2: BBB</p>""".stripMargin
      ),
      "atl-outside-reference-index-of.json missing data handling"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_name"       -> One("AAA"),
        "1_tax"        -> One("101"),
        "2_addAnother" -> Many(List("1")),
        "2_name"       -> One("BBB"),
        "2_tax"        -> One("10"),
        "correction"   -> One("123"),
        "dummy"        -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_tax"      -> NumberResult(101),
        "2_tax"      -> NumberResult(10),
        "correction" -> NumberResult(123)
      ),
      List(
        "1 - AAA.",
        "<p>1 - AAA.</p>",
        "2 - BBB.",
        "<p>2 - BBB.</p>",
        """|Name 1: AAA
           |
           |Name 2: BBB""".stripMargin,
        """|<p>Name 1: AAA</p>
           |<p>Name 2: BBB</p>""".stripMargin
      ),
      "atl-outside-reference-index-of.json show correction"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_name"       -> One("CCC"),
        "1_present"    -> One("101"),
        "2_addAnother" -> Many(List("1")),
        "2_name"       -> One("BBB"),
        "2_present"    -> One("10"),
        "correction"   -> One("123"),
        "dummy"        -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ar0.2",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_name"     -> StringResult("CCC"),
        "1_present"  -> Hidden,
        "2_name"     -> StringResult("BBB"),
        "2_present"  -> NumberResult(10),
        "correction" -> Hidden
      ),
      List(
        "1 - CCC.",
        "<p>1 - CCC.</p>",
        "Present for BBB - first name was CCC",
        "2 - BBB.",
        "<p>2 - BBB.</p>",
        """|Present 1: £0.00
           |
           |Present 2: £10.00""".stripMargin,
        """|<p>Present 1: £0.00</p>
           |<p>Present 2: £10.00</p>""".stripMargin
      ),
      "atl-outside-reference-index-of-conditional.json handle calculations"
    ),
    (
      MongoUserData(
        "1_addAnother"          -> Many(List("0")),
        "1_calendarDate-day"    -> One("1"),
        "1_calendarDate-month"  -> One("2"),
        "1_date-day"            -> One("1"),
        "1_date-month"          -> One("2"),
        "1_date-year"           -> One("2003"),
        "1_taxPeriodDate-month" -> One("2"),
        "1_taxPeriodDate-year"  -> One("2003"),
        "2_addAnother"          -> Many(List("0")),
        "2_calendarDate-day"    -> One("4"),
        "2_calendarDate-month"  -> One("5"),
        "2_date-day"            -> One("4"),
        "2_date-month"          -> One("5"),
        "2_date-year"           -> One("2006"),
        "2_taxPeriodDate-month" -> One("5"),
        "2_taxPeriodDate-year"  -> One("2006"),
        "3_addAnother"          -> Many(List("0")),
        "3_calendarDate-day"    -> One("7"),
        "3_calendarDate-month"  -> One("8"),
        "3_date-day"            -> One("7"),
        "3_date-month"          -> One("8"),
        "3_date-year"           -> One("2009"),
        "3_taxPeriodDate-month" -> One("8"),
        "3_taxPeriodDate-year"  -> One("2009"),
        "4_addAnother"          -> Many(List("1")),
        "4_calendarDate-day"    -> One("10"),
        "4_calendarDate-month"  -> One("11"),
        "4_date-day"            -> One("10"),
        "4_date-month"          -> One("11"),
        "4_date-year"           -> One("2012"),
        "4_taxPeriodDate-month" -> One("11"),
        "4_taxPeriodDate-year"  -> One("2012"),
        "dummy"                 -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "ap0.3.0",
        "ar0.3",
        "ap0.4.0",
        "ar0.4",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_date-day"   -> NumberResult(1),
        "1_date-month" -> NumberResult(2),
        "1_date-year"  -> NumberResult(2003),
        "2_date-day"   -> NumberResult(4),
        "2_date-month" -> NumberResult(5),
        "2_date-year"  -> NumberResult(2006),
        "3_date-day"   -> NumberResult(7),
        "3_date-month" -> NumberResult(8),
        "3_date-year"  -> NumberResult(2009),
        "4_date-day"   -> NumberResult(10),
        "4_date-month" -> NumberResult(11),
        "4_date-year"  -> NumberResult(2012),
        "secret"       -> Empty
      ),
      List(
        "1 - 1 February 2003.",
        "<p>1 - 1 February 2003.</p>",
        "2 - 4 May 2006.",
        "<p>2 - 4 May 2006.</p>",
        "3 - 7 August 2009.",
        "<p>3 - 7 August 2009.</p>",
        "4 - 10 November 2012.",
        "<p>4 - 10 November 2012.</p>",
        """|All dates: 1 February 2003, 4 May 2006, 7 August 2009, 10 November 2012
           |
           |date[1]: 1 February 2003
           |
           |date[2] = '' : 4 May 2006
           |
           |'' = date[2] : 4 May 2006
           |
           |All calendar dates: 1 February, 4 May, 7 August, 10 November
           |
           |calendarDate[1]: 1 February
           |
           |calendarDate[2] = '' : 4 May
           |
           |'' = calendarDate[2] : 4 May
           |
           |All tax period dates: February 2003, May 2006, August 2009, November 2012
           |
           |taxPeriodDate[1]: February 2003
           |
           |taxPeriodDate[2] = '' : May 2006
           |
           |'' = taxPeriodDate[2] : May 2006""".stripMargin,
        """|<p>All dates: 1 February 2003, 4 May 2006, 7 August 2009, 10 November 2012</p>
           |<p>date[1]: 1 February 2003</p>
           |<p>date[2] = '' : 4 May 2006</p>
           |<p>'' = date[2] : 4 May 2006</p>
           |<p>All calendar dates: 1 February, 4 May, 7 August, 10 November</p>
           |<p>calendarDate[1]: 1 February</p>
           |<p>calendarDate[2] = '' : 4 May</p>
           |<p>'' = calendarDate[2] : 4 May</p>
           |<p>All tax period dates: February 2003, May 2006, August 2009, November 2012</p>
           |<p>taxPeriodDate[1]: February 2003</p>
           |<p>taxPeriodDate[2] = '' : May 2006</p>
           |<p>'' = taxPeriodDate[2] : May 2006</p>""".stripMargin
      ),
      "atl-outside-reference-index-of-date.json show secret page"
    ),
    (
      MongoUserData(
        "1_addAnother"          -> Many(List("1")),
        "1_calendarDate-day"    -> One("1"),
        "1_calendarDate-month"  -> One("1"),
        "1_date-day"            -> One("1"),
        "1_date-month"          -> One("1"),
        "1_date-year"           -> One("2001"),
        "1_taxPeriodDate-month" -> One("1"),
        "1_taxPeriodDate-year"  -> One("2001"),
        "dummy"                 -> One("dummy")
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_date-day"   -> NumberResult(1),
        "1_date-month" -> NumberResult(1),
        "1_date-year"  -> NumberResult(2001),
        "secret"       -> Hidden
      ),
      List(
        "1 - 1 January 2001.",
        "<p>1 - 1 January 2001.</p>",
        """|All dates: 1 January 2001
           |
           |date[1]: 1 January 2001
           |
           |date[2] = '' : yes
           |
           |'' = date[2] : yes
           |
           |All calendar dates: 1 January
           |
           |calendarDate[1]: 1 January
           |
           |calendarDate[2] = '' : yes
           |
           |'' = calendarDate[2] : yes
           |
           |All tax period dates: January 2001
           |
           |taxPeriodDate[1]: January 2001
           |
           |taxPeriodDate[2] = '' : yes
           |
           |'' = taxPeriodDate[2] : yes""".stripMargin,
        """<p>All dates: 1 January 2001</p>
          |<p>date[1]: 1 January 2001</p>
          |<p>date[2] = '' : yes</p>
          |<p>'' = date[2] : yes</p>
          |<p>All calendar dates: 1 January</p>
          |<p>calendarDate[1]: 1 January</p>
          |<p>calendarDate[2] = '' : yes</p>
          |<p>'' = calendarDate[2] : yes</p>
          |<p>All tax period dates: January 2001</p>
          |<p>taxPeriodDate[1]: January 2001</p>
          |<p>taxPeriodDate[2] = '' : yes</p>
          |<p>'' = taxPeriodDate[2] : yes</p>""".stripMargin
      ),
      "atl-outside-reference-index-of-date.json hide secret page"
    ),
    (
      MongoUserData(
        "dummy"  -> One("dummy"),
        "name"   -> One("john de Doe"),
        "secret" -> Many(List("reveal")),
        "shout"  -> One("Help!"),
        "tax"    -> One("123")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "name"   -> StringResult("john de Doe"),
        "secret" -> OptionResult(List("reveal"))
      ),
      List(
        """|Name: john de Doe
           |
           |Shout: Help!
           |
           |Tax: £123.00
           |
           |Uppercase name: JOHN DE DOE
           |
           |Lowercase name: john de doe
           |
           |Uppercase tax 123
           |
           |Capitalize name: John de Doe
           |
           |CapitalizeAll name: John De Doe
           |
           |Remove spaces name: johndeDoe
           |
           |LowercaseFirst shout: help!
           |
           |Substring(4) name:  de Doe
           |
           |Substring(4, 10) name:  de Do""".stripMargin,
        """<p>Name: john de Doe</p>
          |<p>Shout: Help!</p>
          |<p>Tax: £123.00</p>
          |<p>Uppercase name: JOHN DE DOE</p>
          |<p>Lowercase name: john de doe</p>
          |<p>Uppercase tax 123</p>
          |<p>Capitalize name: John de Doe</p>
          |<p>CapitalizeAll name: John De Doe</p>
          |<p>Remove spaces name: johndeDoe</p>
          |<p>LowercaseFirst shout: help!</p>
          |<p>Substring(4) name: de Doe</p>
          |<p>Substring(4, 10) name: de Do</p>""".stripMargin
      ),
      "string-functions.json show secret"
    ),
    (
      MongoUserData(
        "dummy"  -> One("dummy"),
        "name"   -> One("john Doe"),
        "secret" -> Many(List("reveal")),
        "shout"  -> One("Help!"),
        "tax"    -> One("123")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "name"   -> StringResult("john Doe"),
        "secret" -> Hidden
      ),
      List(
        """|Name: john Doe
           |
           |Shout: Help!
           |
           |Tax: £123.00
           |
           |Uppercase name: JOHN DOE
           |
           |Lowercase name: john doe
           |
           |Uppercase tax 123
           |
           |Capitalize name: John Doe
           |
           |CapitalizeAll name: John Doe
           |
           |Remove spaces name: johnDoe
           |
           |LowercaseFirst shout: help!
           |
           |Substring(4) name:  Doe
           |
           |Substring(4, 10) name:  Doe""".stripMargin,
        """<p>Name: john Doe</p>
          |<p>Shout: Help!</p>
          |<p>Tax: £123.00</p>
          |<p>Uppercase name: JOHN DOE</p>
          |<p>Lowercase name: john doe</p>
          |<p>Uppercase tax 123</p>
          |<p>Capitalize name: John Doe</p>
          |<p>CapitalizeAll name: John Doe</p>
          |<p>Remove spaces name: johnDoe</p>
          |<p>LowercaseFirst shout: help!</p>
          |<p>Substring(4) name: Doe</p>
          |<p>Substring(4, 10) name: Doe</p>""".stripMargin
      ),
      "string-functions.json hide secret"
    ),
    (
      MongoUserData(
        "1_addAnotherField" -> Many(List("0")),
        "1_name"            -> One("John de Doe"),
        "1_shout"           -> One("Help!"),
        "2_addAnotherField" -> Many(List("1")),
        "2_name"            -> One("Malina East"),
        "2_shout"           -> One("Get out!"),
        "dummy"             -> One("dummy"),
        "secret"            -> Many(List("reveal"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_name" -> StringResult("John de Doe"),
        "2_name" -> StringResult("Malina East"),
        "secret" -> OptionResult(List("reveal"))
      ),
      List(
        "1. john de doe - HELP!",
        """<ol class="govuk-list govuk-list--number">
          | <li>john de doe - HELP!</li>
          |</ol>""".stripMargin,
        "2. malina east - GET OUT!",
        """<ol start="2" class="govuk-list govuk-list--number">
          | <li>malina east - GET OUT!</li>
          |</ol>""".stripMargin,
        """|Name: John de Doe, Malina East
           |
           |Shout: Help!, Get out!
           |
           |Uppercase name: JOHN DE DOE, MALINA EAST
           |
           |Lowercase name: john de doe, malina east
           |
           |Capitalize name: John de Doe, Malina East
           |
           |CapitalizeAll name: John De Doe, Malina East
           |
           |Remove spaces name: JohndeDoe,MalinaEast
           |
           |LowercaseFirst shout: help!, Get out!
           |
           |Substring(4) name:  de Doe, Malina East
           |
           |Substring(4, 10) name:  de Do""".stripMargin,
        """<p>Name: John de Doe, Malina East</p>
          |<p>Shout: Help!, Get out!</p>
          |<p>Uppercase name: JOHN DE DOE, MALINA EAST</p>
          |<p>Lowercase name: john de doe, malina east</p>
          |<p>Capitalize name: John de Doe, Malina East</p>
          |<p>CapitalizeAll name: John De Doe, Malina East</p>
          |<p>Remove spaces name: JohndeDoe,MalinaEast</p>
          |<p>LowercaseFirst shout: help!, Get out!</p>
          |<p>Substring(4) name: de Doe, Malina East</p>
          |<p>Substring(4, 10) name: de Do</p>""".stripMargin
      ),
      "string-functions-atl.json list support"
    ),
    (
      MongoUserData(
        "dateA-day"   -> One("1"),
        "dateA-month" -> One("1"),
        "dateA-year"  -> One("2000"),
        "dateB-day"   -> One("25"),
        "dateB-month" -> One("6"),
        "dateB-year"  -> One("2012"),
        "dummy"       -> One("dummy")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(),
      List(
        """|Date A: 1 January 2000
           |
           |Date B: 25 June 2012
           |
           |Period: P12Y5M24D
           |
           |Total months: 149
           |
           |Total year: 12
           |
           |Months: 5
           |
           |Days: 24
           |
           |Sum: P12Y5M24D
           |
           |P12Y5M24D is less than 15 years
           |
           |P12Y5M24D is more than 10 years
           |
           |P12Y5M24D - P-1M = P12Y6M24D
           |
           |P12Y5M24D + P-1M = P12Y4M24D
           |
           |P12Y5M24D - P1M = P12Y4M24D
           |
           |P12Y5M24D + P1M = P12Y6M24D
           |
           |P1Y - P1Y: P0D
           |
           |P1Y + P1Y: P2Y
           |
           |period(dateA, dateB) = period(dateA, dateB) - P-1M: Not equal
           |
           |period(dateA, dateB) = period(dateA, dateB): Equal
           |
           |P1Y < P1Y: False
           |
           |P1Y <= P1Y: True
           |
           |P1Y > P1Y: False
           |
           |P1Y >= P1Y: True
           |
           |P1Y-1M < P1Y: True
           |
           |P1Y1M <= P1Y: False
           |
           |P1Y1M > P1Y: True
           |
           |P1Y-1M >= P1Y: False""".stripMargin,
        """|<p>Date A: 1 January 2000</p>
           |<p>Date B: 25 June 2012</p>
           |<p>Period: P12Y5M24D</p>
           |<p>Total months: 149</p>
           |<p>Total year: 12</p>
           |<p>Months: 5</p>
           |<p>Days: 24</p>
           |<p>Sum: P12Y5M24D</p>
           |<p>P12Y5M24D is less than 15 years</p>
           |<p>P12Y5M24D is more than 10 years</p>
           |<p>P12Y5M24D - P-1M = P12Y6M24D</p>
           |<p>P12Y5M24D + P-1M = P12Y4M24D</p>
           |<p>P12Y5M24D - P1M = P12Y4M24D</p>
           |<p>P12Y5M24D + P1M = P12Y6M24D</p>
           |<p>P1Y - P1Y: P0D</p>
           |<p>P1Y + P1Y: P2Y</p>
           |<p>period(dateA, dateB) = period(dateA, dateB) - P-1M: Not equal</p>
           |<p>period(dateA, dateB) = period(dateA, dateB): Equal</p>
           |<p>P1Y &lt; P1Y: False</p>
           |<p>P1Y &lt;= P1Y: True</p>
           |<p>P1Y &gt; P1Y: False</p>
           |<p>P1Y &gt;= P1Y: True</p>
           |<p>P1Y-1M &lt; P1Y: True</p>
           |<p>P1Y1M &lt;= P1Y: False</p>
           |<p>P1Y1M &gt; P1Y: True</p>
           |<p>P1Y-1M &gt;= P1Y: False</p>""".stripMargin
      ),
      "period-between-two-dates.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"  -> Many(List("0")),
        "1_dateA-day"   -> One("1"),
        "1_dateA-month" -> One("1"),
        "1_dateA-year"  -> One("2001"),
        "1_dateB-day"   -> One("2"),
        "1_dateB-month" -> One("2"),
        "1_dateB-year"  -> One("2002"),
        "2_addAnother"  -> Many(List("1")),
        "2_dateA-day"   -> One("4"),
        "2_dateA-month" -> One("4"),
        "2_dateA-year"  -> One("2004"),
        "2_dateB-day"   -> One("6"),
        "2_dateB-month" -> One("6"),
        "2_dateB-year"  -> One("2006"),
        "dummy"         -> One("dummy"),
        "secret"        -> Many(List("reveal"))
      ),
      List(
        "ap0.1.0",
        "ar0.1",
        "ap0.2.0",
        "ar0.2",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_dateA-day"   -> NumberResult(1),
        "1_dateA-month" -> NumberResult(1),
        "1_dateA-year"  -> NumberResult(2001),
        "1_dateB-day"   -> NumberResult(2),
        "1_dateB-month" -> NumberResult(2),
        "1_dateB-year"  -> NumberResult(2002),
        "2_dateA-day"   -> NumberResult(4),
        "2_dateA-month" -> NumberResult(4),
        "2_dateA-year"  -> NumberResult(2004),
        "2_dateB-day"   -> NumberResult(6),
        "2_dateB-month" -> NumberResult(6),
        "2_dateB-year"  -> NumberResult(2006),
        "secret"        -> OptionResult(List("reveal"))
      ),
      List(
        "1. 1 January 2001 - 2 February 2002.",
        """<ol class="govuk-list govuk-list--number">
          | <li>1 January 2001 - 2 February 2002.</li>
          |</ol>""".stripMargin,
        "2. 4 April 2004 - 6 June 2006.",
        """<ol start="2" class="govuk-list govuk-list--number">
          | <li>4 April 2004 - 6 June 2006.</li>
          |</ol>""".stripMargin,
        """|Date A: 1 January 2001, 4 April 2004
           |
           |Date B: 2 February 2002, 6 June 2006
           |
           |Period: P1Y1M1D, P2Y2M2D
           |
           |Total months: 39
           |
           |Total year: 3
           |
           |Months: 3
           |
           |Days: 3
           |
           |Sum: P3Y3M3D""".stripMargin,
        """<p>Date A: 1 January 2001, 4 April 2004</p>
          |<p>Date B: 2 February 2002, 6 June 2006</p>
          |<p>Period: P1Y1M1D, P2Y2M2D</p>
          |<p>Total months: 39</p>
          |<p>Total year: 3</p>
          |<p>Months: 3</p>
          |<p>Days: 3</p>
          |<p>Sum: P3Y3M3D</p>""".stripMargin
      ),
      "period-between-two-dates-atl.json Generated"
    ),
    (
      MongoUserData(
        "1_addAnother"  -> Many(List("0")),
        "1_dateA-day"   -> One("1"),
        "1_dateA-month" -> One("1"),
        "1_dateA-year"  -> One("2001"),
        "1_dateB-day"   -> One("2"),
        "1_dateB-month" -> One("2"),
        "1_dateB-year"  -> One("2002"),
        "1_dateControl" -> Many(List("showBoth")),
        "1_dummy"       -> One("AAA"),
        "2_addAnother"  -> Many(List("0")),
        "2_dateA-day"   -> One("4"),
        "2_dateA-month" -> One("4"),
        "2_dateA-year"  -> One("2004"),
        "2_dateControl" -> Many(List("showDateA")),
        "2_dummy"       -> One("BBB"),
        "3_addAnother"  -> Many(List("0")),
        "3_dateB-day"   -> One("5"),
        "3_dateB-month" -> One("5"),
        "3_dateB-year"  -> One("2005"),
        "3_dateControl" -> Many(List("showDateB")),
        "3_dummy"       -> One("CCC"),
        "4_addAnother"  -> Many(List("0")),
        "4_dateControl" -> Many(List("hideBoth")),
        "4_dummy"       -> One("DDD"),
        "5_addAnother"  -> Many(List("1")),
        "5_dateA-day"   -> One("7"),
        "5_dateA-month" -> One("7"),
        "5_dateA-year"  -> One("2007"),
        "5_dateB-day"   -> One("8"),
        "5_dateB-month" -> One("8"),
        "5_dateB-year"  -> One("2008"),
        "5_dateControl" -> Many(List("showBoth")),
        "5_dummy"       -> One("EEE"),
        "dummy2"        -> One("FFF"),
        "secret"        -> Many(List("reveal"))
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ar0.2",
        "ap0.3.0",
        "ap0.3.1",
        "ar0.3",
        "ap0.4.0",
        "ap0.4.1",
        "ar0.4",
        "ap0.5.0",
        "ap0.5.1",
        "ar0.5",
        "n1",
        "n2"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "1_dateA-day"   -> NumberResult(1),
        "1_dateA-month" -> NumberResult(1),
        "1_dateA-year"  -> NumberResult(2001),
        "1_dateB-day"   -> NumberResult(2),
        "1_dateB-month" -> NumberResult(2),
        "1_dateB-year"  -> NumberResult(2002),
        "1_dateControl" -> OptionResult(List("showBoth")),
        "2_dateA-day"   -> NumberResult(4),
        "2_dateA-month" -> NumberResult(4),
        "2_dateA-year"  -> NumberResult(2004),
        "2_dateB-day"   -> Hidden,
        "2_dateB-month" -> Hidden,
        "2_dateB-year"  -> Hidden,
        "2_dateControl" -> OptionResult(List("showDateA")),
        "3_dateA-day"   -> Hidden,
        "3_dateA-month" -> Hidden,
        "3_dateA-year"  -> Hidden,
        "3_dateB-day"   -> NumberResult(5),
        "3_dateB-month" -> NumberResult(5),
        "3_dateB-year"  -> NumberResult(2005),
        "3_dateControl" -> OptionResult(List("showDateB")),
        "4_dateA-day"   -> Hidden,
        "4_dateA-month" -> Hidden,
        "4_dateA-year"  -> Hidden,
        "4_dateB-day"   -> Hidden,
        "4_dateB-month" -> Hidden,
        "4_dateB-year"  -> Hidden,
        "4_dateControl" -> OptionResult(List("hideBoth")),
        "5_dateA-day"   -> NumberResult(7),
        "5_dateA-month" -> NumberResult(7),
        "5_dateA-year"  -> NumberResult(2007),
        "5_dateB-day"   -> NumberResult(8),
        "5_dateB-month" -> NumberResult(8),
        "5_dateB-year"  -> NumberResult(2008),
        "5_dateControl" -> OptionResult(List("showBoth")),
        "secret"        -> OptionResult(List("reveal"))
      ),
      List(
        "1. 1 January 2001 - 2 February 2002.",
        """|<ol class="govuk-list govuk-list--number">
           | <li>1 January 2001 - 2 February 2002.</li>
           |</ol>""".stripMargin,
        "2. 4 April 2004 - .",
        """|<ol start="2" class="govuk-list govuk-list--number">
           | <li>4 April 2004 - .</li>
           |</ol>""".stripMargin,
        "3.  - 5 May 2005.",
        """|<ol start="3" class="govuk-list govuk-list--number">
           | <li>
           |  <ul class="govuk-list govuk-list--bullet">
           |   <li>5 May 2005.</li>
           |  </ul></li>
           |</ol>""".stripMargin,
        "4.  - .",
        """|<ol start="4" class="govuk-list govuk-list--number">
           | <li>
           |  <ul class="govuk-list govuk-list--bullet">
           |   <li>.</li>
           |  </ul></li>
           |</ol>""".stripMargin,
        "5. 7 July 2007 - 8 August 2008.",
        """|<ol start="5" class="govuk-list govuk-list--number">
           | <li>7 July 2007 - 8 August 2008.</li>
           |</ol>""".stripMargin,
        """|Date A: 1 January 2001, 4 April 2004, 7 July 2007
           |
           |Date B: 2 February 2002, 5 May 2005, 8 August 2008
           |
           |Period: P1Y1M1D, P1Y1M1D
           |
           |Total months: 26
           |
           |Total year: 2
           |
           |Months: 2
           |
           |Days: 2
           |
           |Sum: P2Y2M2D""".stripMargin,
        """<p>Date A: 1 January 2001, 4 April 2004, 7 July 2007</p>
          |<p>Date B: 2 February 2002, 5 May 2005, 8 August 2008</p>
          |<p>Period: P1Y1M1D, P1Y1M1D</p>
          |<p>Total months: 26</p>
          |<p>Total year: 2</p>
          |<p>Months: 2</p>
          |<p>Days: 2</p>
          |<p>Sum: P2Y2M2D</p>""".stripMargin
      ),
      "period-between-two-dates-atl-conditional.json Generated"
    ),
    (
      MongoUserData(
        "dateA-day"   -> One("1"),
        "dateA-month" -> One("1"),
        "dateA-year"  -> One("2001"),
        "dateB-day"   -> One("2"),
        "dateB-month" -> One("2"),
        "dateB-year"  -> One("2002"),
        "dummy"       -> One("dummy")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "dateA-day"   -> NumberResult(1),
        "dateA-month" -> NumberResult(1),
        "dateA-year"  -> NumberResult(2001),
        "dateB-day"   -> NumberResult(2),
        "dateB-month" -> NumberResult(2),
        "dateB-year"  -> NumberResult(2002),
        "dummy"       -> Hidden
      ),
      List.empty[String],
      "period-between-two-dates-condition.json hide test page"
    ),
    (
      MongoUserData(
        "dateA-day"   -> One("1"),
        "dateA-month" -> One("1"),
        "dateA-year"  -> One("2001"),
        "dateB-day"   -> One("2"),
        "dateB-month" -> One("2"),
        "dateB-year"  -> One("2001"),
        "dummy"       -> One("dummy")
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty,
      AnswerMap(
        "dateA-day"   -> NumberResult(1),
        "dateA-month" -> NumberResult(1),
        "dateA-year"  -> NumberResult(2001),
        "dateB-day"   -> NumberResult(2),
        "dateB-month" -> NumberResult(2),
        "dateB-year"  -> NumberResult(2001),
        "dummy"       -> StringResult("dummy")
      ),
      List(
        "Test P1M1D"
      ),
      "period-between-two-dates-condition.json show test page"
    ),
    (
      MongoUserData(
        "date-day"   -> One("1"),
        "date-month" -> One("2"),
        "date-year"  -> One("2000"),
        "dummy"      -> One("dummy"),
        "ninoA"      -> One("AA111111A"),
        "ninoB"      -> One("AA444444A"),
        "secret"     -> Many(List("reveal"))
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty.copy(thirdPartyData =
        ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "employmentsNinoA" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName") -> "Acme"
                )
              ),
            "employmentsNinoB" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName") -> "Acme"
                ),
                Map(
                  DataRetrieve.Attribute("employerName") -> "Smith Holdings"
                ),
                Map(
                  DataRetrieve.Attribute("employerName") -> "Acme 2"
                )
              )
          )
        )
      ),
      AnswerMap(
        "secret" -> OptionResult(List("reveal"))
      ),
      List(
        """|count(dataRetrieve.employmentsNinoA): 1
           |
           |count(dataRetrieve.employmentsNinoB): 3""".stripMargin,
        """|<p>count(dataRetrieve.employmentsNinoA): 1</p>
           |<p>count(dataRetrieve.employmentsNinoB): 3</p>""".stripMargin
      ),
      "data-retrieve-count.json show secret page"
    ),
    (
      MongoUserData(
        "date-day"   -> One("1"),
        "date-month" -> One("2"),
        "date-year"  -> One("2000"),
        "dummy"      -> One("dummy"),
        "ninoA"      -> One("AA111111A"),
        "ninoB"      -> One("AA444444A"),
        "secret"     -> Many(List("reveal"))
      ),
      List(
        "n0",
        "n1"
      ),
      EvaluationContext.empty.copy(thirdPartyData =
        ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "employmentsNinoA" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName") -> "Acme"
                ),
                Map(
                  DataRetrieve.Attribute("employerName") -> "Limited works"
                )
              ),
            "employmentsNinoB" ->
              List(
                Map(
                  DataRetrieve.Attribute("employerName") -> "Acme"
                ),
                Map(
                  DataRetrieve.Attribute("employerName") -> "Smith Holdings"
                ),
                Map(
                  DataRetrieve.Attribute("employerName") -> "Acme 2"
                )
              )
          )
        )
      ),
      AnswerMap(
        "secret" -> Hidden
      ),
      List(
        """|count(dataRetrieve.employmentsNinoA): 2
           |
           |count(dataRetrieve.employmentsNinoB): 3""".stripMargin,
        """|<p>count(dataRetrieve.employmentsNinoA): 2</p>
           |<p>count(dataRetrieve.employmentsNinoB): 3</p>""".stripMargin
      ),
      "data-retrieve-count.json hide secret page"
    ),
    (
      MongoUserData(
        "1_addAnother" -> Many(List("0")),
        "1_date-day"   -> One("1"),
        "1_date-month" -> One("1"),
        "1_date-year"  -> One("2001"),
        "1_dummy"      -> One("AAA"),
        "1_ninoA"      -> One("AA111111A"),
        "1_ninoB"      -> One("AA222222A"),
        "1_secret"     -> Many(List("reveal")),
        "2_addAnother" -> Many(List("0")),
        "2_date-day"   -> One("2"),
        "2_date-month" -> One("2"),
        "2_date-year"  -> One("2002"),
        "2_dummy"      -> One("BBB"),
        "2_ninoA"      -> One("AA222222A"),
        "2_ninoB"      -> One("AA333333A"),
        "3_addAnother" -> Many(List("1")),
        "3_date-day"   -> One("3"),
        "3_date-month" -> One("3"),
        "3_date-year"  -> One("2003"),
        "3_dummy"      -> One("CCC"),
        "3_ninoA"      -> One("AA101010A"),
        "3_ninoB"      -> One("AA111111A")
      ),
      List(
        "ap0.1.0",
        "ap0.1.1",
        "ap0.1.2",
        "ar0.1",
        "ap0.2.0",
        "ap0.2.1",
        "ar0.2",
        "ap0.3.0",
        "ap0.3.1",
        "ap0.3.2",
        "ar0.3"
      ),
      EvaluationContext.empty.copy(thirdPartyData =
        ThirdPartyData.empty.copy(dataRetrieve =
          mkDataRetrievesList(
            "1_employmentsNinoA" ->
              List(
                Map(DataRetrieve.Attribute("employerName") -> "Acme")
              ),
            "1_employmentsNinoB" ->
              List(
                Map(DataRetrieve.Attribute("employerName") -> "Acme"),
                Map(DataRetrieve.Attribute("employerName") -> "Smith Holdings")
              ),
            "2_employmentsNinoA" ->
              List(
                Map(DataRetrieve.Attribute("employerName") -> "Acme"),
                Map(DataRetrieve.Attribute("employerName") -> "Limited works")
              ),
            "2_employmentsNinoB" ->
              List(
                Map(DataRetrieve.Attribute("employerName") -> "Acme"),
                Map(DataRetrieve.Attribute("employerName") -> "Smith Holdings"),
                Map(DataRetrieve.Attribute("employerName") -> "Acme 2")
              ),
            "3_employmentsNinoA" ->
              List(),
            "3_employmentsNinoB" ->
              List(
                Map(DataRetrieve.Attribute("employerName") -> "Acme")
              )
          )
        )
      ),
      AnswerMap(
        "1_secret" -> OptionResult(List("reveal")),
        "2_secret" -> Hidden,
        "3_secret" -> Hidden
      ),
      List(
        """|1. count(dataRetrieve.employmentsNinoA): 1
           |
           |count(dataRetrieve.employmentsNinoB): 2""".stripMargin,
        """<ol class="govuk-list govuk-list--number">
          | <li>count(dataRetrieve.employmentsNinoA): 1</li>
          |</ol>
          |<p>count(dataRetrieve.employmentsNinoB): 2</p>""".stripMargin,
        """|1. Nino A AA111111A - 1 employments
           |
           |Nino B AA222222A - 2 employments""".stripMargin,
        """<ol class="govuk-list govuk-list--number">
          | <li>Nino A AA111111A - 1 employments</li>
          |</ol>
          |<p>Nino B AA222222A - 2 employments</p>""".stripMargin,
        """|2. count(dataRetrieve.employmentsNinoA): 2
           |
           |count(dataRetrieve.employmentsNinoB): 3""".stripMargin,
        """<ol start="2" class="govuk-list govuk-list--number">
          | <li>count(dataRetrieve.employmentsNinoA): 2</li>
          |</ol>
          |<p>count(dataRetrieve.employmentsNinoB): 3</p>""".stripMargin,
        """|2. Nino A AA222222A - 2 employments
           |
           |Nino B AA333333A - 3 employments""".stripMargin,
        """<ol start="2" class="govuk-list govuk-list--number">
          | <li>Nino A AA222222A - 2 employments</li>
          |</ol>
          |<p>Nino B AA333333A - 3 employments</p>""".stripMargin,
        """|3. count(dataRetrieve.employmentsNinoA): 0
           |
           |count(dataRetrieve.employmentsNinoB): 1""".stripMargin,
        """<ol start="3" class="govuk-list govuk-list--number">
          | <li>count(dataRetrieve.employmentsNinoA): 0</li>
          |</ol>
          |<p>count(dataRetrieve.employmentsNinoB): 1</p>""".stripMargin,
        """|3. Nino A AA101010A - 0 employments
           |
           |Nino B AA111111A - 1 employments""".stripMargin,
        """<ol start="3" class="govuk-list govuk-list--number">
          | <li>Nino A AA101010A - 0 employments</li>
          |</ol>
          |<p>Nino B AA111111A - 1 employments</p>""".stripMargin
      ),
      "data-retrieve-count-atl.json (show secret on count A = 1)"
    )
  )
  // This can't grow any more !!!
}
