/*
 * Copyright 2018 HM Revenue & Customs
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

import org.scalatest.mockito.MockitoSugar.mock
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.form.FormField

class BooleanExprSpec extends Spec {
  val retrievals: Retrievals = mock[Retrievals]

  "0 equals 0" should "return true" in new TestZero {
    BooleanExpr.isTrue(Equals(FormCtx("number"), Constant("0")), rawDataFromBrowser, authContext) shouldBe true
  }

  "0 equals 1" should "return false" in new TestZero {
    BooleanExpr.isTrue(Equals(FormCtx("number"), Constant("1")), rawDataFromBrowser, authContext) shouldBe false
  }

  "00 equals 0" should "return true" in new Test {
    override val value = "00"
    BooleanExpr.isTrue(Equals(FormCtx("number"), Constant("0")), rawDataFromBrowser, authContext) shouldBe true
  }

  "0 equals 00" should "return true" in new TestZero {
    BooleanExpr.isTrue(Equals(FormCtx("number"), Constant("00")), rawDataFromBrowser, authContext) shouldBe true
  }

  "1,000 equals 1000" should "return true" in new Test {
    override val value = "1,000"
    BooleanExpr.isTrue(Equals(FormCtx("number"), Constant("1000")), rawDataFromBrowser, authContext) shouldBe true
  }

  "Fred equals Fred" should "return true" in new TestFred {
    BooleanExpr.isTrue(Equals(FormCtx("firstName"), Constant("Fred")), rawDataFromBrowser, authContext) shouldBe true
  }

  "Fred equals Dave" should "return false" in new TestFred {
    BooleanExpr.isTrue(Equals(FormCtx("firstName"), Constant("Dave")), rawDataFromBrowser, authContext) shouldBe false
  }

  "0 equals Fred" should "return false" in new TestZero {
    BooleanExpr.isTrue(Equals(FormCtx("number"), Constant("Fred")), rawDataFromBrowser, authContext) shouldBe false
  }

  "Fred equals 0" should "return false" in new TestFred {
    BooleanExpr.isTrue(Equals(FormCtx("firstName"), Constant("0")), rawDataFromBrowser, authContext) shouldBe false
  }

  "1 greater than 0" should "return true" in new Test {
    override val value = "1"
    BooleanExpr.isTrue(GreaterThan(FormCtx("number"), Constant("0")), rawDataFromBrowser, authContext) shouldBe true
  }

  "0 greater than -1" should "return true" in new TestZero {
    BooleanExpr.isTrue(GreaterThan(FormCtx("number"), Constant("-1")), rawDataFromBrowser, authContext) shouldBe true
  }

  "0 greater than 1" should "return false" in new TestZero {
    BooleanExpr.isTrue(GreaterThan(FormCtx("number"), Constant("1")), rawDataFromBrowser, authContext) shouldBe false
  }

  "0 greater than 0" should "return false" in new TestZero {
    BooleanExpr.isTrue(GreaterThan(FormCtx("number"), Constant("0")), rawDataFromBrowser, authContext) shouldBe false
  }

  "Fred greater than Dave" should "return true" in new TestFred {
    BooleanExpr
      .isTrue(GreaterThan(FormCtx("firstName"), Constant("Dave")), rawDataFromBrowser, authContext) shouldBe true
  }

  "Dave greater than Fred" should "return false" in new Test {
    override val value = "Dave"
    BooleanExpr
      .isTrue(GreaterThan(FormCtx("firstName"), Constant("Fred")), rawDataFromBrowser, authContext) shouldBe false
  }

  "Fred greater than Fred" should "return false" in new TestFred {
    BooleanExpr
      .isTrue(GreaterThan(FormCtx("firstName"), Constant("Fred")), rawDataFromBrowser, authContext) shouldBe false
  }

  "1 greater than or equal to 0" should "return true" in new Test {
    override val value = "1"
    BooleanExpr
      .isTrue(GreaterThanOrEquals(FormCtx("number"), Constant("0")), rawDataFromBrowser, authContext) shouldBe true
  }

  "0 greater than or equal to -1" should "return true" in new TestZero {
    BooleanExpr
      .isTrue(GreaterThanOrEquals(FormCtx("number"), Constant("-1")), rawDataFromBrowser, authContext) shouldBe true
  }

  "0 greater than or equal to 1" should "return false" in new TestZero {
    BooleanExpr
      .isTrue(GreaterThanOrEquals(FormCtx("number"), Constant("1")), rawDataFromBrowser, authContext) shouldBe false
  }

  "0 greater than or equal to 0" should "return true" in new TestZero {
    BooleanExpr
      .isTrue(GreaterThanOrEquals(FormCtx("number"), Constant("0")), rawDataFromBrowser, authContext) shouldBe true
  }

  "Fred greater than or equal to Dave" should "return true" in new TestFred {
    BooleanExpr
      .isTrue(GreaterThanOrEquals(FormCtx("firstName"), Constant("Dave")), rawDataFromBrowser, authContext) shouldBe true
  }

  "Dave greater than or equal to Fred" should "return false" in new Test {
    override val value = "Dave"
    BooleanExpr
      .isTrue(GreaterThanOrEquals(FormCtx("firstName"), Constant("Fred")), rawDataFromBrowser, authContext) shouldBe false
  }

  "Fred greater than or equal to Fred" should "return true" in new TestFred {
    BooleanExpr
      .isTrue(GreaterThanOrEquals(FormCtx("firstName"), Constant("Fred")), rawDataFromBrowser, authContext) shouldBe true
  }

  "1 less than 0" should "return false" in new Test {
    override val value = "1"
    BooleanExpr.isTrue(LessThan(FormCtx("number"), Constant("0")), rawDataFromBrowser, authContext) shouldBe false
  }

  "0 less than -1" should "return false" in new TestZero {
    BooleanExpr.isTrue(LessThan(FormCtx("number"), Constant("-1")), rawDataFromBrowser, authContext) shouldBe false
  }

  "0 less than 1" should "return true" in new TestZero {
    BooleanExpr.isTrue(LessThan(FormCtx("number"), Constant("1")), rawDataFromBrowser, authContext) shouldBe true
  }

  "0 less than 0" should "return false" in new TestZero {
    BooleanExpr.isTrue(LessThan(FormCtx("number"), Constant("0")), rawDataFromBrowser, authContext) shouldBe false
  }

  "Fred less than Dave" should "return false" in new Test {
    override val value = "Fred"
    BooleanExpr.isTrue(LessThan(FormCtx("firstName"), Constant("Dave")), rawDataFromBrowser, authContext) shouldBe false
  }

  "Dave less than Fred" should "return true" in new Test {
    override val value = "Dave"
    BooleanExpr.isTrue(LessThan(FormCtx("firstName"), Constant("Fred")), rawDataFromBrowser, authContext) shouldBe true
  }

  "Fred less than Fred" should "return false" in new Test {
    override val value = "Fred"
    BooleanExpr.isTrue(LessThan(FormCtx("firstName"), Constant("Fred")), rawDataFromBrowser, authContext) shouldBe false
  }

  "1 less than or equal to 0" should "return false" in new Test {
    override val value = "1"
    BooleanExpr
      .isTrue(LessThanOrEquals(FormCtx("number"), Constant("0")), rawDataFromBrowser, authContext) shouldBe false
  }

  "0 less than or equal to -1" should "return false" in new TestZero {
    BooleanExpr
      .isTrue(LessThanOrEquals(FormCtx("number"), Constant("-1")), rawDataFromBrowser, authContext) shouldBe false
  }

  "0 less than or equal to 1" should "return true" in new TestZero {
    BooleanExpr
      .isTrue(LessThanOrEquals(FormCtx("number"), Constant("1")), rawDataFromBrowser, authContext) shouldBe true
  }

  "0 less than or equal to 0" should "return true" in new TestZero {
    BooleanExpr
      .isTrue(LessThanOrEquals(FormCtx("number"), Constant("0")), rawDataFromBrowser, authContext) shouldBe true
  }

  "Fred less than or equal to Dave" should "return false" in new Test {
    override val value = "Fred"
    BooleanExpr
      .isTrue(LessThanOrEquals(FormCtx("firstName"), Constant("Dave")), rawDataFromBrowser, authContext) shouldBe false
  }

  "Dave less than or equal to Fred" should "return true" in new Test {
    override val value = "Dave"
    BooleanExpr
      .isTrue(LessThanOrEquals(FormCtx("firstName"), Constant("Fred")), rawDataFromBrowser, authContext) shouldBe true
  }

  "Fred less than or equal to Fred" should "return true" in new Test {
    override val value = "Fred"
    BooleanExpr
      .isTrue(LessThanOrEquals(FormCtx("firstName"), Constant("Fred")), rawDataFromBrowser, authContext) shouldBe true
  }

  trait Test extends ExampleData {
    def value: String

    override def `formField - number` = FormField(`fieldId - number`, value)
    override def `formField - firstName` = FormField(`fieldId - firstName`, value)

    def `fieldId - zero` = FormComponentId("number")

    def `fieldValue - zero` = FormComponent(
      `fieldId - zero`,
      Text(Number(), Constant("0")),
      "sample label",
      None,
      None,
      None,
      true,
      false,
      false,
      false,
      false,
      None
    )
  }

  trait TestZero extends Test {
    def value = "0"
  }

  trait TestFred extends Test {
    def value = "Fred"
  }
}
