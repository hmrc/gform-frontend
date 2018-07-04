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
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.form.FormField

class BooleanExprSpec extends Spec {
  val retrievals: MaterialisedRetrievals = mock[MaterialisedRetrievals]

  val equalsCombinations = Table(
    ("Ctx value", "left argument", "right argument", "expected result"),
    ("0", "number", "0", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("0", "number", "1", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("00", "number", "0", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("0", "number", "00", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("1,000", "number", "1000", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("Fred", "firstName", "Fred", BooleanExprResultWithDependents(true, List(FormComponentId("firstName")))),
    ("Fred", "firstName", "Dave", BooleanExprResultWithDependents(false, List(FormComponentId("firstName")))),
    ("0", "number", "Fred", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("Fred", "firstName", "0", BooleanExprResultWithDependents(false, List(FormComponentId("firstName"))))
  )

  forAll(equalsCombinations) { (ctxValue, left, right, result) =>
    new Test {
      override val value = ctxValue
      BooleanExpr
        .isTrue(Equals(FormCtx(left), Constant(right)), rawDataFromBrowser, authContext.affinityGroup) shouldBe result
    }
  }

  val notEqualsCombinations = Table(
    ("Ctx value", "left argument", "right argument", "expected result"),
    ("0", "number", "0", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("0", "number", "1", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("00", "number", "0", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("0", "number", "00", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("1,000", "number", "1000", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("Fred", "firstName", "Fred", BooleanExprResultWithDependents(false, List(FormComponentId("firstName")))),
    ("Fred", "firstName", "Dave", BooleanExprResultWithDependents(true, List(FormComponentId("firstName")))),
    ("0", "number", "Fred", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("Fred", "firstName", "0", BooleanExprResultWithDependents(true, List(FormComponentId("firstName"))))
  )

  forAll(notEqualsCombinations) { (ctxValue, left, right, result) =>
    new Test {
      override val value = ctxValue
      BooleanExpr
        .isTrue(NotEquals(FormCtx(left), Constant(right)), rawDataFromBrowser, authContext.affinityGroup) shouldBe result
    }
  }

  val greaterThanCombinations = Table(
    ("Ctx value", "left argument", "right argument", "expected result"),
    ("1", "number", "0", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("0", "number", "-1", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("0", "number", "1", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("0", "number", "0", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("Fred", "firstName", "Dave", BooleanExprResultWithDependents(true, List(FormComponentId("firstName")))),
    ("Dave", "firstName", "Fred", BooleanExprResultWithDependents(false, List(FormComponentId("firstName")))),
    ("Fred", "firstName", "Fred", BooleanExprResultWithDependents(false, List(FormComponentId("firstName"))))
  )

  forAll(greaterThanCombinations) { (ctxValue, left, right, result) =>
    new Test {
      override val value = ctxValue
      BooleanExpr
        .isTrue(GreaterThan(FormCtx(left), Constant(right)), rawDataFromBrowser, authContext.affinityGroup) shouldBe result
    }
  }

  val greaterThanOrEqualsCombinations = Table(
    ("Ctx value", "left argument", "right argument", "expected result"),
    ("1", "number", "0", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("0", "number", "-1", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("0", "number", "1", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("0", "number", "0", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("Fred", "firstName", "Dave", BooleanExprResultWithDependents(true, List(FormComponentId("firstName")))),
    ("Dave", "firstName", "Fred", BooleanExprResultWithDependents(false, List(FormComponentId("firstName")))),
    ("Fred", "firstName", "Fred", BooleanExprResultWithDependents(true, List(FormComponentId("firstName"))))
  )

  forAll(greaterThanOrEqualsCombinations) { (ctxValue, left, right, result) =>
    new Test {
      override val value = ctxValue
      BooleanExpr
        .isTrue(GreaterThanOrEquals(FormCtx(left), Constant(right)), rawDataFromBrowser, authContext.affinityGroup) shouldBe result
    }
  }

  val lessThanCombinations = Table(
    ("Ctx value", "left argument", "right argument", "expected result"),
    ("1", "number", "0", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("0", "number", "-1", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("0", "number", "1", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("0", "number", "0", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("Fred", "firstName", "Dave", BooleanExprResultWithDependents(false, List(FormComponentId("firstName")))),
    ("Dave", "firstName", "Fred", BooleanExprResultWithDependents(true, List(FormComponentId("firstName")))),
    ("Fred", "firstName", "Fred", BooleanExprResultWithDependents(false, List(FormComponentId("firstName"))))
  )

  forAll(lessThanCombinations) { (ctxValue, left, right, result) =>
    new Test {
      override val value = ctxValue
      BooleanExpr
        .isTrue(LessThan(FormCtx(left), Constant(right)), rawDataFromBrowser, authContext.affinityGroup) shouldBe result
    }
  }

  val lessThanOrEqualsCombinations = Table(
    ("Ctx value", "left argument", "right argument", "expected result"),
    ("1", "number", "0", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("0", "number", "-1", BooleanExprResultWithDependents(false, List(FormComponentId("number")))),
    ("0", "number", "1", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("0", "number", "0", BooleanExprResultWithDependents(true, List(FormComponentId("number")))),
    ("Fred", "firstName", "Dave", BooleanExprResultWithDependents(false, List(FormComponentId("firstName")))),
    ("Dave", "firstName", "Fred", BooleanExprResultWithDependents(true, List(FormComponentId("firstName")))),
    ("Fred", "firstName", "Fred", BooleanExprResultWithDependents(true, List(FormComponentId("firstName"))))
  )

  forAll(lessThanOrEqualsCombinations) { (ctxValue, left, right, result) =>
    new Test {
      override val value = ctxValue
      BooleanExpr
        .isTrue(LessThanOrEquals(FormCtx(left), Constant(right)), rawDataFromBrowser, authContext.affinityGroup) shouldBe result
    }
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
}
