/*
 * Copyright 2017 HM Revenue & Customs
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

import org.scalatest._
import uk.gov.hmrc.gform.gformbackend.model.{ FormTemplate, FormTypeId, Version }
import uk.gov.hmrc.gform.models.components._

class BooleanExprSpec extends FlatSpec with Matchers with EitherValues {

  val section0 = Section("Your details", None, None, List(FieldValue(FieldId("iptRegNum"), Text(AnyText, Constant(""), total = false), "Insurance Premium Tax (IPT) number", None, None, true, true, true)))

  val section1 = Section("About you", None, None, List(FieldValue(FieldId("firstName"), Text(AnyText, Constant(""), total = false), "First Name", None, None, true, true, true)))

  val section2 = Section("Business details", None, None, List(FieldValue(FieldId("nameOfBusiness"), Text(AnyText, Constant(""), total = false), "Name of business", None, None, true, true, true)))

  val formTemplate = FormTemplate(
    formTypeId = FormTypeId(""),
    formName = "IPT100",
    version = Version("1.2.3"),
    description = "abc",
    characterSet = "UTF-8",
    dmsSubmission = DmsSubmission("nino", "some-classification-type", "some-business-area"),
    submitSuccessUrl = "success-url",
    submitErrorUrl = "error-url",
    sections = List(section0, section1, section2)
  )

  val data = Map(
    FieldId("iptRegNum") -> Seq("Test!Your details!Test"),
    FieldId("firstName") -> Seq("Pete"),
    FieldId("nameOfBusiness") -> Seq("Test!Business details!Test")
  )

  "nextTrueIdxOpt(idx, list, data)" should "return the index of the next boolean expression that evaluates to true or None. Starting with idx+1" in {

    var booleanExprs = formTemplate.sections.map(_.includeIf.getOrElse(IncludeIf(IsTrue)).expr)

    BooleanExpr.nextTrueIdxOpt(0, booleanExprs, data) shouldBe Some(1)
    BooleanExpr.nextTrueIdxOpt(1, booleanExprs, data) shouldBe Some(2)
    BooleanExpr.nextTrueIdxOpt(2, booleanExprs, data) shouldBe None

    val templWithMiddleSectionIncl = formTemplate.copy(
      sections = List(section0, section1.copy(includeIf = Some(IncludeIf(Equals(FormCtx("firstName"), Constant("Pete"))))), section2)
    )

    booleanExprs = templWithMiddleSectionIncl.sections.map(_.includeIf.getOrElse(IncludeIf(IsTrue)).expr)

    BooleanExpr.nextTrueIdxOpt(0, booleanExprs, data) shouldBe Some(1)
    BooleanExpr.nextTrueIdxOpt(1, booleanExprs, data) shouldBe Some(2)
    BooleanExpr.nextTrueIdxOpt(2, booleanExprs, data) shouldBe None

    val templWithMiddleSectionExcl = formTemplate.copy(
      sections = List(section0, section1.copy(includeIf = Some(IncludeIf(Equals(FormCtx("firstName"), Constant("Pet"))))), section2)
    )

    booleanExprs = templWithMiddleSectionExcl.sections.map(_.includeIf.getOrElse(IncludeIf(IsTrue)).expr)

    BooleanExpr.nextTrueIdxOpt(0, booleanExprs, data) shouldBe Some(2)
    BooleanExpr.nextTrueIdxOpt(1, booleanExprs, data) shouldBe Some(2)
    BooleanExpr.nextTrueIdxOpt(2, booleanExprs, data) shouldBe None
  }

}
