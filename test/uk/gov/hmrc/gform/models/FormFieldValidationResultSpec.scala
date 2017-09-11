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

import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation._

class FormFieldValidationResultSpec extends FlatSpec with Matchers {

  val mockDateFieldValue = FieldValue(
    id = FieldId("StartDate"),
    `type` = Date(AnyDate, Offset(0), Some(ExactDateValue(2010, 10, 10))),
    label = "label",
    helpText = None,
    shortName = None,
    mandatory = false,
    editable = true,
    submissible = true,
    derived = true,
    errorMessage = None
  )

  "getOptionalCurrentValue" should "return current value when validation returns FieldError" in {
    val testFieldComponent = ComponentField(
      fieldValue = mockDateFieldValue,
      data = Map(
        "day" -> FieldOk(FieldValue(FieldId("day"), Text(AnyText, Constant("")), "label", None, None, false, true, true, true, None), "1"),
        "month" -> FieldOk(FieldValue(FieldId("month"), Text(AnyText, Constant("")), "label", None, None, false, true, true, true, None), "1"),
        "year" -> FieldError(FieldValue(FieldId("year"), Text(AnyText, Constant("")), "label", None, None, false, true, true, true, None), "NOT_RIGHT", Set("NAN"))
      )
    )

    testFieldComponent.getOptionalCurrentValue("year").get.equalsIgnoreCase("NOT_RIGHT") shouldBe true

  }

  it should "return current value when validation returns other than FieldError" in {
    val testFieldComponent = ComponentField(
      fieldValue = mockDateFieldValue,
      data = Map(
        "day" -> FieldOk(FieldValue(FieldId("day"), Text(AnyText, Constant("")), "day", None, None, false, true, true, true, None), "1"),
        "month" -> FieldOk(FieldValue(FieldId("month"), Text(AnyText, Constant("")), "month", None, None, false, true, true, true, None), "1"),
        "year" -> FieldOk(FieldValue(FieldId("year"), Text(AnyText, Constant("")), "year", None, None, false, true, true, true, None), "2017")
      )
    )

    testFieldComponent.getOptionalCurrentValue("year").get.equalsIgnoreCase("2017") shouldBe true
  }

}
