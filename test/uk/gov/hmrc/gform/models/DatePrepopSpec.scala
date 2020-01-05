/*
 * Copyright 2020 HM Revenue & Customs
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

import org.scalatest.{ EitherValues, FlatSpec, Matchers, OptionValues }
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Offset

class DatePrepopSpec extends FlatSpec with Matchers with EitherValues with OptionValues {

  "if offset is 0, date" should "be prepopulated with value" in {
    val value = Some(DateExpr(2017, 7, 4))
    val offset = Offset(0)

    val result = value.map(DateHelperFunctions.withOffset(offset, _))
    result shouldBe value
  }

  "if offset is -5, date value" should "be before 5 days" in {
    val value = Some(DateExpr(2017, 7, 4))
    val offset = Offset(-5)

    val result = value.map(DateHelperFunctions.withOffset(offset, _))
    result.get shouldBe DateExpr(2017, 6, 29)
  }

  "if offset is 40, date value" should "be after 40 days" in {
    val value = Some(DateExpr(2017, 7, 4))
    val offset = Offset(40)

    val result = value.map(DateHelperFunctions.withOffset(offset, _))
    result.get shouldBe DateExpr(2017, 8, 13)
  }

}
