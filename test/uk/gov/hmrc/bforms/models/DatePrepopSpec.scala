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

package uk.gov.hmrc.bforms.models

import org.scalatest.{EitherValues, FlatSpec, Matchers, OptionValues}
import uk.gov.hmrc.bforms.core.utils.DateHelperFunctions
import uk.gov.hmrc.bforms.core.{DateExpr, Expr, Offset}

/**
  * Created by dimitra on 07/04/17.
  */
class DatePrepopSpec extends FlatSpec with Matchers with EitherValues with OptionValues {

  val extractDefaultDate: Option[Expr] => Option[DateExpr] = expr => expr.collect { case x: DateExpr => x }

  "if offset is 0, date" should "be prepopulated with value" in {
    val value = Some(DateExpr("04", "07", "2017"))
    val dateExpression = extractDefaultDate(value)

    val result = DateHelperFunctions.adjustDate(Offset(0), dateExpression)

    result shouldBe value
  }

  "if offset is -5, date value" should "be before 5 days" in {
    val value = Some(DateExpr("04", "07", "2017"))
    val dateExpression = extractDefaultDate(value)

    val result = DateHelperFunctions.adjustDate(Offset(-5), dateExpression)

    result.get shouldBe DateExpr("29", "06", "2017")
  }

  "if offset is 40, date value" should "be after 40 days" in {
    val value = Some(DateExpr("04", "07", "2017"))
    val dateExpression = extractDefaultDate(value)

    val result = DateHelperFunctions.adjustDate(Offset(40), dateExpression)

    result.get shouldBe DateExpr("13", "08", "2017")
  }

  "if offset date are non defined, no DateExpression" should "be returned" in {
    val result = DateHelperFunctions.adjustDate(Offset(0), None)

    result shouldBe None
  }

}