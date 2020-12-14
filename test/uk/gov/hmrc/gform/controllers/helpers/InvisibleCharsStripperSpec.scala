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

package uk.gov.hmrc.gform.controllers.helpers

import org.scalatest.{ FlatSpecLike, Matchers }

class InvisibleCharsStripperSpec extends FlatSpecLike with Matchers {

  "stripInvisibleChars" should "remove all invisible characters" in {
    val input =
      "123\u061C\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u200B\u200C\u200D\u200E\u200F\u202A\u202B\u202C\u202D\u202E\u202F\u2066\u2067\u2068\u2069\uFEFF4"
    val result = InvisibleCharsStripper.stripInvisibleChars(input)
    result shouldBe "1234"
  }

  it should "return the input as it is when there are no invisible chars" in {
    val input = "1234"
    val result = InvisibleCharsStripper.stripInvisibleChars(input)
    result shouldBe "1234"
  }
}
