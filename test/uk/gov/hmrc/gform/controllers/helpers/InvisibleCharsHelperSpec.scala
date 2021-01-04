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

package uk.gov.hmrc.gform.controllers.helpers

import org.scalatest.{ FlatSpecLike, Matchers }

class InvisibleCharsHelperSpec extends FlatSpecLike with Matchers {

  "replaceInvisibleChars" should "replace invisible characters with space" in {
    val input =
      "123\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u200B\u200C\u200D\u200E\u200F\u202A\u202B\u202C\u202D\u202E\u202F\u2061\u2062\u2063\u2064\u2066\u2067\u2068\u2069\u206A\u206B\u206C\u206D\u206E\u206F\uFEFF4"
    val result = InvisibleCharsHelper.replaceInvisibleChars(input)
    result shouldBe "123            4"
  }

  it should "return the input as it is when there are no invisible chars" in {
    val input = "1234"
    val result = InvisibleCharsHelper.replaceInvisibleChars(input)
    result shouldBe "1234"
  }

  "findInvisibleCharMatches" should "return all matches for invisble chars, with counts" in {
    val input = "123\u2000\u2000\u20014"
    val result = InvisibleCharsHelper.invisibleCharMatches(input)
    result shouldBe Map('\u2000' -> 2, '\u2001' -> 1)
  }

  "getUnicode" should "return unicode representation for the given char" in {
    val input = '\u2000'
    InvisibleCharsHelper.getUnicode(input) shouldBe "U+2000"
  }

  "getDesc" should "return description for the given character" in {
    val input = '\u2000'
    InvisibleCharsHelper.getDesc(input) shouldBe "En Quad"
  }

  it should "return empty string when given character does not exist in invisible chars map" in {
    val input = 'A'
    InvisibleCharsHelper.getDesc(input) shouldBe ""
  }
}
