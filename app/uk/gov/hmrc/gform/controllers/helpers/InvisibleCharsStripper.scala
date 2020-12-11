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

object InvisibleCharsStripper {

  /**
    * U+2000    En Quad                &#8192;      " "
    * U+2001    Em Quad                &#8193;      " "
    * U+2002    En Space               &#8194;      " "
    * U+2003    Em Space               &#8195;      " "
    * U+2004    Three-Per-Em Space     &#8196;      " "
    * U+2005    Four-Per-Em Space      &#8197;      " "
    * U+2006    Six-Per-Em Space       &#8198;      " "
    * U+2007    Figure Space           &#8199;      " "
    * U+2008    Punctuation Space      &#8200;      " "
    * U+2009    Thin Space             &#8201;      " "
    * U+200A    Hair Space             &#8202;      " "
    * U+200B    Zero-Width Space       &#8203;      "​"
    * U+200C    Zero Width Non-Joiner  &#8204;      "‌"
    * U+200D    Zero Width Joiner      &#8205;      "‍"
    * U+200E    Left-To-Right Mark     &#8206;      "‎"
    * U+200F    Right-To-Left Mark     &#8207;      "‏"
    * U+202F    Narrow No-Break Space  &#8239;      " "
    * U+FEFF    Byte order mark
    */
  private val INVISIBLE_CHARS_REGEX =
    "(\u2000|\u2001|\u2002|\u2003|\u2004|\u2005|\u2006|\u2007|\u2008|\u2009|\u200A|\u200B|\u200C|\u200D|\u200E|\u200F|\u202F|\uFEFF)"

  def stripInvisibleChars(input: String) = input.replaceAll(INVISIBLE_CHARS_REGEX, "")
}
