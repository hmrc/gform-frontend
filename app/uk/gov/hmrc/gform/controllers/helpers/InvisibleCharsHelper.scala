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

object InvisibleCharsHelper {

  /**
    * All invisible characters from the U+2000 block
    *
    */
  val INVISIBLE_CHARS_TO_DESC: Map[String, String] = Seq(
    ("\u2000", "En Quad"),
    ("\u2001", "Em Quad"),
    ("\u2002", "En Space"),
    ("\u2003", "Em Space"),
    ("\u2004", "Three-Per-Em Space"),
    ("\u2005", "Four-Per-Em Space"),
    ("\u2006", "Six-Per-Em Space"),
    ("\u2007", "Figure Space"),
    ("\u2008", "Punctuation Space"),
    ("\u2009", "Thin Space"),
    ("\u200A", "Hair Space"),
    ("\u200B", "Zero-Width Space"),
    ("\u200C", "Zero Width Non-Joiner"),
    ("\u200D", "Zero Width Joiner"),
    ("\u200E", "Left-To-Right Mark"),
    ("\u200F", "Right-To-Left Mark"),
    ("\u202A", "Left-to-Right Embedding"),
    ("\u202B", "Right-to-Left Embedding"),
    ("\u202C", "Pop Directional Formatting (PDF)"),
    ("\u202D", "Left-to-Right Override (LRO)"),
    ("\u202E", "Right-to-Left Override (RLO)"),
    ("\u202F", "Narrow No-Break Space"),
    ("\u2061", "Function Application"),
    ("\u2062", "Invisible Times"),
    ("\u2063", "Invisible Separator"),
    ("\u2064", "Invisible Plus"),
    ("\u2066", "Left-to-Right Isolate (LRI)"),
    ("\u2067", "Right-to-Left Isolate (RLI)"),
    ("\u2068", "First Strong Isolate (FSI)"),
    ("\u2069", "Pop Directional Isolate (PDI)"),
    ("\u206A", "Inhibit Symmetric Swapping"),
    ("\u206B", "Activate Symmetric Swapping"),
    ("\u206C", "Inhibit Arabic Form Shaping"),
    ("\u206D", "Activate Arabic Form Shaping"),
    ("\u206E", "National Digit Shapes"),
    ("\u206F", "Nominal Digit Shapes"),
    ("\uFEFF", "Byte order mark"),
  ).toMap

  private val INVISIBLE_CHARS_REGEX = s"(${INVISIBLE_CHARS_TO_DESC.keys.mkString("|")})".r

  def replaceInvisibleChars(input: String): String = INVISIBLE_CHARS_REGEX.replaceAllIn(input, " ")

  def findInvisibleCharMatches(input: String): Map[String, Int] =
    INVISIBLE_CHARS_REGEX.findAllIn(input).toList.groupBy(identity).mapValues(_.size)

  def getUnicode(char: String) = s"U+${char.charAt(0).toInt.toHexString.toUpperCase}"

  def getDesc(char: String): String = INVISIBLE_CHARS_TO_DESC.getOrElse(char, "")
}
