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

object InvisibleCharsHelper {

  case class UnicodeDescMapping(code: Char, desc: String, mapping: String)

  /** All invisible characters from the U+2000 block
    */
  private val INVISIBLE_UNICODE_DESC_MAPPING: Seq[UnicodeDescMapping] = Seq(
    UnicodeDescMapping('\u2000', "En Quad", " "),
    UnicodeDescMapping('\u2001', "Em Quad", " "),
    UnicodeDescMapping('\u2002', "En Space", " "),
    UnicodeDescMapping('\u2003', "Em Space", " "),
    UnicodeDescMapping('\u2004', "Three-Per-Em Space", " "),
    UnicodeDescMapping('\u2005', "Four-Per-Em Space", " "),
    UnicodeDescMapping('\u2006', "Six-Per-Em Space", " "),
    UnicodeDescMapping('\u2007', "Figure Space", " "),
    UnicodeDescMapping('\u2008', "Punctuation Space", " "),
    UnicodeDescMapping('\u2009', "Thin Space", " "),
    UnicodeDescMapping('\u200A', "Hair Space", " "),
    UnicodeDescMapping('\u200B', "Zero-Width Space", ""),
    UnicodeDescMapping('\u200C', "Zero Width Non-Joiner", ""),
    UnicodeDescMapping('\u200D', "Zero Width Joiner", ""),
    UnicodeDescMapping('\u200E', "Left-To-Right Mark", ""),
    UnicodeDescMapping('\u200F', "Right-To-Left Mark", ""),
    UnicodeDescMapping('\u202A', "Left-to-Right Embedding", ""),
    UnicodeDescMapping('\u202B', "Right-to-Left Embedding", ""),
    UnicodeDescMapping('\u202C', "Pop Directional Formatting (PDF)", ""),
    UnicodeDescMapping('\u202D', "Left-to-Right Override (LRO)", ""),
    UnicodeDescMapping('\u202E', "Right-to-Left Override (RLO)", ""),
    UnicodeDescMapping('\u202F', "Narrow No-Break Space", " "),
    UnicodeDescMapping('\u2061', "Function Application", ""),
    UnicodeDescMapping('\u2062', "Invisible Times", ""),
    UnicodeDescMapping('\u2063', "Invisible Separator", ""),
    UnicodeDescMapping('\u2064', "Invisible Plus", ""),
    UnicodeDescMapping('\u2066', "Left-to-Right Isolate (LRI)", ""),
    UnicodeDescMapping('\u2067', "Right-to-Left Isolate (RLI)", ""),
    UnicodeDescMapping('\u2068', "First Strong Isolate (FSI)", ""),
    UnicodeDescMapping('\u2069', "Pop Directional Isolate (PDI)", ""),
    UnicodeDescMapping('\u206A', "Inhibit Symmetric Swapping", ""),
    UnicodeDescMapping('\u206B', "Activate Symmetric Swapping", ""),
    UnicodeDescMapping('\u206C', "Inhibit Arabic Form Shaping", ""),
    UnicodeDescMapping('\u206D', "Activate Arabic Form Shaping", ""),
    UnicodeDescMapping('\u206E', "National Digit Shapes", ""),
    UnicodeDescMapping('\u206F', "Nominal Digit Shapes", ""),
    UnicodeDescMapping('\uFEFF', "Byte order mark", "")
  )

  private val INVISIBLE_CHAR_MAP: Map[Char, UnicodeDescMapping] =
    INVISIBLE_UNICODE_DESC_MAPPING.groupBy(_.code).mapValues(_.head)

  def replaceInvisibleChars(input: String): String =
    input.toCharArray.map(c => INVISIBLE_CHAR_MAP.get(c).map(_.mapping).getOrElse(c)).mkString("")

  def invisibleCharMatches(input: String): Map[Char, Int] =
    input.toCharArray.filter(INVISIBLE_CHAR_MAP.contains).groupBy(identity).mapValues(_.length)

  def getUnicode(char: Char) = s"U+${char.toInt.toHexString.toUpperCase}"

  def getDesc(char: Char): String = INVISIBLE_CHAR_MAP.get(char).map(_.desc).getOrElse("")
}
