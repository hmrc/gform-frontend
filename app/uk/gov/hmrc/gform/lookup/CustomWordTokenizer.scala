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

package uk.gov.hmrc.gform.lookup

import java.text.BreakIterator
import java.util._

import scala.collection.JavaConverters._

import com.miguelfonseca.completely.common.Precondition.checkPointer
import com.miguelfonseca.completely.text.analyze.Analyzer

class CustomWordTokenizer extends Analyzer {

  private val boundary = BreakIterator.getWordInstance

  override def apply(input: Collection[String]): Collection[String] = {
    checkPointer(input != null)

    val result = new LinkedList[String]

    for (text <- input.asScala) {
      checkPointer(text != null)

      boundary.setText(text.toString)

      var start = boundary.first
      var end = boundary.next

      while ({
        end != BreakIterator.DONE
      }) {
        val word = text.substring(start, end)
        if (Character.isLetterOrDigit(word.charAt(0)) || Character.getType(word.charAt(0)) == Character.CURRENCY_SYMBOL)
          result.add(word)

        start = end
        end = boundary.next
      }
    }
    result
  }

}
