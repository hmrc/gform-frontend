/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel
import java.math.BigInteger
import java.security.MessageDigest

import scala.math.pow
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

object SubmissionReferenceUtil {

  def findDigits(source: Long, no: Int, digits: Array[Long]): Array[Long] =
    if (no == 0) {
      (source % 36) +: digits
    } else {
      findDigits(source, no - 1, ((source / pow(36L, no)) % 36).toLong +: digits)
    }

  def verifyCheckChar(reference: String): Boolean =
    if (reference.length >= 14) {
      val removeHyphens = reference.replace("-", "")
      val stringToInts = removeHyphens.toCharArray.map(i => Integer.parseInt(i.toString, 36)).map(_.toLong)
      calcCheckChar(stringToInts) % 36 == stringToInts(11)
    } else { false }

  def getSubmissionReference(maybeEnvelopeId: Option[EnvelopeId]): String = {
    val envelopeId = maybeEnvelopeId match {
      case Some(x) => x
      case None    => EnvelopeId("")
    }
    if (!envelopeId.value.isEmpty) {
      // As 36^11 (number of combinations of 11 base 36 digits) < 2^63 (number of combinations of 63 base 2 digits) we can get full significance from this digest.
      val digest = MessageDigest.getInstance("SHA-256").digest(envelopeId.value.getBytes()).take(8)
      val abc = new BigInteger(digest).abs()

      val digitArrayWithoutCheck2 = findDigits(abc.longValue(), 10, Array())
      val digitArray = digitArrayWithoutCheck2 :+ (calcCheckChar(digitArrayWithoutCheck2) % 36)
      val unformattedString = digitArray.map(i => Integer.toString(i.toInt, 36)).mkString.toUpperCase
      unformattedString.grouped(4).mkString("-")
    } else { "" }
  }

  def calcCheckChar(digits: Array[Long]): Long =
    (0 to 5).foldLeft(0)((sum, element) => sum + digits(element * 2).toInt) * 3 + (1 to 5).foldLeft(0)((sum, element) =>
      sum + digits(element * 2 - 1).toInt)
}
