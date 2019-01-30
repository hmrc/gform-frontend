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
import java.security.MessageDigest
import java.math.BigInteger
import scala.math.pow

import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

object SubmissionReferenceUtil {

  def findDigits(source: Long, no: Int, digits: Array[Long]): Array[Long] =
    if (no == 0) {
      (source % 36) +: digits
    } else {
      findDigits(source, no - 1, ((source / pow(36L, no)) % 36).toLong +: digits)
    }

  def getSubmissionReference(envelopeId: EnvelopeId) = {
    val digest = MessageDigest.getInstance("SHA-256").digest(envelopeId.value.getBytes()).take(8)
    def toUnsigned(x: Byte): Long = if (x < 0) 127 - x else x
    val x
      : Long = toUnsigned(digest(0)) + 256 * (toUnsigned(digest(1)) + 256 * (toUnsigned(digest(2)) + 256 * (toUnsigned(
      digest(3)) + 256 * (toUnsigned(digest(4)) + 256 * (toUnsigned(digest(5)) + 256 * (toUnsigned(digest(6)) + 256 * (toUnsigned(
      digest(7)) % 128)))))))

    val b = findDigits(x, 10, Array())
    val fd11 = (((b(0) + b(2) + b(4) + b(6) + b(8) + b(10)) * 3) + (b(1) + b(3) + b(5) + b(7) + b(9))) % 36
    val c = b :+ fd11
    val d = c.map(i => "0123456789ABCDEFGHIGJLMNOPQRSTUVWXYZ".toCharArray()(i.toInt)).mkString
    val addHyphens = d.take(4) + "-" + d.substring(4, 8) + "-" + d.takeRight(4)
    addHyphens
  }

  def check(reference: String) = {
    val a = reference.replace("-", "")
    val b = a.toCharArray.map(i => Integer.parseInt(i.toString, 36))
    (((b(0) + b(2) + b(4) + b(6) + b(8) + b(10)) * 3) + (b(1) + b(3) + b(5) + b(7) + b(9))) % 36 == b(11)

  }
}
