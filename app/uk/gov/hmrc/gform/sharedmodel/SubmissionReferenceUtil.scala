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

  def getSubmissionReference(envelopeId: EnvelopeId) = {
    val digest = MessageDigest.getInstance("SHA-256").digest(envelopeId.value.getBytes()).take(8)
    def toUnsigned(x: Byte): Long = if (x < 0) 127 - x else x
    val x
      : Long = toUnsigned(digest(0)) + 256 * (toUnsigned(digest(1)) + 256 * (toUnsigned(digest(2)) + 256 * (toUnsigned(
      digest(3)) + 256 * (toUnsigned(digest(4)) + 256 * (toUnsigned(digest(5)) + 256 * (toUnsigned(digest(6)) + 256 * (toUnsigned(
      digest(7))                                                       % 128)))))))
    val d0 = x                                                         % 36
    val d1 = (x / 36L)                                                 % 36
    val d2 = (x / (36L * 36))                                          % 36
    val d3 = (x / (36L * 36 * 36))                                     % 36
    val d4 = (x / (36L * 36 * 36 * 36))                                % 36
    val d5 = (x / (36L * 36 * 36 * 36 * 36))                           % 36
    val d6 = (x / (36L * 36 * 36 * 36 * 36 * 36))                      % 36
    val d7 = (x / (36L * 36 * 36 * 36 * 36 * 36 * 36))                 % 36
    val d8 = (x / (36L * 36 * 36 * 36 * 36 * 36 * 36 * 36))            % 36
    val d9 = (x / (36L * 36 * 36 * 36 * 36 * 36 * 36 * 36 * 36))       % 36
    val d10 = (x / (36L * 36 * 36 * 36 * 36 * 36 * 36 * 36 * 36 * 36)) % 36

    val d11 = (((d0 + d2 + d4 + d6 + d8 + d10) * 3) + (d1 + d3 + d5 + d7 + d9)) % 36

    val c0 = "0123456789ABCDEFGHIGJLMNOPQRSTUVWXYZ".toCharArray()(d0.toInt)
    val c1 = "0123456789ABCDEFGHIGJLMNOPQRSTUVWXYZ".toCharArray()(d1.toInt)
    val c2 = "0123456789ABCDEFGHIGJLMNOPQRSTUVWXYZ".toCharArray()(d2.toInt)
    val c3 = "0123456789ABCDEFGHIGJLMNOPQRSTUVWXYZ".toCharArray()(d3.toInt)
    val c4 = "0123456789ABCDEFGHIGJLMNOPQRSTUVWXYZ".toCharArray()(d4.toInt)
    val c5 = "0123456789ABCDEFGHIGJLMNOPQRSTUVWXYZ".toCharArray()(d5.toInt)
    val c6 = "0123456789ABCDEFGHIGJLMNOPQRSTUVWXYZ".toCharArray()(d6.toInt)
    val c7 = "0123456789ABCDEFGHIGJLMNOPQRSTUVWXYZ".toCharArray()(d7.toInt)
    val c8 = "0123456789ABCDEFGHIGJLMNOPQRSTUVWXYZ".toCharArray()(d8.toInt)
    val c9 = "0123456789ABCDEFGHIGJLMNOPQRSTUVWXYZ".toCharArray()(d9.toInt)
    val c10 = "0123456789ABCDEFGHIGJLMNOPQRSTUVWXYZ".toCharArray()(d10.toInt)

    val c11 = "0123456789ABCDEFGHIGJLMNOPQRSTUVWXYZ".toCharArray()(d11.toInt)

    val getCheck = (digest.sum * -1).toByte
    val bigInt = new BigInteger(1, digest :+ getCheck).toString(36).toUpperCase
    val a = bigInt.take(4) + "-" + bigInt.substring(4, 8) + "-" + bigInt.takeRight(4)
    val b = findDigits(x, 10, Array())
    a
  }

  def findDigits(source: Long, no: Int, digits: Array[Long]): Array[Long] =
    if (no == 0) {
      digits :+ (source % 36)
    } else {
      findDigits(source, no - 1, digits :+ (source / pow(36, no)).toLong % 36)
    }

}
