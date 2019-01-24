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
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

object SubmissionReferenceUtil {

  def getSubmissionReference(envelopeId: EnvelopeId) = {
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(envelopeId.value.getBytes)
    val bigInt = new BigInteger(1, digest)
    val hashedString = bigInt.toString(16)
    val correctLength = hashedString.take(12).toUpperCase
    correctLength.take(4) + "-" + correctLength.substring(4, 8) + "-" + correctLength.takeRight(4)
  }
}
