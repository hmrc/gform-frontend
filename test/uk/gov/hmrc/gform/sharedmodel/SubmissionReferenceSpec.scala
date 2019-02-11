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

import play.api.libs.json._
import uk.gov.hmrc.gform._
import uk.gov.hmrc.gform.sharedmodel.SubmissionReferenceUtil._
import uk.gov.hmrc.gform.sharedmodel.SubRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import java.math.BigInteger

class SubmissionReferenceSpec extends Spec {

  "A valid EnvelopeID" should "return a 12 digit alphanumeric string separated with hyphens" in {
    val res: String = getSubmissionReference(Some(envelopeId))
    res should fullyMatch regex "[A-Z0-9]{4}-[A-Z0-9]{4}-[A-Z0-9]{4}"
  }

  "A missing EnvelopeID" should "return an empty string" in {
    val res: String = getSubmissionReference(None)
    res should be("")
  }

  "A Submission Reference" should "Have it's last digit be derived from the previous eleven" in {
    val res: Boolean = verifyCheckChar(getSubmissionReference(Some(envelopeId)))
    res should be(true)
  }

  "An incorrect Submission Reference" should "cause the check to return false" in {
    val res: Boolean = verifyCheckChar("ABCD-EFGH-IJKK")
    res should be(false)
  }

  "fgerd" should "sdfsd" in {
    val res: String = SubRef.calculate(BigInteger.valueOf(95), 36, 11, Stream.continually(List(1,3)).flatten)
    res should be("t0000000002n")
  }

  it should "0" in{
    val res: String = SubRef.calculate(BigInteger.valueOf(0), 3, 2, Stream.continually(List(1,2)).flatten)
    res should be("000")
  }

  it should "1" in{
    val res: String = SubRef.calculate(BigInteger.valueOf(1), 3, 2, Stream.continually(List(1,2)).flatten)
    res should be("201")
  }

  it should "2" in{
    val res: String = SubRef.calculate(BigInteger.valueOf(2), 3, 2, Stream.continually(List(1,2)).flatten)
    res should be("102")
  }

  it should "3" in{
    val res: String = SubRef.calculate(BigInteger.valueOf(3), 3, 2, Stream.continually(List(1,2)).flatten)
    res should be("110")
  }

  it should "4" in{
    val res: String = SubRef.calculate(BigInteger.valueOf(4), 3, 2, Stream.continually(List(1,2)).flatten)
    res should be("011")
  }

  it should "5" in{
    val res: String = SubRef.calculate(BigInteger.valueOf(5), 3, 2, Stream.continually(List(1,2)).flatten)
    res should be("212")
  }

  it should "6" in{
    val res: String = SubRef.calculate(BigInteger.valueOf(6), 3, 2, Stream.continually(List(1,2)).flatten)
    res should be("220")
  }

  it should "7" in{
    val res: String = SubRef.calculate(BigInteger.valueOf(7), 3, 2, Stream.continually(List(1,2)).flatten)
    res should be("121")
  }

  it should "8" in{
    val res: String = SubRef.calculate(BigInteger.valueOf(8), 3, 2, Stream.continually(List(1,2)).flatten)
    res should be("022")
  }




}
