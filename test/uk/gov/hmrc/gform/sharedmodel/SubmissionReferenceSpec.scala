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
import java.math.BigInteger

import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.mkFormTemplate
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

class SubmissionReferenceSpec extends Spec {

  "A valid EnvelopeID" should "return a 12 digit alphanumeric string separated with hyphens" in {
    val res: String = getSubmissionReference(envelopeId)
    res should fullyMatch regex "[A-Z0-9]{4}-[A-Z0-9]{4}-[A-Z0-9]{4}"
  }

  "A missing EnvelopeID" should "return an empty string" in {
    val res: String = getSubmissionReference(EnvelopeId(""))
    res should be("")
  }

  "A Submission Reference" should "Have it's last digit be derived from the previous eleven" in {
    val res: Boolean = verifyCheckChar(getSubmissionReference(envelopeId))
    res should be(true)
  }

  "An incorrect Submission Reference" should "cause the check to return false" in {
    val res: Boolean = verifyCheckChar("ABCD-EFGH-IJKK")
    res should be(false)
  }

  "A given value, radix, digits and comb" should "calculate the correct submission reference" in {
    val calculateValues = Table(
      ("bigInteger", "radix", "digits", "stream", "output"),
      (BigInteger.valueOf(95), 36, 11, Stream.continually(List(1, 3)).flatten, "t0000000002n"),
      (BigInteger.valueOf(0), 3, 2, Stream.continually(List(1, 2)).flatten, "000"),
      (BigInteger.valueOf(1), 3, 2, Stream.continually(List(1, 2)).flatten, "201"),
      (BigInteger.valueOf(2), 3, 2, Stream.continually(List(1, 2)).flatten, "102"),
      (BigInteger.valueOf(3), 3, 2, Stream.continually(List(1, 2)).flatten, "110"),
      (BigInteger.valueOf(4), 3, 2, Stream.continually(List(1, 2)).flatten, "011"),
      (BigInteger.valueOf(5), 3, 2, Stream.continually(List(1, 2)).flatten, "212"),
      (BigInteger.valueOf(6), 3, 2, Stream.continually(List(1, 2)).flatten, "220"),
      (BigInteger.valueOf(7), 3, 2, Stream.continually(List(1, 2)).flatten, "121"),
      (BigInteger.valueOf(8), 3, 2, Stream.continually(List(1, 2)).flatten, "022")
    )

    forAll(calculateValues) { (bigInteger, radix, digits, stream, expectedOutput) ⇒
      val reference = calculate(bigInteger, radix, digits, stream)
      reference shouldBe expectedOutput
    }
  }
}
