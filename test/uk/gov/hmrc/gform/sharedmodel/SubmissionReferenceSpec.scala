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
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

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

}
