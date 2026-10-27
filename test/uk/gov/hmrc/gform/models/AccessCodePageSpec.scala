/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.gform.gform.AccessCodeForm
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BySubmissionReference, ContinueOrDeletePage, DraftRetrievalMethod, FormAccessCode, FormAccessCodeForAgents }

class AccessCodePageSpec extends AnyFlatSpecLike with Matchers {

  private val retrievalMethods: List[(String, DraftRetrievalMethod, String)] = List(
    ("agent access code", FormAccessCodeForAgents(ContinueOrDeletePage.Show), "A12-B456-8ZZ"),
    ("access code", FormAccessCode(ContinueOrDeletePage.Show), "A12-B456-8ZZ"),
    ("submission reference", BySubmissionReference, "AB12-B456-98ZZ")
  )

  retrievalMethods.foreach { case (name, method, validCode) =>
    List("true", "false").foreach { isContinue =>
      s"$name with isContinue=$isContinue" should "report only the required error for blank input" in {
        List("", "   ", "\t").foreach { code =>
          val result = AccessCodePage
            .form(method)
            .bind(
              Map(AccessCodePage.key -> code, AccessCodePage.isContinueKey -> isContinue)
            )

          result.errors.map(error => error.key -> error.message) shouldBe Seq(
            AccessCodePage.key -> "error.required"
          )
        }
      }

      it should "report only the required error for missing input" in {
        val result = AccessCodePage.form(method).bind(Map(AccessCodePage.isContinueKey -> isContinue))

        result.errors.map(error => error.key -> error.message) shouldBe Seq(
          AccessCodePage.key -> "error.required"
        )
      }

      it should "report only the pattern error for invalid non-blank input" in {
        List("invalid", validCode.toLowerCase, validCode.replace("-", ""), s" $validCode ").foreach { code =>
          val result = AccessCodePage
            .form(method)
            .bind(
              Map(AccessCodePage.key -> code, AccessCodePage.isContinueKey -> isContinue)
            )

          result.errors.map(error => error.key -> error.message) shouldBe Seq(
            AccessCodePage.key -> "error.pattern"
          )
        }
      }

      it should "accept a correctly formatted code" in {
        val result = AccessCodePage
          .form(method)
          .bind(
            Map(AccessCodePage.key -> validCode, AccessCodePage.isContinueKey -> isContinue)
          )

        result.errors shouldBe empty
        result.value shouldBe Some(AccessCodeForm(validCode, isContinue))
      }
    }
  }
}
