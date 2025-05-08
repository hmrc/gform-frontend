/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.data.Forms.{ mapping, nonEmptyText, single }
import play.api.data.validation.Constraints
import play.api.data.{ Form, Mapping }
import uk.gov.hmrc.gform.gform.AccessCodeForm
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BySubmissionReference, DraftRetrievalMethod }

object AccessCodePage {

  val key = "accessCode"
  val optionKey = "accessOption"
  val isContinueKey = "isContinue"
  val optionNew = "new"
  val optionContinue = "continue"
  val optionDownload = "downloadPrevious"

  def decision: Form[String] =
    Form(
      single(
        optionKey -> nonEmptyText
      )
    )

  def form(draftRetrievalMethod: DraftRetrievalMethod): Form[AccessCodeForm] = {
    val format: Int = draftRetrievalMethod match {
      case BySubmissionReference => accessCodeSubmissionRef
      case _                     => accessCodeAgent
    }
    val verification = verificationFor(format)

    Form(
      mapping(
        key           -> verification,
        isContinueKey -> nonEmptyText
      )(AccessCodeForm.apply)(AccessCodeForm.unapply)
    )
  }

  private def verificationFor(i: Int): Mapping[String] =
    nonEmptyText.verifying(
      Constraints.pattern(("^[A-Z0-9]{" + i + "}-[A-Z0-9]{4}-[A-Z0-9]{" + i + "}$").r)
    )

  private val accessCodeAgent = 3
  private val accessCodeSubmissionRef = 4

}
