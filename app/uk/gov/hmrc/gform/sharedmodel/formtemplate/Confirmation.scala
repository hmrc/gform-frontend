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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.i18n.Messages
import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.{ Envelope, EnvelopeWithMapping }
import uk.gov.hmrc.gform.gform.handlers.FormHandlerResult
import uk.gov.hmrc.gform.validation.{ ComponentsValidatorHelper, FieldError, ValidationResult }

final case class Confirmation(
  question: FormComponent,
  pageId: PageId
) {
  def isRequiredError(envelope: Envelope, cache: AuthCacheWithForm)(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): FormHandlerResult = {

    val errors = ComponentsValidatorHelper.errors(question, "choice.error.required", None, "")

    FormHandlerResult(
      ValidationResult(
        Map(question.id -> FieldError(question, "", errors)),
        None
      ),
      EnvelopeWithMapping(envelope, cache.form)
    )
  }
}

object Confirmation {
  implicit val confirmationFormat: OFormat[Confirmation] = Json.format[Confirmation]
}
