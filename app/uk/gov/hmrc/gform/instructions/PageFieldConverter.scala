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

package uk.gov.hmrc.gform.instructions

import play.api.i18n.Messages
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.instructions.FormModelSummaryConverter.PageField
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ ComponentType, FormComponent, SectionNumber }
import uk.gov.hmrc.gform.validation.ValidationResult

trait PageFieldConverter[T <: ComponentType] {
  def convert(
    fieldValue: FormComponent,
    cache: AuthCacheWithForm,
    sectionNumber: SectionNumber,
    validationResult: ValidationResult,
    envelope: Envelope)(implicit lise: SmartStringEvaluator, messages: Messages, l: LangADT): PageField
}

object PageFieldConverter {
  def apply[T <: ComponentType](implicit c: PageFieldConverter[T]) = c
}
