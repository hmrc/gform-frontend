/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.gform

import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.{ PageModel, Visibility }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.form.ValidatorsResult
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.validation.{ GetEmailCodeFieldMatcher, ValidationResult }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

package object handlers {

  type ValidatePageModel[F[_], D <: DataOrigin] = (
    PageModel[Visibility],
    CacheData,
    Envelope,
    FormModelVisibilityOptics[D],
    GetEmailCodeFieldMatcher
  ) => F[ValidatedType[ValidatorsResult]]

}
