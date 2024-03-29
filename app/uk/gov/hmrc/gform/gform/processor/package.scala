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

package uk.gov.hmrc.gform.gform

import play.twirl.api.Html
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EnrolmentSection, FormTemplate }
import uk.gov.hmrc.gform.validation.ValidationResult
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.ErrorLink

package object processor {
  type RenderEnrolmentSection = (
    FormTemplate,
    MaterialisedRetrievals,
    EnrolmentSection,
    FormModelOptics[DataOrigin.Mongo],
    List[ErrorLink],
    ValidationResult
  ) => Html
}
