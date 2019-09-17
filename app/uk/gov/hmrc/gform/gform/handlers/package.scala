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

package uk.gov.hmrc.gform.gform

import play.twirl.api.Html
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormTemplate, Section, SectionNumber }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, Obligations, VariadicFormData }
import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

package object handlers {

  type HtmlGenerator = (
    Option[AccessCode],
    Form,
    SectionNumber,
    FormDataRecalculated,
    FormTemplate,
    List[(FormComponent, FormFieldValidationResult)],
    Envelope,
    EnvelopeId,
    ValidatedType[ValidationResult],
    List[Section],
    Int,
    List[ContentType],
    MaterialisedRetrievals,
    VisitIndex,
    Option[String],
    Obligations) => Html

  type UpdateObligations[F[_]] = (FormId, UserData, Form, Form) => F[Unit]

  type RecalculateDataAndSections[F[_]] =
    (VariadicFormData, AuthCacheWithForm) => F[(FormDataRecalculated, List[Section])]

  type ValidateForm[F[_]] = (
    FormDataRecalculated,
    List[Section],
    SectionNumber,
    EnvelopeId,
    MaterialisedRetrievals,
    ThirdPartyData,
    FormTemplate) => F[(List[(FormComponent, FormFieldValidationResult)], ValidatedType[ValidationResult], Envelope)]

  type ValidateFormComponents[F[_]] = (
    List[FormComponent],
    Section,
    EnvelopeId,
    MaterialisedRetrievals,
    ThirdPartyData,
    FormTemplate,
    FormDataRecalculated) => F[ValidatedType[ValidationResult]]

  type EvaluateValidation =
    (
      ValidatedType[ValidationResult],
      List[FormComponent],
      FormDataRecalculated,
      Envelope) => List[(FormComponent, FormFieldValidationResult)]

}
