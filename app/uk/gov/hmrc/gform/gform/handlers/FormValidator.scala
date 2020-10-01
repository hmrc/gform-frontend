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

package uk.gov.hmrc.gform.gform.handlers

import cats.syntax.eq._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.{ CacheData, Origin }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.ExpandUtils.submittedFCs
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ FastForward, FormModel, ProcessData }
import uk.gov.hmrc.gform.models.gform.FormValidationOutcome
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormModelOptics, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ EmailCodeFieldMatcher, FormFieldValidationResult, GetEmailCodeFieldMatcher, ValidationResult }
import uk.gov.hmrc.gform.validation.ValidationUtil

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

class FormValidator(implicit ec: ExecutionContext) {

  // This is abstract in DataOrigin, since this is used in FastForward logic and when we render a form page with a GET.
  def validatePageModelBySectionNumber[D <: DataOrigin](
    formModelOptics: FormModelOptics[D],
    sectionNumber: SectionNumber,
    cache: CacheData,
    envelope: Envelope,
    validatePageModel: ValidatePageModel[Future, D]
  )(
    implicit hc: HeaderCarrier
  ): Future[FormHandlerResult] = {
    val formModel = formModelOptics.formModelRenderPageOptics.formModel
    val visibilityFormModel = formModelOptics.formModelVisibilityOptics.formModel
    val visibilityPageModel = visibilityFormModel(sectionNumber)
    val allFC: List[FormComponent] = formModel.allFormComponents

    for {
      v <- validatePageModel(
            visibilityPageModel,
            cache,
            envelope,
            formModelOptics.formModelVisibilityOptics,
            GetEmailCodeFieldMatcher(formModelOptics.formModelVisibilityOptics.formModel)
          )
    } yield {
      val validationResult: ValidationResult =
        ValidationUtil.evaluateValidationResult(
          visibilityPageModel.allFormComponents,
          v,
          formModelOptics.formModelVisibilityOptics,
          envelope)
      FormHandlerResult(validationResult, envelope)
    }

  }

  def toFormValidationOutcome(
    fhr: FormHandlerResult
  ): FormValidationOutcome = {
    val FormHandlerResult(validationResult, _) = fhr
    validationResult.toFormValidationOutcome
  }

  def fastForwardValidate(
    processData: ProcessData,
    cache: CacheData,
    envelope: Envelope,
    validatePageModel: ValidatePageModel[Future, DataOrigin.Browser],
    fastForward: FastForward
  )(
    implicit hc: HeaderCarrier
  ): Future[Option[SectionNumber]] = {

    val formModelOptics: FormModelOptics[DataOrigin.Browser] = processData.formModelOptics

    val availableSectionNumbers: List[SectionNumber] = Origin(formModelOptics).availableSectionNumbers
    availableSectionNumbers.foldLeft(Future.successful(None: Option[SectionNumber])) {
      case (accF, currentSn) =>
        accF.flatMap {
          case Some(sn) => Future.successful(Some(sn))
          case None =>
            validatePageModelBySectionNumber(
              formModelOptics,
              currentSn,
              cache,
              envelope,
              validatePageModel
            ).map(toFormValidationOutcome).map {
              case FormValidationOutcome(isValid, _, _) =>
                val page = formModelOptics.formModelRenderPageOptics.formModel(currentSn)
                val hasBeenVisited = processData.visitsIndex.contains(currentSn.value)

                val stop = page.isTerminationPage || !hasBeenVisited

                if (isValid && !stop && fastForward.goOn(currentSn)) None else Some(currentSn)
            }
        }
    }
  }
}
