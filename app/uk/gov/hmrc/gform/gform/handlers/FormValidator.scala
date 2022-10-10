/*
 * Copyright 2022 HM Revenue & Customs
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

import cats.Monoid
import uk.gov.hmrc.gform.controllers.{ CacheData, Origin }
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.Coordinates
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ EnteredVariadicFormData, FastForward, ProcessData }
import uk.gov.hmrc.gform.models.gform.FormValidationOutcome
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ GetEmailCodeFieldMatcher, ValidationResult }
import uk.gov.hmrc.gform.validation.ValidationUtil

import scala.concurrent.{ ExecutionContext, Future }

class FormValidator(implicit ec: ExecutionContext) {

  def validatePageModelBySectionNumbers[D <: DataOrigin](
    formModelOptics: FormModelOptics[D],
    sectionNumbers: List[SectionNumber],
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    validatePageModel: ValidatePageModel[Future, D]
  ): Future[FormHandlerResult] =
    Future
      .traverse(sectionNumbers) { sectionNumber =>
        validatePageModelBySectionNumber(
          formModelOptics,
          sectionNumber,
          cache,
          envelope,
          validatePageModel
        ).map(_.validationResult)
      }
      .map(list => FormHandlerResult(Monoid[ValidationResult].combineAll(list), envelope))

  // This is abstract in DataOrigin, since this is used in FastForward logic and when we render a form page with a GET.
  def validatePageModelBySectionNumber[D <: DataOrigin](
    formModelOptics: FormModelOptics[D],
    sectionNumber: SectionNumber,
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    validatePageModel: ValidatePageModel[Future, D]
  ): Future[FormHandlerResult] = {
    val visibilityFormModel = formModelOptics.formModelVisibilityOptics.formModel
    val visibilityPageModel = visibilityFormModel(sectionNumber)

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
          envelope
        )
      FormHandlerResult(validationResult, envelope)
    }

  }

  def toFormValidationOutcome(
    fhr: FormHandlerResult,
    enteredVariadicFormData: EnteredVariadicFormData
  ): FormValidationOutcome = {
    val FormHandlerResult(validationResult, _) = fhr
    validationResult.toFormValidationOutcome(enteredVariadicFormData)
  }

  def fastForwardValidate(
    processData: ProcessData,
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    validatePageModel: ValidatePageModel[Future, DataOrigin.Browser],
    fastForward: FastForward,
    maybeCoordinates: Option[Coordinates]
  ): Future[Option[SectionNumber]] = {

    val formModelOptics: FormModelOptics[DataOrigin.Browser] = processData.formModelOptics

    val availableSectionNumbers0: List[SectionNumber] = Origin(formModelOptics).availableSectionNumbers

    val availableSectionNumbers = maybeCoordinates.fold(availableSectionNumbers0)(coordinates =>
      availableSectionNumbers0.filter(_.contains(coordinates))
    )
    def isValidSectionNumberF(sn: SectionNumber): Future[Boolean] =
      validatePageModelBySectionNumber(
        formModelOptics,
        sn,
        cache,
        envelope,
        validatePageModel
      ).map(fhr => toFormValidationOutcome(fhr, EnteredVariadicFormData.empty).isValid)

    val ffYesSnF = availableSectionNumbers
      .foldLeft(Future.successful(None: Option[SectionNumber])) { case (accF, currentSn) =>
        for {
          acc     <- accF
          isValid <- isValidSectionNumberF(currentSn)
          page = formModelOptics.formModelRenderPageOptics.formModel(currentSn)
          hasBeenVisited = processData.visitsIndex.contains(currentSn)
          postcodeLookupHasAddress = page.postcodeLookup.fold(true) { formComponent =>
                                       cache.thirdPartyData.addressIsConfirmed(formComponent.id)
                                     }
        } yield (acc match {
          case None =>
            if (
              fastForward.goOn(currentSn) &&
              hasBeenVisited &&
              postcodeLookupHasAddress &&
              isValid &&
              !page.isTerminationPage
            ) None
            else Some(currentSn)
          case otherwise => otherwise
        })
      }

    fastForward match {
      case FastForward.CYA(from, to) =>
        lazy val nextFrom = availableSectionNumbers.find(_ > from)
        ffYesSnF.map(ffYes =>
          (ffYes, to) match {
            case (None, None)                                => None
            case (None, Some(cyaTo))                         => Some(cyaTo)
            case (Some(_), None)                             => nextFrom
            case (Some(yesTo), Some(cyaTo)) if cyaTo > yesTo => nextFrom
            case (Some(yesTo), Some(cyaTo))                  => Some(cyaTo)
          }
        )
      case _ =>
        ffYesSnF
    }
  }
}
