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

  def maybeGetInvalidSectionNumber(
    processData: ProcessData,
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    validatePageModel: ValidatePageModel[Future, DataOrigin.Browser],
    maybeSectionNumber: Option[SectionNumber]
  ): Future[Option[SectionNumber]] = {

    val formModelOptics: FormModelOptics[DataOrigin.Browser] = processData.formModelOptics
    val availableSectionNumbers = getAvailableSectionNumbers(maybeSectionNumber, formModelOptics)
    def isValidSectionNumberF(sn: SectionNumber): Future[Boolean] =
      validatePageModelBySectionNumber(
        formModelOptics,
        sn,
        cache,
        envelope,
        validatePageModel
      ).map(fhr => toFormValidationOutcome(fhr, EnteredVariadicFormData.empty).isValid)

    availableSectionNumbers
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
              hasBeenVisited &&
              postcodeLookupHasAddress &&
              isValid &&
              !page.isTerminationPage
            ) None
            else Some(currentSn)
          case otherwise => otherwise
        })
      }
  }

  private def getAvailableSectionNumbers(
    currentSectionNumber: Option[SectionNumber],
    formModelOptics: FormModelOptics[DataOrigin.Browser]
  ): List[SectionNumber] = {
    val maybeCoordinates = currentSectionNumber.flatMap(_.toCoordinates)

    val availableSectionNumbers: List[SectionNumber] = Origin(
      formModelOptics.formModelVisibilityOptics.formModel
    ).availableSectionNumbers

    maybeCoordinates.fold(availableSectionNumbers)(coordinates =>
      availableSectionNumbers.filter(_.contains(coordinates))
    )
  }

  def fastForwardValidate(
    processData: ProcessData,
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    validatePageModel: ValidatePageModel[Future, DataOrigin.Browser],
    fastForward: List[FastForward],
    maybeSectionNumber: Option[SectionNumber]
  ): Future[SectionOrSummary] = {

    lxol.pp.log((fastForward, maybeSectionNumber), "[777777AAA]")
    val maybeCoordinates = maybeSectionNumber.flatMap(_.toCoordinates)
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = processData.formModelOptics

    val availableSectionNumbers = getAvailableSectionNumbers(maybeSectionNumber, formModelOptics)

    val ffYesSnF = maybeGetInvalidSectionNumber(
      processData,
      cache,
      envelope,
      validatePageModel,
      maybeSectionNumber
    )

    lazy val nextFrom = for {
      sectionNumber <- maybeSectionNumber
      next          <- availableSectionNumbers.find(_ > sectionNumber)
    } yield next

    val r = fastForward match {
      case FastForward.CYA(to) :: xs =>
        ffYesSnF.map(ffYes =>
          (ffYes, to, maybeSectionNumber) match {
            case (None, SectionOrSummary.FormSummary, _)    => SectionOrSummary.FormSummary
            case (None, SectionOrSummary.TaskSummary, _)    => SectionOrSummary.TaskSummary
            case (None, SectionOrSummary.Section(cyaTo), _) => SectionOrSummary.Section(cyaTo)
            case (Some(yesTo), _, Some(sn)) if yesTo == sn  => SectionOrSummary.Section(yesTo)
            case (Some(yesTo), SectionOrSummary.FormSummary, _) =>
              nextFrom.map(SectionOrSummary.Section(_)).getOrElse(SectionOrSummary.FormSummary)
            case (Some(yesTo), SectionOrSummary.Section(cyaTo), _) if cyaTo > yesTo =>
              nextFrom.map(SectionOrSummary.Section(_)).getOrElse(SectionOrSummary.TaskSummary)
            case (Some(yesTo), SectionOrSummary.Section(cyaTo), _) => SectionOrSummary.Section(cyaTo)
          }
        )
      case FastForward.StopAt(to) :: xs =>
        ffYesSnF.map {
          case None =>
            if (availableSectionNumbers.contains(to)) {
              SectionOrSummary.Section(to)
            } else if (maybeCoordinates.isEmpty) SectionOrSummary.FormSummary
            else SectionOrSummary.TaskSummary
          case Some(r) => if (r < to) SectionOrSummary.Section(r) else SectionOrSummary.Section(to)
        }
      case _ =>
        ffYesSnF.map { ffYesSn =>
          (ffYesSn, nextFrom) match {
            case (None, None) =>
              if (maybeCoordinates.isEmpty) SectionOrSummary.FormSummary else SectionOrSummary.TaskSummary
            case (None, Some(sn)) =>
              SectionOrSummary.Section(sn)
              if (maybeCoordinates.isEmpty) SectionOrSummary.FormSummary else SectionOrSummary.TaskSummary
            case (Some(r), None)     => SectionOrSummary.Section(r)
            case (Some(r), Some(sn)) => if (r < sn) SectionOrSummary.Section(r) else SectionOrSummary.Section(sn)
          }
        }
    }
    lxol.pp.log(r, "RESULT")
    r.map { a =>
      lxol.pp.log(a, "RESULT")
      a
    }
  }
}
