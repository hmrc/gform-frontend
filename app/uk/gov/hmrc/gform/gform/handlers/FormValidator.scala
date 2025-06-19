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

package uk.gov.hmrc.gform.gform.handlers

import cats.Monoid
import uk.gov.hmrc.gform.controllers.{ CacheData, Origin }
import uk.gov.hmrc.gform.eval.BooleanExprResolver
import uk.gov.hmrc.gform.models
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ CheckYourAnswers, EnteredVariadicFormData, FastForward, FormModel, PageMode, PageModel, ProcessData, Repeater, Visibility }
import uk.gov.hmrc.gform.models.gform.FormValidationOutcome
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber.Classic
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber.Classic.AddToListPage
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ GetEmailCodeFieldMatcher, ValidationResult }
import uk.gov.hmrc.gform.validation.ValidationUtil

import scala.annotation.tailrec
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

  private def sectionIsVisible(sectionNumber: SectionNumber, visibilityFormModel: FormModel[Visibility]) = {

    def pageIncludeIf(list: List[PageModel[Visibility]]) = list.flatMap(_.getIncludeIf)
    //    println("pageIncludeIf: " + pageIncludeIf)
    // println("page: " + page)
    def evalRes(pageIncludeIf: List[IncludeIf]) = pageIncludeIf.forall { includeIf =>
      //        println("onDemandIncludeIf: " + visibilityFormModel.onDemandIncludeIf)
      visibilityFormModel.onDemandIncludeIf.forall(f => f(includeIf))
    }

    val res = {
      sectionNumber match {
        case section @ AddToListPage.DefaultPage(sectionIndex) =>
          evalRes(
            pageIncludeIf(
              List(
                visibilityFormModel.pageModelLookup(AddToListPage.Page(sectionIndex, 1, 0)),
                visibilityFormModel.pageModelLookup(section)
              )
            )
          )
        case section @ Classic.RepeatedPage(sectionIndex, pageNumber) =>
          val repeats =
            visibilityFormModel.repeatingPageBrackets.find(_.hasSectionNumber(sectionNumber)).map(_.source.repeats)

          val res = repeats.flatMap { repeats =>
            val includeIf = IncludeIf(GreaterThan(repeats, Constant(pageNumber.toString)))
            visibilityFormModel.onDemandIncludeIf.map { f =>
              f(includeIf)
            }
          }
          res.getOrElse(true)
        case section => evalRes(pageIncludeIf(List(visibilityFormModel.pageModelLookup(section))))
      }
    }

    //      println("page: " + page)

    //      println("SectionNumber: " + sectionNumber)
    // println("Next section is visible: " + res)
    res
  }

  def toFormValidationOutcome(
    fhr: FormHandlerResult,
    enteredVariadicFormData: EnteredVariadicFormData
  ): FormValidationOutcome = {
    val FormHandlerResult(validationResult, _) = fhr
    validationResult.toFormValidationOutcome(enteredVariadicFormData)
  }

  def mustBeVisitedSectionNumber(
    processData: ProcessData,
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    validatePageModel: ValidatePageModel[Future, DataOrigin.Browser],
    maybeSectionNumber: Option[SectionNumber]
  ): Future[Option[SectionNumber]] = {

    val formModelOptics: FormModelOptics[DataOrigin.Browser] = processData.formModelOptics
    val availableSectionNumbers =
      getAvailableSectionNumbers(maybeSectionNumber, formModelOptics.formModelVisibilityOptics.formModel)
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
          page = formModelOptics.formModelVisibilityOptics.formModel(currentSn)
          hasBeenVisited = processData.visitsIndex.contains(currentSn)
          postcodeLookupHasAddress = page.postcodeLookup.fold(true) { formComponent =>
                                       cache.thirdPartyData.addressIsConfirmed(formComponent.id)
                                     }
        } yield acc match {
          case None =>
            if (
              hasBeenVisited &&
              postcodeLookupHasAddress &&
              isValid &&
              !formModelOptics.formModelVisibilityOptics.formModel.onDemandIncludeIf.exists { includeIfF =>
                page.isTerminationPage(
                  BooleanExprResolver(expr => includeIfF(IncludeIf(expr)))
                )
              }
            ) None
            else {
              if (sectionIsVisible(currentSn, formModelOptics.formModelVisibilityOptics.formModel)) {
                Some(currentSn)
              } else None
            }
          case otherwise =>
            val isCurrentSection = maybeSectionNumber.contains(currentSn)
            if (isCurrentSection && !isValid)
              maybeSectionNumber
            else
              otherwise
        }
      }
  }

  private def getAvailableSectionNumbers(
    currentSectionNumber: Option[SectionNumber],
    formModel: FormModel[_]
  ): List[SectionNumber] = {
    val maybeCoordinates = currentSectionNumber.flatMap(_.maybeCoordinates)

    val availableSectionNumbers: List[SectionNumber] =
      formModel.availableSectionNumbers

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
    val maybeCoordinates = maybeSectionNumber.flatMap(_.maybeCoordinates)
    val formModelOptics: FormModelOptics[DataOrigin.Browser] = processData.formModelOptics

    val visibilityFormModel = formModelOptics.formModelVisibilityOptics.formModel

    def atlHasSectionNumber(sectionNumber: SectionNumber): Boolean =
      formModelOptics.formModelVisibilityOptics.formModel.brackets.addToListBrackets
        .exists(_.hasSectionNumber(sectionNumber) && sectionIsVisible(sectionNumber, visibilityFormModel))

    val availableSectionNumbers =
      getAvailableSectionNumbers(maybeSectionNumber, formModelOptics.formModelVisibilityOptics.formModel)

    def findLastATLSectionNumber(sn: SectionNumber): SectionNumber = {
      val isAtlSection = atlHasSectionNumber(sn)
      if (isAtlSection) {
        availableSectionNumbers
          .filter(_ >= sn)
          .sliding(2)
          .toList
          .find {
            case List(a, b) => atlHasSectionNumber(a) && !atlHasSectionNumber(b)
            case List(a)    => atlHasSectionNumber(a)
            case _          => false
          }
          .flatMap(_.headOption)
          .getOrElse(availableSectionNumbers.lastOption.getOrElse(sn))
      } else sn
    }

    val ffYesSnF = mustBeVisitedSectionNumber(
      processData,
      cache,
      envelope,
      validatePageModel,
      maybeSectionNumber
    ).map(sn => sn.find(sn => sectionIsVisible(sn, visibilityFormModel)))
    // println("currentPage: " + maybeSectionNumber)

    val nextFrom = maybeSectionNumber.toList.flatMap { currentSectionNumber =>
      val sectionsAfterCurrent =
        availableSectionNumbers.filter(_ > currentSectionNumber)
      println("sections after current: " + sectionsAfterCurrent)
      sectionsAfterCurrent
    } collectFirst {
      case sectionNumber if sectionIsVisible(sectionNumber, visibilityFormModel) =>
        sectionNumber
    }

    /* val nextFrom = for {
      sectionNumber <- maybeSectionNumber
      next          <- availableSectionNumbers.find(_ > sectionNumber)
    } yield next*/

    println("nextFrom: " + nextFrom)

    println("ff: " + fastForward)

    ffYesSnF.map { ffYesSn =>
      println("ffYesSn: " + ffYesSn)
    }

    fastForward match {
      case FastForward.CYA(to) :: xs =>
        ffYesSnF.map(ffYes =>
          (ffYes, to, maybeSectionNumber) match {
            case (None, SectionOrSummary.FormSummary, _)    => SectionOrSummary.FormSummary
            case (None, SectionOrSummary.TaskSummary, _)    => SectionOrSummary.TaskSummary
            case (None, SectionOrSummary.Section(cyaTo), _) => SectionOrSummary.Section(cyaTo)
            case (Some(yesTo), _, Some(sn)) if yesTo <= sn  => SectionOrSummary.Section(yesTo)
            case (Some(yesTo), SectionOrSummary.FormSummary, _) =>
              nextFrom.map(SectionOrSummary.Section(_)).getOrElse(SectionOrSummary.FormSummary)
            case (Some(yesTo), SectionOrSummary.Section(cyaTo), _) if cyaTo > yesTo =>
              nextFrom.map(SectionOrSummary.Section(_)).getOrElse(SectionOrSummary.TaskSummary)
            case (Some(yesTo), SectionOrSummary.Section(cyaTo), _) => SectionOrSummary.Section(cyaTo)
            case (Some(yesTo), _, _)                               => SectionOrSummary.Section(yesTo)
            case (None, _, _)                                      => nextFrom.map(SectionOrSummary.Section(_)).getOrElse(SectionOrSummary.FormSummary)
          }
        )
      case FastForward.StopAt(to) :: xs =>
        ffYesSnF.map { ffYes =>
          val visibleTo =
            availableSectionNumbers.find(section => section >= to && sectionIsVisible(section, visibilityFormModel))
          (ffYes, visibleTo) match {
            case (None, None) if maybeCoordinates.isEmpty => SectionOrSummary.FormSummary
            case (None, None)                             => SectionOrSummary.TaskSummary
            case (None, Some(t))                          => SectionOrSummary.Section(t)
            case (Some(r), None)                          => SectionOrSummary.Section(r)
            case (Some(r), Some(t)) if r < t              => SectionOrSummary.Section(r)
            case (Some(r), Some(t))                       => SectionOrSummary.Section(t)
          }
        }
      case _ =>
        ffYesSnF.map { ffYesSn =>
          // println("ffYesSn: " + ffYesSn)
          (ffYesSn, nextFrom) match {
            case (None, None) =>
              if (maybeCoordinates.isEmpty) SectionOrSummary.FormSummary else SectionOrSummary.TaskSummary
            case (None, Some(sn)) =>
              // println("atlHasSectionNumber: " + atlHasSectionNumber(sn))
              if (atlHasSectionNumber(sn)) {
                SectionOrSummary.Section(sn)
              } else {
                if (maybeCoordinates.isEmpty) SectionOrSummary.FormSummary else SectionOrSummary.TaskSummary
              }
            case (Some(r), None) => SectionOrSummary.Section(r)
            case (Some(r), Some(sn)) =>
              val lsn = {
                if (fastForward == List(FastForward.Yes))
                  findLastATLSectionNumber(sn)
                else sn
              }
              val redirect =
                if (r < lsn) SectionOrSummary.Section(r)
                else SectionOrSummary.Section(lsn)
              println("redirect: " + redirect)
              redirect
          }
        }
    }
  }
}
