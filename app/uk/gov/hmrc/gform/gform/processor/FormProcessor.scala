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

package uk.gov.hmrc.gform.gform.processor

import cats.instances.future._
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.eq._
import play.api.i18n.I18nSupport
import play.api.mvc.Results.Redirect
import play.api.mvc.{ AnyContent, Request, Result }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.FileIdsWithMapping
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.{ EnvelopeWithMapping, FileUploadAlgebra }
import uk.gov.hmrc.gform.gform.handlers.FormControllerRequestHandler
import uk.gov.hmrc.gform.gform.{ FastForwardService, routes }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.models.gform.{ FormValidationOutcome, NoSpecificAction }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.DataOrigin.Mongo
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.sharedmodel.form.{ FormComponentIdToFileIdMapping, FormModelOptics, ThirdPartyData, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, SectionNumber, SectionTitle4Ga, SuppressErrors }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class FormProcessor(
  i18nSupport: I18nSupport,
  processDataService: ProcessDataService[Future],
  gformConnector: GformConnector,
  validationService: ValidationService,
  fastForwardService: FastForwardService,
  recalculation: Recalculation[Future, Throwable],
  fileUploadService: FileUploadAlgebra[Future],
  handler: FormControllerRequestHandler
)(implicit ec: ExecutionContext) {

  import i18nSupport._

  def processRemoveAddToList(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    ff: FastForward,
    formModelOptics: FormModelOptics[Mongo],
    processData: ProcessData,
    idx: Int,
    addToListId: AddToListId
  )(implicit
    request: Request[AnyContent],
    hc: HeaderCarrier,
    lang: LangADT,
    sse: SmartStringEvaluator
  ): Future[Result] = {

    def saveAndRedirect(
      updFormModelOptics: FormModelOptics[DataOrigin.Browser],
      componentIdToFileId: FormComponentIdToFileIdMapping
    ): Future[Result] = {
      val updFormModel: FormModel[DataExpanded] = updFormModelOptics.formModelRenderPageOptics.formModel

      val sn = updFormModel.brackets.addToListBracket(addToListId).lastSectionNumber

      val visitsIndex = VisitIndex
        .updateSectionVisits(updFormModel, processData.formModel, processData.visitsIndex)

      val processDataUpd = processData.copy(
        formModelOptics = updFormModelOptics,
        visitsIndex = VisitIndex(visitsIndex)
      )

      val cacheUpd = cache.copy(
        form = cache.form.copy(visitsIndex = VisitIndex(visitsIndex), componentIdToFileId = componentIdToFileId)
      )

      validateAndUpdateData(cacheUpd, processDataUpd, sn, sn, maybeAccessCode, ff, formModelOptics) {
        maybeSectionNumber =>
          val addToListBracket: Bracket.AddToList[DataExpanded] =
            formModelOptics.formModelRenderPageOptics.formModel.brackets.addToListBracket(addToListId)

          val sectionNumber =
            if (addToListBracket.iterations.size === 1)
              maybeSectionNumber
                .map(addToListBracket.iterationForSectionNumber(_).firstSectionNumber)
                .getOrElse(sn)
            else
              sn

          val sectionTitle4Ga = getSectionTitle4Ga(processDataUpd, sectionNumber)

          Redirect(
            routes.FormController
              .form(
                cache.formTemplate._id,
                maybeAccessCode,
                sectionNumber,
                sectionTitle4Ga,
                SuppressErrors.Yes,
                FastForward.Yes
              )
          )
      }
    }

    val formModel = formModelOptics.formModelRenderPageOptics.formModel
    val bracket = formModel.brackets.addToListBracket(addToListId)
    val (updData, componentIdToFileIdMapping, filesToDelete) =
      AddToListUtils.removeRecord(
        processData,
        bracket,
        idx,
        FileIdsWithMapping(formModel.allFileIds, cache.form.componentIdToFileId)
      )

    for {
      updFormModelOptics <- FormModelOptics
                              .mkFormModelOptics[DataOrigin.Browser, Future, SectionSelectorType.Normal](
                                updData.asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]],
                                cache,
                                recalculation
                              )
      redirect <- saveAndRedirect(updFormModelOptics, componentIdToFileIdMapping)
      _        <- fileUploadService.deleteFiles(cache.form.envelopeId, filesToDelete)
    } yield redirect
  }

  def validateAndUpdateData(
    cache: AuthCacheWithForm,
    processData: ProcessData,
    sectionNumber: SectionNumber,
    validationSectionNumber: SectionNumber,
    maybeAccessCode: Option[AccessCode],
    fastForward: FastForward,
    formModelOptics: FormModelOptics[Mongo]
  )(
    toResult: Option[SectionNumber] => Result
  )(implicit hc: HeaderCarrier, request: Request[AnyContent], l: LangADT, sse: SmartStringEvaluator): Future[Result] =
    for {
      envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)
      envelopeWithMapping = EnvelopeWithMapping(envelope, cache.form)
      FormValidationOutcome(_, formData, validatorsResult) <- handler.handleFormValidation(
                                                                processData.formModelOptics,
                                                                validationSectionNumber,
                                                                cache.toCacheData,
                                                                envelopeWithMapping,
                                                                validationService.validatePageModel
                                                              )
      res <- {

        val oldData: VariadicFormData[SourceOrigin.Current] = processData.formModelOptics.pageOpticsData

        val formDataU = oldData.toFormData ++ formData
        val before: ThirdPartyData = cache.form.thirdPartyData
        val after: ThirdPartyData = before.updateFrom(validatorsResult)

        val needsSecondPhaseRecalculation =
          (before.desRegistrationResponse, after.desRegistrationResponse).mapN(_ =!= _)

        val visitsIndex = processData.visitsIndex.visit(sectionNumber)

        val cacheUpd =
          cache.copy(
            form = cache.form
              .copy(
                thirdPartyData = after.copy(obligations = processData.obligations),
                formData = formDataU,
                visitsIndex = visitsIndex
              )
          )

        if (needsSecondPhaseRecalculation.getOrElse(false)) {
          val newDataRaw = cache.variadicFormData[SectionSelectorType.Normal]
          for {
            newProcessData <- processDataService
                                .getProcessData[SectionSelectorType.Normal](
                                  newDataRaw,
                                  cacheUpd,
                                  formModelOptics,
                                  gformConnector.getAllTaxPeriods,
                                  NoSpecificAction
                                )
            result <- validateAndUpdateData(
                        cacheUpd,
                        newProcessData,
                        sectionNumber,
                        validationSectionNumber,
                        maybeAccessCode,
                        fastForward,
                        formModelOptics
                      )(toResult) // recursive call
          } yield result
        } else {
          fastForwardService
            .updateUserData(
              cacheUpd,
              processData.copy(visitsIndex = visitsIndex),
              maybeAccessCode,
              fastForward,
              envelopeWithMapping
            )(toResult)
        }
      }
    } yield res

  def getSectionTitle4Ga(processData: ProcessData, sectionNumber: SectionNumber): SectionTitle4Ga =
    sectionTitle4GaFactory(processData.formModel(sectionNumber).title, sectionNumber)
}
