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

package uk.gov.hmrc.gform.obligation
import java.text.SimpleDateFormat

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.auth.AuthService
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier
import cats.implicits._
import scala.concurrent.{ ExecutionContext, Future }

class ObligationService(gformConnector: GformConnector) {

  val stringToDate = new SimpleDateFormat("yyyy-MM-dd")

  private def lookupObligationsMultipleD(
    formTemplate: FormTemplate,
    authService: AuthService,
    retrievals: MaterialisedRetrievals,
    form: Form,
    hmrcTaxPeriodIdentifiers: NonEmptyList[HmrcTaxPeriod],
    condition: Boolean)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Form] =
    if (condition) {
      for {
        idNumbers <- hmrcTaxPeriodIdentifiers.nonEmptyTraverse(i =>
                      withEvaluatedIdB(formTemplate, authService, retrievals, form, i))
        taxResponses <- gformConnector.getAllTaxPeriods(idNumbers)
        obligations = obligationsB(idNumbers, taxResponses)
      } yield form.copy(obligations = RetrievedObligations(obligations))
    } else
      Future.successful(form)

  private def lookupObligationsMultipleB(
    formTemplate: FormTemplate,
    authService: AuthService,
    retrievals: MaterialisedRetrievals,
    form: Form,
    hmrcTaxPeriodIdentifiers: NonEmptyList[HmrcTaxPeriod])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[List[TaxPeriodInformation]] =
    for {
      idNumbers <- hmrcTaxPeriodIdentifiers.nonEmptyTraverse(i =>
                    withEvaluatedIdB(formTemplate, authService, retrievals, form, i))
      taxResponses <- gformConnector.getAllTaxPeriods(idNumbers)
    } yield obligationsB(idNumbers, taxResponses)

  private def lookupObligationsMultipleC(
    formTemplate: FormTemplate,
    authService: AuthService,
    retrievals: MaterialisedRetrievals,
    form: Form,
    hmrcTaxPeriodIdentifiers: NonEmptyList[HmrcTaxPeriod])(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[Form] =
    for {
      idNumbers <- hmrcTaxPeriodIdentifiers.nonEmptyTraverse(i =>
                    withEvaluatedIdB(formTemplate, authService, retrievals, form, i))
      taxResponses <- gformConnector.getAllTaxPeriods(idNumbers)
      obligations = obligationsB(idNumbers, taxResponses)
    } yield form.copy(obligations = RetrievedObligations(obligations))

  private def withEvaluatedIdB(
    formTemplate: FormTemplate,
    authService: AuthService,
    retrievals: MaterialisedRetrievals,
    form: Form,
    hmrcTaxPeriod: HmrcTaxPeriod)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[HmrcTaxPeriodWithEvaluatedId] =
    for {
      evaluated <- authService.evaluateSubmissionReference(
                    hmrcTaxPeriod.idNumber,
                    retrievals,
                    formTemplate,
                    FormDataHelpers.formDataMap(form.formData),
                    form.envelopeId)
    } yield HmrcTaxPeriodWithEvaluatedId(hmrcTaxPeriod, IdNumberValue(evaluated))

  private def obligationsB(
    idNumbers: NonEmptyList[HmrcTaxPeriodWithEvaluatedId],
    taxResponses: List[TaxResponse]): List[TaxPeriodInformation] =
    taxResponses.flatMap(j =>
      j.obligation.obligations.flatMap(h =>
        makeAllInfoList(j.id, idNumbers.find(x => x.hmrcTaxPeriod == j.id).get, h.obligationDetails)))

  def makeAllInfoList(
    id: HmrcTaxPeriod,
    evaluatedId: HmrcTaxPeriodWithEvaluatedId,
    obligation: List[ObligationDetail]) =
    obligation.map(
      i =>
        TaxPeriodInformation(
          id,
          evaluatedId.idNumberValue,
          i.inboundCorrespondenceFromDate,
          i.inboundCorrespondenceToDate,
          i.periodKey))

  def lookupIfPossible(
    form: Form,
    formTemplate: FormTemplate,
    authService: AuthService,
    retrievals: MaterialisedRetrievals)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Form] = {

    val hmrcTaxPeriodIdentifiers = formTemplate.expandFormTemplateFull.allFCs.collect {
      case IsHmrcTaxPeriod(el) => el
    }
    val hmrcTaxPeriodIdentifiersNonEmpty: NonEmptyList[HmrcTaxPeriod] =
      NonEmptyList.fromList(hmrcTaxPeriodIdentifiers) match {
        case Some(x) => x
        case _ =>
          NonEmptyList(HmrcTaxPeriod(IdType("NONE"), TextExpression(Constant("NONE")), RegimeType("NONE")), List())
      }

    def ifStatement(condition: Boolean) =
      if (condition) {
        lookupObligationsMultipleD(formTemplate, authService, retrievals, form, hmrcTaxPeriodIdentifiersNonEmpty, true)
      } else {
        Future.successful(form)
      }

    hmrcTaxPeriodIdentifiers match {
      case a :: _ =>
        for {
          currentIdNumber <- Future.traverse(hmrcTaxPeriodIdentifiers)(
                              i =>
                                authService.evaluateSubmissionReference(
                                  i.idNumber,
                                  retrievals,
                                  formTemplate,
                                  FormDataHelpers.formDataMap(form.formData),
                                  form.envelopeId
                              ))
          output <- form.obligations match {
                     case RetrievedObligations(x) => {
                       for {
                         idNumbers <- Future(x.map(i => i.idNumberValue.value))
                         output <- {
                           val a = currentIdNumber
                           val b = idNumbers
                           val c = a
                           lookupObligationsMultipleD(
                             formTemplate,
                             authService,
                             retrievals,
                             form,
                             hmrcTaxPeriodIdentifiersNonEmpty,
                             !currentIdNumber.forall(i => idNumbers.contains(i)))
                         }
                       } yield output
                     }
                     case NotChecked => {
                       val a = currentIdNumber
                       val b = a
                       lookupObligationsMultipleD(
                         formTemplate,
                         authService,
                         retrievals,
                         form,
                         hmrcTaxPeriodIdentifiersNonEmpty,
                         currentIdNumber.exists(i => !i.isEmpty))
                     }
                   }
        } yield output
      case _ => Future.successful(form)
    }
  }

  def updateObligations(formId: FormId, userData: UserData, form: Form, newForm: Form)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext) =
    if (form.obligations != newForm.obligations) {
      gformConnector.updateUserData(formId, userData)
    } else {
      Future.successful(())
    }
}
