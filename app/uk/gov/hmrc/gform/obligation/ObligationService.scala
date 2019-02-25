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

import uk.gov.hmrc.gform.auth.AuthService
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class ObligationService(gformConnector: GformConnector) {

  val stringToDate = new SimpleDateFormat("yyyy-MM-dd")

  def lookupObligationsMultiple(
    formTemplate: FormTemplate,
    authService: AuthService,
    retrievals: MaterialisedRetrievals,
    form: Form)(implicit hc: HeaderCarrier, ec: ExecutionContext) = {
    val hmrcTaxPeriodIdentifiers = formTemplate.expandFormTemplateFull.allFCs.collect {
      case IsHmrcTaxPeriod(el) => el
    }
    for {
      listOfIdNumbers <- Future.traverse(hmrcTaxPeriodIdentifiers)(
                          i =>
                            authService.evaluateSubmissionReference(
                              i.idNumber,
                              retrievals,
                              formTemplate,
                              FormDataHelpers.formDataMap(form.formData),
                              form.envelopeId))
      replaceIdNumber = listOfIdNumbers.zip(hmrcTaxPeriodIdentifiers)
      output <- gformConnector
                 .getAllTaxPeriods(replaceIdNumber.map(a =>
                   HmrcTaxPeriod(a._2.idType, TextExpression(Constant(a._1)), a._2.regimeType)))
                 .map(i =>
                   i.flatMap(j => j.obligation.obligations.flatMap(h => makeAllInfoList(j.id, h.obligationDetails))))
    } yield output
  }

  def makeAllInfoList(id: HmrcTaxPeriod, obligation: List[ObligationDetail]) =
    obligation.map(
      i =>
        TaxPeriodInformation(
          id.idType,
          id.idNumber,
          id.regimeType,
          i.inboundCorrespondenceFromDate,
          i.inboundCorrespondenceToDate,
          i.periodKey))

  def lookupIfPossible(
    form: Form,
    formTemplate: FormTemplate,
    authService: AuthService,
    retrievals: MaterialisedRetrievals)(implicit hc: HeaderCarrier, ec: ExecutionContext) = {
    val hmrcTaxPeriodIdentifiers = formTemplate.expandFormTemplateFull.allFCs.collect {
      case IsHmrcTaxPeriod(el) => el
    }
    for {
      string <- authService.evaluateSubmissionReference(
                 hmrcTaxPeriodIdentifiers.head.idNumber,
                 retrievals,
                 formTemplate,
                 FormDataHelpers.formDataMap(form.formData),
                 form.envelopeId)
      output <- form.obligations match {
                 case Some(x) => {
                   for {
                     idNumbers <- Future.traverse(x.listAllInfo)(
                                   i =>
                                     authService.evaluateSubmissionReference(
                                       i.idNumber,
                                       retrievals,
                                       formTemplate,
                                       FormDataHelpers.formDataMap(form.formData),
                                       form.envelopeId))
                     output <- {
                       if (idNumbers.forall(i => string.equals(i))) {
                         Future.successful(form)
                       } else {
                         val newObligations = lookupObligationsMultiple(formTemplate, authService, retrievals, form)
                         newObligations.map(i => form.copy(obligations = Some(ListOfTaxPeriodInformation(i))))
                       }
                     }
                   } yield output
                 }
                 case None => {
                   if (hmrcTaxPeriodIdentifiers.forall(i => !i.regimeType.value.isEmpty && !string.isEmpty)) {
                     val newObligations = lookupObligationsMultiple(formTemplate, authService, retrievals, form)
                     newObligations.map(i => form.copy(obligations = Some(ListOfTaxPeriodInformation(i))))
                   } else {
                     Future.successful(form)
                   }
                 }
               }
    } yield output
  }

  def updateObligations(formId: FormId, userData: UserData, form: Form, newForm: Form)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext) =
    if (form.obligations != newForm.obligations) {
      gformConnector.updateUserData(formId, userData)
    } else {
      Future.successful()
    }
}
