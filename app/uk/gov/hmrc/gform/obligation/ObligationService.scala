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
import cats.kernel.Eq
import cats.implicits._
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

  private def lookupObligationsMultiple(
    formTemplate: FormTemplate,
    authService: AuthService,
    retrievals: MaterialisedRetrievals,
    form: Form,
    hmrcTaxPeriodIdentifiers: NonEmptyList[HmrcTaxPeriod])(implicit hc: HeaderCarrier, ec: ExecutionContext) =
    for {
      listOfIdNumbers <- {
        Future.traverse(hmrcTaxPeriodIdentifiers.toList)(i => {
          val j = authService.evaluateSubmissionReference(
            i.idNumber,
            retrievals,
            formTemplate,
            FormDataHelpers.formDataMap(form.formData),
            form.envelopeId)
          j.map(jj => HmrcTaxPeriodWithEvaluatedId(i, IdNumberValue(jj)))
        })
      }
      output <- gformConnector
                 .getAllTaxPeriods(listOfIdNumbers)
                 .map(
                   i =>
                     i.flatMap(
                       j =>
                         j.obligation.obligations.flatMap(
                           h =>
                             makeAllInfoList(
                               j.id,
                               listOfIdNumbers.find(x => x.hmrcTaxPeriod == j.id).get,
                               h.obligationDetails))))
    } yield output

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
        val newObligations =
          lookupObligationsMultiple(formTemplate, authService, retrievals, form, hmrcTaxPeriodIdentifiersNonEmpty)
        newObligations.map(i => form.copy(obligations = RetrievedObligations(i)))
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
                           ifStatement(!currentIdNumber.forall(i => idNumbers.contains(i)))
                         }
                       } yield output
                     }
                     case NotChecked => {
                       val a = currentIdNumber
                       val b = a
                       ifStatement(currentIdNumber.exists(i => !i.isEmpty))
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
