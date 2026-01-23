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

import play.api.i18n.Messages
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.IncludeIfValue
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DestinationIncludeIf, DestinationWithCustomerCaseflow, DestinationWithCustomerId, DestinationWithNiRefundClaimBankDetails, DestinationWithPaymentReference, DestinationWithPegaCaseId, DestinationWithTaxpayerId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.{ DestinationEvaluation, DestinationResult }

object DestinationEvaluator {

  def apply[D <: DataOrigin](
    formTemplate: FormTemplate,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit messages: Messages): DestinationEvaluation = {

    def evalIncludeIf(includeIf: DestinationIncludeIf) = includeIf match {
      case IncludeIfValue(includeIf) => Some(formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None))
      case _                         => None
    }

    val evaluations = formTemplate.destinations match {
      case dl: DestinationList =>
        dl.destinations.collect {
          case d: DestinationWithCustomerCaseflow =>
            val includeIfEval = evalIncludeIf(d.includeIf)
            val customerId =
              formModelVisibilityOptics.evalAndApplyTypeInfoFirst(d.customerId()).stringRepresentation.take(32)

            val postalCode =
              d.postalCode().map(formModelVisibilityOptics.evalAndApplyTypeInfoFirst(_).stringRepresentation)

            val caseId = d.caseId().map(formModelVisibilityOptics.evalAndApplyTypeInfoFirst(_).stringRepresentation)

            DestinationResult(
              d.id,
              includeIfEval,
              Some(customerId),
              None,
              None,
              None,
              None,
              postalCode,
              caseId,
              None,
              None,
              None,
              None,
              None
            )
          case d: DestinationWithCustomerId =>
            val includeIfEval = evalIncludeIf(d.includeIf)
            val customerId =
              formModelVisibilityOptics.evalAndApplyTypeInfoFirst(d.customerId()).stringRepresentation.take(32)

            DestinationResult(
              d.id,
              includeIfEval,
              Some(customerId),
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None
            )
          case d: DestinationWithTaxpayerId =>
            val includeIfEval = evalIncludeIf(d.includeIf)
            val taxpayerId =
              formModelVisibilityOptics.evalAndApplyTypeInfoFirst(d.taxpayerId()).stringRepresentation.take(32)

            DestinationResult(
              d.id,
              includeIfEval,
              None,
              Some(taxpayerId),
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None
            )
          case d: DestinationWithPaymentReference =>
            val includeIfEval = evalIncludeIf(d.includeIf)
            val paymentReference =
              formModelVisibilityOptics.evalAndApplyTypeInfoFirst(d.paymentReference).stringRepresentation
            val nino =
              d.nino.map(formModelVisibilityOptics.evalAndApplyTypeInfoFirst(_).stringRepresentation)
            val utr =
              d.utr.map(formModelVisibilityOptics.evalAndApplyTypeInfoFirst(_).stringRepresentation)
            val postalCode =
              d.postalCode.map(formModelVisibilityOptics.evalAndApplyTypeInfoFirst(_).stringRepresentation)

            DestinationResult(
              d.id,
              includeIfEval,
              None,
              None,
              Some(paymentReference),
              nino,
              utr,
              postalCode,
              None,
              None,
              None,
              None,
              None,
              None
            )
          case d: DestinationWithPegaCaseId =>
            val includeIfEval = evalIncludeIf(d.includeIf)
            val caseId = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(d.caseId).stringRepresentation
            DestinationResult(
              d.id,
              includeIfEval,
              None,
              None,
              None,
              None,
              None,
              None,
              Some(caseId),
              None,
              None,
              None,
              None,
              None
            )
          case d: DestinationWithNiRefundClaimBankDetails =>
            val includeIfEval = evalIncludeIf(d.includeIf)
            val bankAccountName =
              formModelVisibilityOptics.evalAndApplyTypeInfoFirst(d.bankAccountName).stringRepresentation
            val sortCode = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(d.sortCode).stringRepresentation
            val accountNumber =
              formModelVisibilityOptics.evalAndApplyTypeInfoFirst(d.accountNumber).stringRepresentation
            val rollNumber =
              d.rollNumber.map(formModelVisibilityOptics.evalAndApplyTypeInfoFirst(_).stringRepresentation)
            val refundClaimReference =
              formModelVisibilityOptics.evalAndApplyTypeInfoFirst(d.refundClaimReference).stringRepresentation
            val nino = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(d.nino).stringRepresentation
            DestinationResult(
              d.id,
              includeIfEval,
              None,
              None,
              None,
              Some(nino),
              None,
              None,
              None,
              Some(bankAccountName),
              Some(sortCode),
              Some(accountNumber),
              rollNumber,
              Some(refundClaimReference)
            )
          case others =>
            val includeIfEval = evalIncludeIf(others.includeIf)
            DestinationResult(
              others.id,
              includeIfEval,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None
            )
        }
      case _ => List.empty[DestinationResult]
    }

    DestinationEvaluation(evaluations)
  }

}
