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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.{ Obligations, RetrievedObligations, TaxResponse }

trait ObligationValidator extends TaxSelectionNavigator {

  def validateWithDes(
    formDataRecalculated: FormDataRecalculated,
    cachedObligation: Obligations,
    desObligation: Obligations,
    clearTaxResponses: FormDataRecalculated => FormDataRecalculated): FormDataRecalculated =
    (cachedObligation, desObligation) match {
      case (RetrievedObligations(obligation), RetrievedObligations(responseObligation))
          if mayClear(formDataRecalculated, obligation, responseObligation) =>
        clearTaxResponses(formDataRecalculated)
      case _ => formDataRecalculated
    }

  private def mayClear(
    formDataRecalculated: FormDataRecalculated,
    cachedObligation: NonEmptyList[TaxResponse],
    responseObligation: NonEmptyList[TaxResponse]): Boolean =
    cachedObligation.toList
      .zip(responseObligation.toList)
      .map {
        case (cached, taxResponse) => (cached.obligation, taxResponse)
      }
      .map { case (obligation, taxResponse) => taxSelectionNavigator(formDataRecalculated, obligation, taxResponse) }
      .count(_ == GoBackToTaxPeriodSelection) > 0
}
